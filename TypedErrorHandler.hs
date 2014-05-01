{-# LANGUAGE DeriveDataTypeable,
  ExistentialQuantification, 
  MultiParamTypeClasses,
  FunctionalDependencies,
  TypeSynonymInstances,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  NoMonomorphismRestriction,
  UndecidableInstances #-}


module TypedErrorHandler 
    (SigT,
     IsSignal,
     signal,
     registerHandler,
     registerHandlerT,
     finally,
     SignalResult(..),
     runCodeT,
     runCode,
     handleIf) where

import qualified ErrorHandling as EH
import ErrorHandling (SignalResult(..))
import Data.Typeable
import Unsafe.Coerce
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.Identity


class (Typeable signal) => IsSignal signal value | signal -> value where


data Signal = forall a b . IsSignal a b => Signal a
data Value  = forall a . Value a

newtype SigT r m a = SigT {runSigT :: EH.SignalingCodeT Signal (m Value) r m a}
 deriving (Monad, MonadCont, Functor, Applicative, MonadIO)

instance MonadTrans (SigT r) where
    lift = SigT . lift

instance MonadState st m => MonadState st (SigT r m) where
    get = lift get
    put = lift . put
    state = lift . state


class Monad m => Sig m where
    signal :: IsSignal signal value => signal -> m value
    registerHandler :: IsSignal signal value => (signal -> m (SignalResult value c)) -> m c -> m c
    finally :: m () -> m a -> m a

instance Monad m => Sig (SigT r m) where
    signal s = 
        do
          mValue <- SigT (EH.signal (Signal s))
          value <- lift mValue
          return $ unValue value
        where
          unValue (Value a) = unsafeCoerce a


    registerHandler errorHandler normalCode = SigT $ EH.registerHandler errorHandler'' (runSigT normalCode)
        where
          errorHandler'' = runSigT . errorHandler'
          errorHandler' (Signal a) = case cast a of
                                       Nothing -> return NotHandled
                                       Just signal -> errorHandler signal >>= return . convertSignal
          convertSignal NotHandled = NotHandled
          convertSignal (Continue a) = Continue (return (Value a))
          convertSignal (Abort c) = Abort c

     
    finally err norm = SigT $ EH.finally (runSigT err) (runSigT norm)


registerHandlerT :: (Sig m, IsSignal signal value) => signal -> (signal -> m (SignalResult value c)) -> m c -> m c
registerHandlerT = const registerHandler

handleIf :: (Sig m, IsSignal signal value) => (signal -> m (Maybe a)) -> (a -> m (SignalResult value c)) -> m c -> m c
handleIf g catch norm = registerHandler (handler <=< g) norm
    where 
      handler Nothing = return NotHandled
      handler (Just q) = catch q


data SampleError =
  SampleError0
 |NoRealError
 |Success
   deriving (Typeable, Show)

instance IsSignal SampleError String


testCode :: Monad m => SigT r m String
testCode = registerHandler 
              (\ e -> return $ Continue $ show (e :: SampleError)) 
            (signal SampleError0 >> return "Bestanden")

runCodeT :: Monad m => SigT a m a -> m a
runCodeT = EH.runCodeT . runSigT

runCode :: SigT a Identity a -> a
runCode = EH.runCode . runSigT
