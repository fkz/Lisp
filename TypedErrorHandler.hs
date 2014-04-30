{-# LANGUAGE DeriveDataTypeable,
  ExistentialQuantification, 
  MultiParamTypeClasses,
  FunctionalDependencies,
  TypeSynonymInstances,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  NoMonomorphismRestriction #-}


module TypedErrorHandler 
    (SigT,
     IsSignal,
     signal,
     registerHandler,
     registerHandlerT,
     EH.finally,
     SignalResult(..),
     EH.runCodeT,
     EH.runCode) where


import qualified ErrorHandling as EH
import ErrorHandling (SignalResult(..))
import Data.Typeable
import Unsafe.Coerce
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad.Cont


class (Typeable signal) => IsSignal signal value | signal -> value where


data Signal = forall a b . IsSignal a b => Signal a
data Value  = forall a . Value a

type SigT r m = EH.SignalingCodeT Signal (m Value) r m


class Monad m => Sig m where
    signal :: IsSignal signal value => signal -> m value
    registerHandler :: IsSignal signal value => (signal -> m (SignalResult value c)) -> m c -> m c


instance Monad m => Sig (SigT r m) where
    signal s = 
        do
          mValue <- (EH.signal (Signal s))
          value <- lift mValue
          return $ unValue value
        where
          unValue (Value a) = unsafeCoerce a


    registerHandler errorHandler normalCode = EH.registerHandler errorHandler' normalCode
        where
          errorHandler' (Signal a) = case cast a of
                                       Nothing -> return NotHandled
                                       Just signal -> errorHandler signal >>= return . convertSignal
          convertSignal NotHandled = NotHandled
          convertSignal (Continue a) = Continue (return (Value a))
          convertSignal (Abort c) = Abort c

registerHandlerT :: (Sig m, IsSignal signal value) => signal -> (signal -> m (SignalResult value c)) -> m c -> m c
registerHandlerT = const registerHandler


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
