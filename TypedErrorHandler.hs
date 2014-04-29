{-# LANGUAGE DeriveDataTypeable,
  ExistentialQuantification, 
  MultiParamTypeClasses,
  FunctionalDependencies,
  TypeSynonymInstances,
  FlexibleInstances,
  GeneralizedNewtypeDeriving #-}


module TypedErrorHandler 
    (IsSignal,
     signal,
     registerHandler,
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

type SigT r m = EH.SignalingCodeT (m Signal) (m Value) r m

newtype SigTS s r m a = SigTS (EH.SignalingCodeT (Signal, s) (Value, s) r m a)
    deriving (Monad, MonadTrans, MonadCont)

class Monad m => Sig m where
    signal :: IsSignal signal value => signal -> m value
    registerHandler :: IsSignal signal value => (signal -> m (SignalResult value c)) -> m c -> m c

instance Sig (SigT r m) where
    signal s = (EH.signal (Signal s)) >>= unValue
        where
          unValue (Value a) = unsafeCoerce a

    registerHandler errorHandler normalCode = EH.registerHandler errorHandler' normalCode
        where
          errorHandler' (Signal a) = case cast a of
                                       Nothing -> return NotHandled
                                       Just signal -> errorHandler signal >>= return . convertSignal
          convertSignal NotHandled = NotHandled
          convertSignal (Continue a) = Continue (Value a)
          convertSignal (Abort c) = Abort c



--instance Sig m => Sig (StateT s m) where
--    signal = lift . signal
--    registerHandler handler normal = 
--        State $ \oldstate -> do
--          registerHandler
--            (\signal -> runStateT (handler signal) oldstate
--        do
