{-# LANGUAGE DeriveFunctor, 
             MultiParamTypeClasses, 
             ExistentialQuantification, 
             FunctionalDependencies,
             DeriveDataTypeable,
             FlexibleInstances,
             UndecidableInstances #-}
module ErrorHandling 
    (SignalResult(..),
     SignalingCodeT,
     SignalingCode,
     signal,
     registerHandler,
     runCodeT,
     runCode,
     mapSignalReturnType,
     mapSignal,
     catchAllToString,
     dontCatch,
     finally) where

import Control.Monad.Cont
import Control.Monad.Identity
import Data.Typeable
import Unsafe.Coerce
import Control.Monad.State.Class
import Control.Applicative


data SignalResult a c = NotHandled | Continue a | Abort c

data SignalKind r m s = Normal s [ ContT r m () ]
 deriving Functor

-- code which can signal messages 
-- and react accordingly

-- parameters are:
--  s  type of Signals
--  q  type of return value from signal handlers
--  r  last return type from the Cont Monad
--  a  monadic return type, also from Cont Monad
data SignalingCodeT s q r m a = SignalingCodeT {runSignalingCodeT :: (SignalKind r m s -> ContT r m q) -> ContT r m a}

instance Monad (SignalingCodeT s q r m) where
    return = SignalingCodeT . const . return
    SignalingCodeT a >>= f = SignalingCodeT $ \sig -> do
                                   value <- a sig
                                   runSignalingCodeT (f value) sig

instance MonadCont (SignalingCodeT s q r m) where
    callCC fun = SignalingCodeT $ \ topSigHandler ->
                 callCC $ \ handler ->
                     runSignalingCodeT (fun $ \value ->
                                            SignalingCodeT $ const (handler value))
                        topSigHandler

instance MonadTrans (SignalingCodeT s q r) where
    lift = SignalingCodeT . const . lift

instance Functor (SignalingCodeT s q r m) where
    fmap f (SignalingCodeT a) = SignalingCodeT $ \ sig ->
                                fmap f (a sig)

instance Applicative (SignalingCodeT s q r m) where
    pure = return
    (SignalingCodeT f) <*> (SignalingCodeT a) = 
        SignalingCodeT $ \sig -> f sig <*> a sig
    

instance MonadState st m => MonadState st (SignalingCodeT s q r m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadIO m => MonadIO (SignalingCodeT s q r m) where
    liftIO = lift . liftIO


type SignalingCode s q r a = SignalingCodeT s q r Identity a

-- signal a signal
signal :: s -> SignalingCodeT s q r m q
signal s = SignalingCodeT $ \sig -> sig (Normal s [])


watch :: ((SignalKind r m s -> ContT r m q) -> SignalKind r m s -> ContT r m q) -> 
         SignalingCodeT s q r m c -> SignalingCodeT s q r m c
watch watcher (SignalingCodeT normalCode) = SignalingCodeT $ \ topSigHandler ->
    callCC $ \ handler ->
        normalCode $ \ sig ->
            watcher topSigHandler sig

-- register a handler
registerHandler :: (s -> SignalingCodeT s q r m (SignalResult q c)) -> SignalingCodeT s q r m c -> SignalingCodeT s q r m c
registerHandler errorHandler (SignalingCodeT normalCode) = SignalingCodeT $ \ topSigHandler ->
    callCC $ \ handler -> 
      normalCode $ \sig' -> case sig' of 
        Normal sig abh -> do
          handlerResult <- runSignalingCodeT (errorHandler sig) topSigHandler
          case handlerResult of
            NotHandled -> topSigHandler sig'
            Continue value -> return value
            Abort c -> sequence_ (reverse abh) >> handler c
--        _ -> topSigHandler sig'  


finally :: SignalingCodeT s q r m () -> SignalingCodeT s q r m c -> SignalingCodeT s q r m c
finally always = (watch $ \ topSigHandler sig' ->
   case sig' of
     Normal sig abh -> 
         topSigHandler (Normal sig (abh ++ [runSignalingCodeT always topSigHandler])))
   >=> (always >>) . return

-- run the continuation
runCodeT :: Monad m => SignalingCodeT s q a m a -> m a
runCodeT (SignalingCodeT f) = flip runContT return $ f $ const (return undefined)

runCode :: SignalingCode s q a a -> a
runCode = runIdentity . runCodeT

-- map the signal handler return type (which flows up)
mapSignalReturnType :: (q' -> SignalingCodeT s q' r m q) -> 
                    SignalingCodeT s q r m a -> SignalingCodeT s q' r m a
mapSignalReturnType q'ToQ (SignalingCodeT code) = 
    SignalingCodeT $ \ sigHandler ->
        code $ \sig -> do
          v1 <- sigHandler sig
          runSignalingCodeT (q'ToQ v1) sigHandler

-- map the signal type (which flows down)
mapSignal :: (s -> s') -> SignalingCodeT s q r m a -> SignalingCodeT s' q r m a
mapSignal sToS' (SignalingCodeT code) =
    SignalingCodeT $ \ sigHandler ->
        code $ \sig -> do
          v1 <- sigHandler (fmap sToS' sig)
          runSignalingCodeT (return v1) sigHandler

-- catch everything into a string
catchAllToString :: Show s => SignalingCodeT s q r m a -> SignalingCodeT () () r m (Either String a)
catchAllToString = mapSignal (const ()) . mapSignalReturnType (return undefined) .
                   registerHandler (return . Abort . Left . show) . (return . Right =<<)

dontCatch :: q -> SignalingCodeT s q r m a -> SignalingCodeT s q r m a
dontCatch q = registerHandler (const $ return $ Continue q)
