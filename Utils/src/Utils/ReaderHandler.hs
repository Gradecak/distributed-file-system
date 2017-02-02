{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Utils.ReaderHandler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Servant

type ReaderHandler a = ReaderT a (ExceptT ServantErr IO)

readerToHandler :: a -> ReaderHandler a :~> Handler
readerToHandler inf = Nat $ readerToHandler' inf

readerToHandler' :: a -> forall b. (ReaderHandler a) b -> Handler b
readerToHandler' inf r = runReaderT r inf
