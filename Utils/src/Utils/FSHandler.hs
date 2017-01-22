{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Utils.FSHandler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Servant

type FSHandler a = ReaderT a (ExceptT ServantErr IO)

readerToHandler :: a -> FSHandler a :~> Handler
readerToHandler inf = Nat $ readerToHandler' inf

readerToHandler' :: a -> forall b. (FSHandler a) b -> Handler b
readerToHandler' inf r = runReaderT r inf
