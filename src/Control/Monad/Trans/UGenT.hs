{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Control.Monad.Trans.UGenT
  ( Unique
  , UGenT, runUGenT, genUnique
  ) where

import Control.Monad.State.Strict
import Control.Monad.Fail (MonadFail)
import Control.Applicative (Alternative)

newtype Unique = Unique Integer
  deriving (Eq, Ord)

newtype UGenT m a = UGenT (StateT Unique m a)
  deriving
    ( Functor, Applicative, Alternative
    , Monad, MonadTrans, MonadFix, MonadFail, MonadIO
    )

runUGenT :: Functor m => UGenT m a -> m a
runUGenT (UGenT (StateT f)) = fst <$> f (Unique 0)

genUnique :: Monad m => UGenT m Unique
genUnique = UGenT (modify bump *> get)
  where bump (Unique i) = Unique (i + 1)
