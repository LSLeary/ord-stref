{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.UGenT
  ( UGenT, runUGenT
  , UGen, runUGen
  , Unique, genUnique
  ) where

import Control.Monad.State.Strict
import Control.Monad.Fail (MonadFail)
import Control.Applicative (Alternative)
import Data.Functor.Identity (Identity(runIdentity))

-- | Transform a Monad with unique symbol generation.
newtype UGenT m a = UGenT (StateT Unique m a)
  deriving
    ( Functor, Applicative, Alternative
    , Monad, MonadTrans, MonadFix, MonadFail, MonadIO
    )

-- | Escape the 'UGenT' Monad transformer, discarding generator state.
runUGenT :: Functor m => UGenT m a -> m a
runUGenT (UGenT (StateT f)) = fst <$> f (Unique 0)

type UGen = UGenT Identity

-- | Perform a computation in the USG Monad @UGen@.
runUGen :: UGen a -> a
runUGen = runIdentity . runUGenT

-- | An opaque data type, the complete interface to which consists of @Eq@,
--   @Ord@ and @genUnique@.
newtype Unique = Unique Integer
  deriving (Eq, Ord)

-- | Generate a new symbol, unique within the current monadic computation.
--
--   prop> runUGen $ (/=) <$> genUnique <*> genUnique
--
--   The symbol generation is pure:
--
--   prop> runUGen genUnique == runUGen genUnique
genUnique :: Monad m => UGenT m Unique
genUnique = UGenT (modify' bump *> get)
  where bump (Unique i) = Unique (i + 1)
