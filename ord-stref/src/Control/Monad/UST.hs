{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Control.Monad.UST
  ( ST, runST
  , fromBase, newUnique
  , fixST, stToIO
  ) where

import Control.Monad.Trans.UGenT
import qualified Control.Monad.ST as Base

import Control.Monad.Trans.Class (lift)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Fail (MonadFail)
import Control.Applicative (liftA2)

newtype ST s a = U { unU :: UGenT (Base.ST s) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadFail)
instance Semigroup a => Semigroup (ST s a) where (<>)   = liftA2 (<>)
instance Monoid    a => Monoid    (ST s a) where mempty = pure mempty

runST :: (forall s. ST s a) -> a
runST s = Base.runST (runUGenT (unU s))

-- | Manually wrap @ST@ from @base@.
fromBase :: Base.ST s a -> ST s a
fromBase = U . lift

toBase :: ST s a -> Base.ST s a
toBase = runUGenT . unU

-- | Generate a new symbol, unique within the thread.
--   prop> runST $ (/=) <$> newUnique <*> newUnique
--
--   Does not violate purity:
--   prop> runST newUnique == runST newUnique
newUnique :: ST s Unique
newUnique = U genUnique

fixST :: (a -> ST s a) -> ST s a
fixST = mfix

stToIO :: ST Base.RealWorld a -> IO a
stToIO = Base.stToIO . toBase
