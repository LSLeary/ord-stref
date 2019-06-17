{-# LANGUAGE GeneralisedNewtypeDeriving, RankNTypes #-}

module Control.Monad.UST
  ( ST, runST
  , fromBase, newUnique
  , fixST, stToIO
  ) where

import Control.Monad.Trans.UGenT
import qualified Control.Monad.ST as S

import Control.Monad.Trans.Class (lift)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Fail (MonadFail)
import Control.Applicative (liftA2)

newtype ST s a = U { unU :: UGenT (S.ST s) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadFail)
instance Semigroup a => Semigroup (ST s a) where (<>)   = liftA2 (<>)
instance Monoid    a => Monoid    (ST s a) where mempty = pure mempty

runST :: (forall s. ST s a) -> a
runST s = S.runST (runUGenT (unU s))

fromBase :: S.ST s a -> ST s a
fromBase = U . lift

toBase :: ST s a -> S.ST s a
toBase = runUGenT . unU

newUnique :: ST s Unique
newUnique = U genUnique

fixST :: (a -> ST s a) -> ST s a
fixST = mfix

stToIO :: ST S.RealWorld a -> IO a
stToIO = S.stToIO . toBase
