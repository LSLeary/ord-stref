{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE RoleAnnotations, RankNTypes, BlockArguments #-}
{-# LANGUAGE PolyKinds, DataKinds, UnboxedTuples #-} -- for old GHC

-----------------------------------------------------------------------------
-- |
-- Module    : Control.Monad.ST.USG
-- Copyright : (c) 2019-2023 L.S.Leary
-- License   : BSD3
--
-- A drop-in replacement for "Control.Monad.ST", extending its interface with
-- unique symbol generation via 'newUnique'.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.USG (

  -- * The ST Monad
  ST, runST, fixST,

  -- * Unique Symbol Generation
  Unique, newUnique,

  -- * Converting ST to and from Base.ST
  fromBase, withUnliftBaseST,

  -- * Converting ST to IO
  Base.RealWorld, stToIO

) where

-- base
import Data.Monoid (Ap(..))
import Data.Functor (($>))
import Control.Monad.Fix (MonadFix(..))
import qualified Control.Monad.ST as Base
import qualified Data.STRef       as Base

-- transformers
import Control.Monad.Trans.Reader (ReaderT(..), ask)

-- primitive
import Control.Monad.Primitive (PrimMonad)


-- | See 'Base.ST'.
newtype ST s a = ST{ unST :: ReaderT (Base.STRef s Int) (Base.ST s) a }
  deriving (Functor, Applicative, Monad, MonadFix, PrimMonad)
  deriving (Semigroup, Monoid) via Ap (ST s) a

-- | See 'Base.runST'.
runST :: (forall s. ST s a) -> a
runST s = Base.runST (toBase s)

-- | See 'Base.fixST'.
fixST :: (a -> ST s a) -> ST s a
fixST = mfix


-- | An abstract data type with an interface comprised entirely by 'newUnique',
--   'Eq' and 'Ord'. Symbols may not escape the scope of the state thread.
newtype Unique s = Unique Int
  deriving (Eq, Ord)

type role Unique nominal

-- | Generate a 'Unique' symbol.
--
--   prop> runST $ (/=) <$> newUnique <*> newUnique
--
newUnique :: ST s (Unique s)
newUnique = do
  ref <- ST ask
  fromBase do
    int <- Base.readSTRef ref
    Base.writeSTRef ref (int + 1) $> Unique int


-- | Convert 'Base.ST' (from @base@) to 'ST'.
fromBase :: Base.ST s a -> ST s a
fromBase bst = withUnliftBaseST \_ -> bst

-- | Unlift 'Base.ST' (from @base@) à la @MonadUnliftIO@.
--   Allows 'Base.ST'-specific control operators to be lifted to 'ST'.
withUnliftBaseST :: ((forall x. ST s x -> Base.ST s x) -> Base.ST s a) -> ST s a
withUnliftBaseST k = (ST . ReaderT) \ref -> k (flip runReaderT ref . unST)


-- | See 'Base.stToIO'.
stToIO :: ST Base.RealWorld a -> IO a
stToIO = Base.stToIO . toBase


-- Internal: allows 'Unique's to escape the scope of their uniqueness.
--
-- prop> runST $ (==) <$> toBase newUnique <*> toBase newUnique
--
toBase :: ST s a -> Base.ST s a
toBase s = runReaderT (unST s) =<< Base.newSTRef minBound

