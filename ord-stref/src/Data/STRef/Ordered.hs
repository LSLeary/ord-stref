-----------------------------------------------------------------------------
-- |
-- Module    : Data.STRef.Ordered
-- Copyright : (c) 2019-2023 L.S.Leary
-- License   : BSD3
--
-- A drop-in replacement for "Data.STRef", extending its interface with 'Ord'.
-- Uses 'ST' from "Control.Monad.ST.USG".
--
-----------------------------------------------------------------------------

module Data.STRef.Ordered (

  -- * STRefs
  STRef, newSTRef,
  readSTRef, writeSTRef,
  modifySTRef, modifySTRef'

) where

-- base
import Data.Function (on)
import qualified Data.STRef as Base

-- ord-stref
import Control.Monad.ST.USG (ST, Unique, newUnique, fromBase)


-- | Equivalent to 'Base.STRef', but with 'Ord' and a small performance penalty.
data STRef s a = STRef
  { refID :: {-# UNPACK #-} !(Unique s)
  , ref   :: {-# UNPACK #-} !(Base.STRef s a)
  }

instance Eq  (STRef s a) where (==)    = (==)    `on` refID
instance Ord (STRef s a) where compare = compare `on` refID

-- | See 'Base.newSTRef'.
newSTRef :: a -> ST s (STRef s a)
newSTRef a = STRef <$> newUnique <*> fromBase (Base.newSTRef a)

-- | See 'Base.readSTRef'.
readSTRef :: STRef s a -> ST s a
readSTRef = fromBase . Base.readSTRef . ref

-- | See 'Base.writeSTRef'.
writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef r = fromBase . Base.writeSTRef (ref r)

-- | See 'Base.modifySTRef'.
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef r = fromBase . Base.modifySTRef (ref r)

-- | See 'Base.modifySTRef''.
modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' r = fromBase . Base.modifySTRef' (ref r)

