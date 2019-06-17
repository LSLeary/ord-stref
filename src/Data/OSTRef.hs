
module Data.OSTRef
  ( STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , modifySTRef
  , modifySTRef'
  ) where

import qualified Data.STRef as S
import Control.Monad.Trans.UGenT (Unique)
import Control.Monad.UST (ST, newUnique, fromBase)
import Data.Ord (comparing)

data STRef s a = O { refID :: Unique, ref :: S.STRef s a }
  deriving Eq

instance Ord (STRef s a) where
  compare = comparing refID

newSTRef :: a -> ST s (STRef s a)
newSTRef a = O <$> newUnique <*> fromBase (S.newSTRef a)

readSTRef :: STRef s a -> ST s a
readSTRef = fromBase . S.readSTRef . ref

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef r = fromBase . S.writeSTRef (ref r)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef r = fromBase . S.modifySTRef (ref r)

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' r = fromBase . S.modifySTRef' (ref r)
