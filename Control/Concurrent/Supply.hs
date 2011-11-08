{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- | A globally unique fresh variable supply with manually managed local pools.
module Control.Concurrent.Supply
  ( Supply
  , newSupply
  , freshId
  , splitSupply
  , freshId#
  , splitSupply#
  ) where

import Data.Hashable
import Data.IORef
import Data.Functor
import Data.Monoid
import GHC.IO (unsafeDupablePerformIO)

infixr 5 :-
data Stream a = a :- Stream a

instance Functor Stream where
  fmap f (a :- as) = f a :- fmap f as

extract :: Stream a -> a
extract (a :- _) = a

units :: Stream ()
units = () :- units

data Block = Block Int !(Stream Block)

instance Eq Block where
  Block a (Block b _ :- _) == Block c (Block d _ :- _) = a == c && b == d

instance Ord Block where
  Block a (Block b _ :- _) `compare` Block c (Block d _ :- _) = compare a c `mappend` compare b d

instance Show Block where
  showsPrec d (Block a (Block b _ :- _)) = showParen (d >= 10) $
    showString "Block " . showsPrec 10 a . showString " (Block " . showsPrec 10 b . showString " ... :- ...)"

instance Hashable Block where
  hashWithSalt s (Block a (Block b _ :- _)) = s `hashWithSalt` a `hashWithSalt` b

blockSize :: Int
blockSize = 1024
{-# INLINE blockSize #-}

-- Minimum size to be worth splitting a supply rather than just CAS'ing twice to avoid multiple subsequent biased splits
minSplitSupplySize :: Int
minSplitSupplySize = 32 -- based on sqrt blockSize
{-# INLINE minSplitSupplySize #-}

blockCounter :: IORef Int
blockCounter = unsafeDupablePerformIO $ newIORef 0

gen :: () -> Block
gen _ = Block (unsafeDupablePerformIO $ atomicModifyIORef blockCounter $ \ i -> let i' = i + blockSize in (i', i))
              (gen <$> units)
{-# NOINLINE gen #-}

newBlock :: IO Block
newBlock = return $ gen ()
{-# NOINLINE newBlock #-}

splitBlock# :: Block -> (# Block, Block #)
splitBlock# (Block i (x :- xs)) = (# x, Block i xs #)
{-# INLINE splitBlock# #-}

-- | A user managed globally unique variable supply.
data Supply = Supply {-# UNPACK #-} !Int {-# UNPACK #-} !Int Block
  deriving (Eq,Ord,Show)

instance Hashable Supply where
  hashWithSalt s (Supply i j b) = s `hashWithSalt` i `hashWithSalt` j `hashWithSalt` b

blockSupply :: Block -> Supply
blockSupply (Block i bs) = Supply i (i + blockSize - 1) (extract bs)

-- Grab a new supply, all supplies generate disjoint sets of variables
-- when used in a linear fashion, but you can reuse a supply to obtain
-- the same results during replay.
newSupply :: IO Supply
newSupply = blockSupply <$> newBlock

freshId :: Supply -> (Int, Supply)
freshId s = case freshId# s of
  (# i, s' #) -> (i, s')

splitSupply :: Supply -> (Supply, Supply)
splitSupply s = case splitSupply# s of
  (# l, r #) -> (l, r)

-- | Obtain a globally unique variable from the supply.
freshId# :: Supply -> (# Int, Supply #)
freshId# (Supply i j b)
  | i /= j = (# i, Supply (i + 1) j b #)
  | otherwise = case b of
    Block k b' -> (# i, Supply k (k + blockSize - 1) (extract b') #)

-- | Split a supply into two supplies that provide disjoint ids
splitSupply# :: Supply -> (# Supply, Supply #)
splitSupply# (Supply i k b) = case splitBlock# b of
    (# bl, br #)
      | k - i >= minSplitSupplySize
      , j <- i + div (k - i) 2 ->
        (# Supply i j bl, Supply (j + 1) k br #)
      | Block x (l :- r :- _) <- bl
      , y <- x + div blockSize 2
      , z <- x + blockSize - 1 ->
        (# Supply x (y - 1) l, Supply y z r #)
