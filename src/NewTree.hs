{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts #-}

module NewTree where

import qualified Data.Tree as T
import qualified Data.ByteString as B
import Data.Word
import Data.Serialize
import Data.Int
import GHC.Generics

type BinTree = T.Tree (Maybe B.ByteString)

empty :: BinTree
empty = T.Node Nothing []

singleton :: B.ByteString -> BinTree
singleton b = T.Node (Just b) []

serialize :: (Serialize a) => a -> BinTree
serialize a = singleton (encode a)

deserialize :: (Serialize a) => BinTree -> Either String a
deserialize (T.Node (Just b) []) = decode b
deserialize (T.Node Nothing _)   = Left "Invalid encoding. Expecting Just and found Nothing."
deserialize _                    = Left "Invalid encoding. Expected no children."

class BTree a where
  toBTree :: a -> BinTree
  fromBTree :: BinTree -> Either String a

  default toBTree :: (Generic a, BTree' (Rep a)) => a -> BinTree
  toBTree x = toBTree' (from x)

  default fromBTree :: (Generic a, BTree' (Rep a)) => BinTree -> Either String a
  fromBTree b = do
    b' <- fromBTree' b
    return (to b')

instance BTree () where
  toBTree () = empty
  fromBTree (T.Node Nothing []) = Right ()
  fromBTree _                   = Left "Invalid () encoding."

instance BTree Bool where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Char where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Double where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Float where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Int where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Int8 where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Int16 where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Int32 where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Int64 where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Integer where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Ordering where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Word where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Word8 where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Word16 where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Word32 where
  toBTree = serialize
  fromBTree = deserialize

instance BTree Word64 where
  toBTree = serialize
  fromBTree = deserialize

instance BTree B.ByteString where
  toBTree = serialize
  fromBTree = deserialize

class BTree' f where
  toBTree' :: f p -> BinTree
  fromBTree' :: BinTree -> Either String (f p)

instance BTree' U1 where
  toBTree' U1 = empty
  fromBTree' (T.Node Nothing []) = Right U1
  fromBTree' _ = Left "Invalid U1 encoding."

instance (BTree' f, BTree' g) => BTree' (f :+: g) where
  toBTree' (L1 x) = T.Node (Just $ encode False) [toBTree' x]
  toBTree' (R1 x) = T.Node (Just $ encode True) [toBTree' x]
  fromBTree' (T.Node (Just b) [x]) = do
    b' <- decode b
    if b' then do
      x' <- fromBTree' x
      return (R1 x')
    else do
      x' <- fromBTree' x
      return (L1 x')
  fromBTree' _ = Left "Invalid :+: encoding."

instance (BTree' f, BTree' g) => BTree' (f :*: g) where
  toBTree' (x :*: y) = T.Node Nothing [toBTree' x, toBTree' y]
  fromBTree' (T.Node Nothing [x,y]) = do
    x' <- fromBTree' x
    y' <- fromBTree' y
    return (x' :*: y')
  fromBTree' _ = Left "Invalid :*: encoding."

instance (BTree c) => BTree' (K1 i c) where
  toBTree' (K1 x) = toBTree x
  fromBTree' x = do
     x' <- fromBTree x
     return (K1 x')

instance (BTree' f) => BTree' (M1 i t f) where
  toBTree' (M1 x) = toBTree' x
  fromBTree' x = do
    x' <- fromBTree' x
    return (M1 x')
