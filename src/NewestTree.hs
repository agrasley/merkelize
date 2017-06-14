{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts #-}

module NewestTree where

import qualified Data.ByteString as B
import GHC.Generics
import qualified Data.Serialize as S
import Data.Bits (shift)
import Crypto.Hash




data Tree a alg = Hash (Digest alg) | Empty | Leaf a | Node a (Tree a alg) | Two (Tree a alg) (Tree a alg)

instance Show a => Show (Tree a alg) where
  show = pretty 0

spaces :: Int -> String
spaces i = replicate i ' '

pretty :: (Show a) => Int -> Tree a alg -> String
pretty i Empty       = spaces i ++ "Empty"
pretty i (Leaf a)    = spaces i ++ "Leaf " ++ show a
pretty i (Node a t)  = spaces i ++ "Node " ++ show a ++ "\n" ++ pretty (i+4) t
pretty i (Two t1 t2) = pretty (i+3) t1 ++ "\n" ++ spaces i ++ "Two\n" ++ pretty (i+3) t2
pretty i (Hash d)    = spaces i ++ "Hash " ++ take 10 (show d) ++ "..."




type BTree alg = Tree B.ByteString alg

getDigest :: (HashAlgorithm alg) => BTree alg -> Digest alg
getDigest Empty       = hashFinalize hashInit
getDigest (Leaf a)    = hash a
getDigest (Node a t)  = hashFinalize $ hashUpdate (hashUpdate hashInit (getDigest t)) a
getDigest (Two t1 t2) = hashFinalize $ hashUpdates hashInit [getDigest t1, getDigest t2]
getDigest (Hash d)    = d

validate :: (HashAlgorithm alg) => BTree alg -> BTree alg -> Bool
validate a b = getDigest a == getDigest b

data Path = None | One Path | L Path | R Path | Both Path Path
  deriving Show

fromPath :: (HashAlgorithm alg) => Path -> BTree alg -> BTree alg
fromPath None         t          = Hash $ getDigest t
fromPath (One None)   (Hash d)   = Hash d
fromPath (One None)   Empty      = Empty
fromPath (One None)   (Leaf a)   = Leaf a
fromPath (One p)      (Node a t) = Node a (fromPath p t)
fromPath (L p)        (Two l r)  = Two (fromPath p l) (fromPath None r)
fromPath (R p)        (Two l r)  = Two (fromPath None l) (fromPath p r)
fromPath (Both pl pr) (Two l r)  = Two (fromPath pl l) (fromPath pr r)
fromPath _            _          = error "Invalid path encoding for this tree."




class ToTree a where
  toTree :: a -> BTree alg

  default toTree :: (Generic a, GToTree (Rep a)) => a -> BTree alg
  toTree = gToTree . from


serialize :: (S.Serialize a) => a -> BTree alg
serialize = Leaf . S.encode

instance ToTree () where
  toTree () = Empty

instance ToTree Bool where
  toTree = serialize

instance ToTree Char where
  toTree = serialize

instance ToTree Double where
  toTree = serialize

instance ToTree Float where
  toTree = serialize

instance ToTree Int where
  toTree = serialize

instance ToTree Integer where
  toTree = serialize




type Size = Int
type Count = Int

class GToTree f where
  gToTree :: f p -> BTree alg
  gToTree' :: Count -> Size -> f p -> BTree alg


instance GToTree U1 where
  gToTree U1 = Empty
  gToTree' c s U1 = serialize (c,s)

instance GToTree a => GToTree (M1 i c a) where
  gToTree = gToTree . unM1
  gToTree' c s = gToTree' c s . unM1

instance (GToTree f, GToTree g) => GToTree (f :*: g) where
  gToTree (f :*: g) = Two (gToTree f) (gToTree g)
  gToTree' c s (f :*: g) = Node (S.encode (c,s)) (Two (gToTree f) (gToTree g))

instance (GToTree f, GToTree g) => GToTree (f :+: g) where
  gToTree (L1 x) = gToTree' 0 1 x
  gToTree (R1 x) = gToTree' 1 1 x
  gToTree' c s (L1 x) = gToTree' c (s*2) x
  gToTree' c s (R1 x) = gToTree' (c+s*2) (s*2) x

instance (ToTree c) => GToTree (K1 i c) where
  gToTree (K1 x) = toTree x
  gToTree' c s (K1 x) = Node (S.encode (c,s)) (toTree x)




class FromTree a where
  fromTree :: BTree alg -> Either String a

  default fromTree :: (Generic a, GFromTree (Rep a)) => BTree alg -> Either String a
  fromTree x = fmap to (gFromTree x)


deserialize :: (S.Serialize a) => BTree alg -> Either String a
deserialize (Leaf b) = S.decode b
deserialize (Hash _) = Right $ error "Attempting to evaluate a hash summary."
deserialize _        = Left "Expected Leaf."

instance FromTree () where
  fromTree Empty    = Right ()
  fromTree (Hash _) = Right $ error "Attempting to evaluate a hash summary."
  fromTree _        = Left "Expected Empty."

instance FromTree Bool where
  fromTree = deserialize

instance FromTree Char where
  fromTree = deserialize

instance FromTree Double where
  fromTree = deserialize

instance FromTree Float where
  fromTree = deserialize

instance FromTree Int where
  fromTree = deserialize

instance FromTree Integer where
  fromTree = deserialize




class GFromTree f where
  gFromTree :: BTree alg -> Either String (f p)
  gFromTree' :: Count -> Size -> BTree alg -> Either String (f p)


instance GFromTree U1 where
  gFromTree Empty    = Right U1
  gFromTree (Hash _) = Right $ error "Attempting to evaluate a hash summary."
  gFromTree _        = Left "Invalid encoding for U1. Expected Empty."
  gFromTree' _ _ _   = Left "Invalid encoding for :+:. Expected Leaf or Node."

instance GFromTree a => GFromTree (M1 i c a) where
  gFromTree (Hash _) = Right $ error "Attempting to evaluate a hash summary."
  gFromTree x        = fmap M1 (gFromTree x)
  gFromTree' c s t   = fmap M1 (gFromTree' c s t)

instance (GFromTree f, GFromTree g) => GFromTree (f :*: g) where
  gFromTree h@(Hash _) = (:*:) <$> gFromTree h <*> gFromTree h
  gFromTree (Two l r)  = (:*:) <$> gFromTree l <*> gFromTree r
  gFromTree _          = Left "Invalid encoding for :*:. Expected Two."
  gFromTree' _ _ _     = Left "Invalid encoding for :+:. Expected Leaf or Node."

getInts :: B.ByteString -> Either String (Count,Size)
getInts = S.decode

instance (GFromTree f, GFromTree g) => GFromTree (f :+: g) where
  gFromTree (Hash _)   = Right $ error "Attempting to evaluate a hash summary."
  gFromTree (Leaf a)   = do (c,s) <- getInts a
                            gFromTree' c s Empty
  gFromTree (Node a t) = do (c,s) <- getInts a
                            gFromTree' c s t
  gFromTree _          = Left "Invalid encoding for :+:. Expected Leaf or Node."
  gFromTree' c 1 t | even c    = fmap L1 (gFromTree t)
                   | otherwise = fmap R1 (gFromTree t)
  gFromTree' c s t | even c    = fmap L1 (gFromTree' c' s' t)
                   | otherwise = fmap R1 (gFromTree' c' s' t)
                     where
                       c' = shift c (-1)
                       s' = s `div` 2

instance (FromTree c) => GFromTree (K1 i c) where
  gFromTree (Hash _) = Right $ error "Attempting to evaluate a hash summary."
  gFromTree t        = fmap K1 (fromTree t)
  gFromTree' _ _ _   = Left "Invalid encoding for :+:. Expected Leaf or Node."
