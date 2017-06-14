{-# LANGUAGE DeriveGeneric #-}
module TestGenerics where

import GHC.Generics
import NewestTree
import Crypto.Hash

{-
let y = [A, R3, J E, Y Z] :: [AZ (AZ ())]
let z = toTree y
let x = fromTree z :: Either String [AZ (AZ ())]
let a = fromPath (One (L (One None))) z :: BTree SHA256
let isHeadA (Right (A:_)) = True
let a' = fromTree a :: Either String [AZ (AZ ())]
isHeadA a'
let b = fromPath (One (R (One (L (One None))))) z :: BTree SHA256
let isSndR3 (Right (_:R3:_)) = True
let b' = fromTree b :: Either String [AZ (AZ ())]
isSndR3 b'
-}

{-
let x = [1,2,3] :: [Int]
let y = toTree x
let z = fromTree y :: Either String [Int]
let a = fromPath (One (L (One None))) y :: BTree SHA256
let head' (Right (x:_)) = x
let a' = fromTree a :: Either String [Int]
head' a'
let b = fromPath (One (R (One (L (One None))))) y :: BTree SHA256
let snd' (Right (_:x:_)) = x
let b' = fromTree b :: Either String [Int]
snd' b'
let c = fromPath (One (Both (One None) (One (L (One None))))) y :: BTree SHA256
let c' = fromTree c :: Either String [Int]
-}

{-
let x = (1,2,3,4) :: (Int,Int,Int,Int)
let y = toTree x
let z = fromTree y :: Either String (Int,Int,Int,Int)
let a = fromPath (Both None None) y :: BTree SHA256
let a' = fromTree a :: Either String (Int,Int,Int,Int)
let b = fromPath (R (L (One None))) y :: BTree SHA256
let b' = fromTree b :: Either String (Int,Int,Int,Int)
let third (Right (_, _, x, _)) = x
-}
data AZ a = A | B a | C | D | E | F | G | H | I | J a | K | L3 | M | N | O | P a | Q | R3 | S | T | U | V | W | X | Y a | Z
  deriving (Show, Generic)

instance ToTree a => ToTree (AZ a)
instance FromTree a => FromTree (AZ a)

data Test1 = L2 Int Int Int Int | R2 Int Int Int
  deriving (Show, Generic)

instance ToTree Test1
instance FromTree Test1

instance ToTree a => ToTree [a]
instance FromTree a => FromTree [a]

instance (ToTree a, ToTree b, ToTree c, ToTree d) => ToTree (a,b,c,d)
instance (FromTree a, FromTree b, FromTree c, FromTree d) => FromTree (a,b,c,d)
