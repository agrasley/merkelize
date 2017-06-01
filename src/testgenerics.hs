{-# LANGUAGE DeriveGeneric #-}
module TestGenerics where

import GHC.Generics
import NewerTree

data I = I Int
  deriving (Show, Generic)

data P a b = P a b
  deriving (Show, Generic)

data P1 a b = P1 (P a b)
  deriving (Show, Generic)

instance (ToTree a, ToTree b) => ToTree (P a b)

instance (ToTree a, ToTree b) => ToTree (P1 a b)

data AB = A | B
  deriving (Show, Generic)

instance ToTree AB

data Pair = Pair AB AB
  deriving (Show, Generic)

instance ToTree Pair

data TwoPair = TwoPair Pair Pair
  deriving (Show, Generic)

instance ToTree TwoPair

data Test2 = L2 AB AB | R2 AB
  deriving (Show, Generic)

data Test1 = L Int Int Int Int | R Int Int Int
  deriving (Show, Generic)

instance ToTree Test1

instance ToTree a => ToTree [a]

{-
L1 (
      (K1 {unK1 = 1} :*: K1 {unK1 = 2})
    :*:
      (K1 {unK1 = 3} :*: K1 {unK1 = 4})
)

T.Node "0" [1, 2, 3, 4]
-}
