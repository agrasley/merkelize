{-# LANGUAGE DeriveGeneric #-}
module TestGenerics where

import GHC.Generics
import NewerTree

data AZ a = A | B a | C | D | E | F | G | H | I | J a | K | L | M | N | O | P a | Q | R | S | T | U | V | W | X | Y a | Z
  deriving (Show, Generic)

instance ToTree a => ToTree (AZ a)
instance FromTree a => FromTree (AZ a)

data Test1 = L2 Int Int Int Int | R2 Int Int Int
  deriving (Show, Generic)

instance ToTree Test1
instance FromTree Test1

instance ToTree a => ToTree [a]
instance FromTree a => FromTree [a]

{-
L1 (
      (K1 {unK1 = 1} :*: K1 {unK1 = 2})
    :*:
      (K1 {unK1 = 3} :*: K1 {unK1 = 4})
)

T.Node "0" [1, 2, 3, 4]
-}
