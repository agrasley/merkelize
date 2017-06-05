{-# LANGUAGE DeriveGeneric #-}
module TestGenerics where

import GHC.Generics
import NewestTree

-- let y = [A, R3, J E, Y Z] :: [AZ (AZ ())]
-- let z = toTree y
-- let x = fromTree z :: Either String [AZ (AZ ())]
-- let a = fromPath (One (L (One None))) z :: BTree SHA256
-- let isHeadA (Right (A:_)) = True
-- let a' = fromTree a :: Either String [AZ (AZ ())]
-- isHeadA a'
-- let b = fromPath (One (R (One (L (One None))))) z :: BTree SHA256
-- let isSndR3 (Right (_:R3:_)) = True
-- let b' = fromTree b :: Either String [AZ (AZ ())]
-- isSndR3 b'
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
