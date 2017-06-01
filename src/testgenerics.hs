{-# LANGUAGE DeriveGeneric #-}
module TestGenerics where

import GHC.Generics
import NewerTree
import qualified Data.Tree as T

-- let y = [A, R, J E, Y Z] :: [AZ (AZ ())]
-- let z = toTree y
-- let a = fromPath (Just (T.Node 0 [T.Node 0 [T.Node 0 []]])) z :: BTree SHA256
-- let isHeadA (Right (A:_)) = True
-- let a' = fromTree a :: Either String [AZ (AZ ())]
-- isHeadA a'
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
