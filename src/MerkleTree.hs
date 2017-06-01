{-# LANGUAGE GADTs, OverloadedStrings #-}
module MerkleTree where

import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.Tree as T

type ByteT = T.Tree ByteString
type PathT = T.Tree Int

data MerkleT alg where
  HashNode :: (HashAlgorithm alg) => Digest alg -> MerkleT alg
  ByteNode :: (HashAlgorithm alg) => ByteString -> [MerkleT alg] -> MerkleT alg

instance Show (MerkleT alg) where
  show (HashNode d) = let
                        s = show d
                      in if length s > 10 then
                        take 10 s ++ "..."
                      else
                        s
  show (ByteNode b ts) = "ByteNode " ++ show b ++ " " ++ show ts

getDigest :: MerkleT alg -> Digest alg
getDigest (HashNode d) = d
getDigest (ByteNode b ts) = hashFinalize $ hashUpdate (hashUpdates hashInit (fmap getDigest ts)) b

validate :: MerkleT alg -> MerkleT alg -> Bool
validate a b = getDigest a == getDigest b

fromByteTree :: (HashAlgorithm alg) => ByteT -> MerkleT alg
fromByteTree (T.Node b ts) = ByteNode b (fmap fromByteTree ts)

fromPath :: (HashAlgorithm alg) => ByteT -> Maybe PathT -> MerkleT alg
fromPath bt Nothing = HashNode (getDigest . fromByteTree $ bt)
fromPath (T.Node b bs) (Just (T.Node _ is)) = ByteNode b (snd pathFold)
  where iChildren = fmap (\t@(T.Node i _) -> (i,t)) is
        foldFn x (cnt, ms) = (cnt-1, fromPath x (lookup cnt iChildren):ms)
        pathFold = foldr foldFn (length bs - 1,[]) bs

--{- Testing
p1 :: Maybe PathT
p1 = Just (T.Node 0 [T.Node 0 [T.Node 0 []]])

p2 :: Maybe PathT
p2 = Just (T.Node 0 [T.Node 0 [T.Node 0 []], T.Node 1 [T.Node 1 []]])

bt1 :: ByteT
bt1 = T.Node "a" [T.Node "b" [T.Node "c" [], T.Node "d" []], T.Node "e" [T.Node "f" [], T.Node "g" []]]

mt1 :: MerkleT SHA256
mt1 = fromByteTree bt1

mt1Nothing :: MerkleT SHA256
mt1Nothing = fromPath bt1 Nothing

mt1Path1 :: MerkleT SHA256
mt1Path1 = fromPath bt1 p1

mt1Path2 :: MerkleT SHA256
mt1Path2 = fromPath bt1 p2

bt2 :: ByteT
bt2 = T.Node "a" [T.Node "b" [T.Node "c" [], T.Node "d" []], T.Node "e" [T.Node "g" [], T.Node "f" []]]

mt2 :: MerkleT SHA256
mt2 = fromByteTree bt2

mt2Nothing = fromPath bt2 Nothing

testValidateId :: Bool
testValidateId = validate mt1 mt1

testValidateDiff :: Bool
testValidateDiff = validate mt1 mt2

testNothingPath :: Bool
testNothingPath = validate mt1 mt1Nothing

testNothingDiff :: Bool
testNothingDiff = validate mt1Nothing mt2Nothing


-- -}
