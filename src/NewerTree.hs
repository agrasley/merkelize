{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts #-}

module NewerTree where

import qualified Data.ByteString as B
import GHC.Generics
import Control.Monad.State
import qualified Data.Serialize as S
import Data.Bits (shift)
import Crypto.Hash
import Control.Monad.Except

data Tree a alg = Empty | Leaf a | Node a (Tree a alg) | Forest [Tree a alg] | Hole (Digest alg)
  deriving (Show)

type BTree alg = Tree B.ByteString alg

getDigest :: (HashAlgorithm alg) => BTree alg -> Digest alg
getDigest Empty       = hashFinalize hashInit
getDigest (Leaf a)    = hash a
getDigest (Node a t)  = hashFinalize $ hashUpdate (hashUpdate hashInit (getDigest t)) a
getDigest (Forest ts) = hashFinalize $ hashUpdates hashInit (fmap getDigest ts)
getDigest (Hole d)    = d

serialize :: (S.Serialize a) => a -> BTree alg
serialize a = Leaf (S.encode a)

class ToTree a where
  toTree :: a -> BTree alg

  default toTree :: (Generic a, GToTree (Rep a)) => a -> BTree alg
  toTree x = runGToTree (from x)

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

runGToTree :: (GToTree f) => f p -> BTree alg
runGToTree g = evalState (gToTree g) Nothing

reset :: State (Maybe Int) ()
reset = put Nothing

class GToTree f where
  gToTree :: f p -> State (Maybe Int) (BTree alg)

instance GToTree U1 where
  gToTree U1 = do
    m <- get
    reset
    case m of
      Nothing -> return Empty
      Just i  -> return $ Leaf (S.encode i)

instance GToTree a => GToTree (M1 i c a) where
  gToTree = gToTree . unM1

instance (GToTree f, GToTree g) => GToTree (f :*: g) where
  gToTree (f :*: g) = do
    m <- get
    reset
    f' <- gToTree f
    g' <- gToTree g
    let res a b | Forest xs <- a,
                  Forest ys <- b  = Forest $ xs ++ ys
                | Forest xs <- a  = Forest $ xs ++ [b]
                | Forest ys <- b  = Forest $ a : ys
                | otherwise       = Forest [a,b]
    case m of
      Nothing -> return $ res f' g'
      Just i  -> return $ Node (S.encode i) (res f' g')

l1 :: State (Maybe Int) ()
l1 = do
  m <- get
  case m of
    Nothing -> put (Just 0)
    Just i  -> put (Just (shift i 1))

r1 :: State (Maybe Int) ()
r1 = do
  m <- get
  case m of
    Nothing -> put (Just 1)
    Just i  -> put (Just (shift i 1 + 1))

instance (GToTree f, GToTree g) => GToTree (f :+: g) where
  gToTree (L1 x) = do
    l1
    gToTree x
  gToTree (R1 x) = do
    r1
    gToTree x

instance (ToTree c) => GToTree (K1 i c) where
  gToTree (K1 x) = do
    m <- get
    case m of
      Nothing -> return $ toTree x
      Just i  -> return $ Node (S.encode i) (toTree x)

class FromTree a where
  fromTree :: BTree alg -> Either String a

deserialize :: (S.Serialize a) => BTree alg -> Either String a
deserialize (Leaf b) = S.decode b
deserialize (Hole _) = Right undefined
deserialize _        = Left "Expected Leaf."

instance FromTree () where
  fromTree Empty    = Right ()
  fromTree (Hole _) = Right undefined
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

type GMonad alg = ExceptT String (State (BTree alg))

class GFromTree f where
  gFromTree :: GMonad alg (f p)


runGFromTree :: (GFromTree f) => BTree alg -> Either String (f p)
runGFromTree = evalState (runExceptT gFromTree)


{-
getCount :: GMonad alg (Maybe Int)
getCount = do
  (_,r) <- lift get
  return r

getTree :: GMonad alg (BTree alg)
getTree = do
  (l,_) <- lift get
  return l

putCount :: Maybe Int -> GMonad alg ()
putCount mi = do
  t <- getTree
  lift $ put (t,mi)

putTree :: BTree alg -> GMonad alg ()
putTree t = do
  mi <- getCount
  lift $ put (t,mi)
-}
{-
instance GFromTree U1 where
  gFromTree = do
    x <- lift get
    case x of
      Empty -> return U1
      (Hole _) -> return undefined
      (Forest (Hole _:xs)) -> do
        lift $ put (Forest xs)
        return undefined
      (Forest (Empty:xs)) -> do
        lift $ put (Forest xs)
        return U1
      _ -> throwError "Invalid encoding for U1. Expected Empty."

instance GFromTree a => GFromTree (M1 i c a) where
  gFromTree = do
    x <- lift get
    case x of
      (Hole _) -> return undefined
      _        -> do
        y <- gFromTree
        return (M1 y)

instance (GFromTree f, GFromTree g) => GFromTree (f :*: g) where
  gFromTree = do
    x <- lift get
    case x of
      (Hole _) -> return undefined
      (Forest _) -> do
        l <- gFromTree
        r <- gFromTree
        return (l :*: r)
      _ -> throwError "Invalid encoding for :*:. Expected Forest."

getInt :: B.ByteString -> GMonad alg Int
getInt b = case S.decode b of
             Left s -> throwError s
             Right i -> return i

leafHelper :: (GFromTree g, GFromTree f) => Int -> ExceptT String (State (BTree alg)) ((:+:) f g p)
leafHelper 0 = do
  lift $ put Empty
  x <- gFromTree
  return (L1 x)
leafHelper 1 = do
  lift $ put Empty
  x <- gFromTree
  return (R1 x)
leafHelper i = do
  let j = shift i (-1)
  lift $ put (serialize j)
  if even i then do
    x <- gFromTree
    return (L1 x)
  else do
    x <- gFromTree
    return (R1 x)

nodeHelper :: (GFromTree g, GFromTree f) => BTree alg -> Int -> ExceptT String (State (BTree alg)) ((:+:) f g p)
nodeHelper t 0 = do
  lift $ put t
  x <- gFromTree
  return (L1 x)
nodeHelper t 1 = do
  lift $ put t
  x <- gFromTree
  return (R1 x)
nodeHelper t i = do
  let j = shift i (-1)
  lift $ put (Node (S.encode j) t)
  if even i then do
    x <- gFromTree
    return (L1 x)
  else do
    x <- gFromTree
    return (R1 x)

instance (GFromTree f, GFromTree g) => GFromTree (f :+: g) where
  gFromTree = do
    x <- lift get
    case x of
      (Hole _) -> return undefined
      (Leaf a) -> do
        i <- getInt a
        leafHelper i
      (Node a t) -> do
        i <- getInt a
        nodeHelper t i
      (Forest (Hole _:xs)) -> do
        lift $ put (Forest xs)
        return undefined
      (Forest (Leaf a:xs)) -> undefined
      (Forest (Node a t:xs)) -> undefined
      _ -> throwError "Invalid encoding for :+:. Expected Leaf or Node."
-}
