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

data ToState alg = ToState {toSum :: ToSum, toProd :: ToProd alg}

type Size = Int
type Count = Maybe Int
type ToSum = (Count,Size)
type ToProd alg = [BTree alg]

resetSum :: State (ToState alg) ()
resetSum = putSum (Nothing, 0)

getSum :: State (ToState alg) ToSum
getSum = do
  s <- get
  return (toSum s)

putSum :: ToSum -> State (ToState alg) ()
putSum s = do
  p <- getProd
  put $ ToState s p

getProd :: State (ToState alg) (ToProd alg)
getProd = do
  s <- get
  return (toProd s)

putProd :: ToProd alg -> State (ToState alg) ()
putProd p = do
  s <- getSum
  put $ ToState s p

resetProd :: State (ToState alg) ()
resetProd = putProd []

pushProd :: BTree alg -> State (ToState alg) ()
pushProd t = do
  p <- getProd
  putProd (t:p)

runGToTree :: (GToTree f) => f p -> BTree alg
runGToTree g = evalState (gToTree g) (ToState (Nothing, 0) [])

class GToTree f where
  gToTree :: f p -> State (ToState alg) (BTree alg)
  gToSTree :: f p -> State (ToState alg) ()


u1ToTree :: Count -> Size -> BTree alg
u1ToTree Nothing  _ = Empty
u1ToTree (Just i) s = serialize (i,s)

instance GToTree U1 where
  gToTree U1 = do
    (m,s) <- getSum
    resetSum
    return $ u1ToTree m s
  gToSTree U1 = do
    (m,s) <- getSum
    resetSum
    pushProd (u1ToTree m s)

instance GToTree a => GToTree (M1 i c a) where
  gToTree = gToTree . unM1
  gToSTree = gToSTree . unM1

prodToTree :: Count -> Size -> ToProd alg -> BTree alg
prodToTree Nothing _ p = Forest (reverse p)
prodToTree (Just i) s p = Node (S.encode (i,s)) (Forest (reverse p))

instance (GToTree f, GToTree g) => GToTree (f :*: g) where
  gToTree (f :*: g) = do
    (mi,s) <- getSum
    resetSum
    gToSTree f
    gToSTree g
    p <- getProd
    resetProd
    return $ prodToTree mi s p
  gToSTree (f :*: g) = do
    gToSTree f
    gToSTree g

l1 :: State (ToState alg) ()
l1 = do
  (m,s) <- getSum
  case m of
    Nothing -> putSum (Just 0, 1)
    Just i  -> putSum (Just i, s*2)

r1 :: State (ToState alg) ()
r1 = do
  (m,s) <- getSum
  case m of
    Nothing -> putSum (Just 1, 1)
    Just i  -> putSum (Just (i+s*2), s*2)

sumToSTree :: (GToTree a) => a p -> State (ToState alg) ()
sumToSTree x = do
  p <- getProd
  resetProd
  y <- gToTree x
  putProd p
  pushProd y

instance (GToTree f, GToTree g) => GToTree (f :+: g) where
  gToTree (L1 x) = do
    l1
    gToTree x
  gToTree (R1 x) = do
    r1
    gToTree x
  gToSTree (L1 x) = do
    l1
    sumToSTree x
  gToSTree (R1 x) = do
    r1
    sumToSTree x

k1ToTree :: (ToTree a) => Count -> Size -> a -> BTree alg
k1ToTree Nothing _ x = toTree x
k1ToTree (Just i) s x = Node (S.encode (i,s)) (toTree x)

instance (ToTree c) => GToTree (K1 i c) where
  gToTree (K1 x) = do
    (m,s) <- getSum
    resetSum
    return $ k1ToTree m s x
  gToSTree (K1 x) = do
    (m,s) <- getSum
    resetSum
    pushProd $ k1ToTree m s x


class FromTree a where
  fromTree :: BTree alg -> Either String a

  default fromTree :: (Generic a, GFromTree (Rep a)) => BTree alg -> Either String a
  fromTree x = do
     y <- runGFromTree x
     return (to y)

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

getInts :: B.ByteString -> GMonad alg (Int, Size)
getInts b = case S.decode b of
             Left s -> throwError s
             Right is -> return is

leafHelper :: (GFromTree g, GFromTree f) => Int -> Size -> GMonad alg ((:+:) f g p)
leafHelper i 1 = do
  lift $ put Empty
  if even i then do
    x <- gFromTree
    return (L1 x)
  else do
    x <- gFromTree
    return (R1 x)
leafHelper i s = do
  let j = shift i (-1)
  let s' = s `div` 2
  lift $ put (serialize (j,s'))
  if even i then do
    x <- gFromTree
    return (L1 x)
  else do
    x <- gFromTree
    return (R1 x)

nodeHelper :: (GFromTree g, GFromTree f) => BTree alg -> Int -> Size -> GMonad alg ((:+:) f g p)
nodeHelper t i 1 = do
  lift $ put t
  if even i then do
    x <- gFromTree
    return (L1 x)
  else do
    x <- gFromTree
    return (R1 x)
nodeHelper t i s = do
  let j = shift i (-1)
  let s' = s `div` 2
  lift $ put (Node (S.encode (j,s')) t)
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
        (i,s) <- getInts a
        leafHelper i s
      (Node a t) -> do
        (i,s) <- getInts a
        nodeHelper t i s
      (Forest (Hole _:xs)) -> do
        lift $ put (Forest xs)
        return undefined
      (Forest (Leaf a:xs)) -> do
        (i,s) <- getInts a
        y <- leafHelper i s
        lift $ put (Forest xs)
        return y
      (Forest (Node a t:xs)) -> do
        (i,s) <- getInts a
        y <- nodeHelper t i s
        lift $ put (Forest xs)
        return y
      _ -> throwError "Invalid encoding for :+:. Expected Leaf or Node."

k1FromTree :: (FromTree c) => BTree alg -> GMonad alg c
k1FromTree t = case fromTree t of
                 Left s -> throwError s
                 Right c -> return c

instance (FromTree c) => GFromTree (K1 i c) where
  gFromTree = do
    x <- lift get
    case x of
      (Hole _) -> return undefined
      (Forest (Forest _ : xs)) -> undefined
      y -> do
        c <- k1FromTree y
        return $ K1 c
