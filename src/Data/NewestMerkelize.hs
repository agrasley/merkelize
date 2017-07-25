{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.NewestMerkelize (
    ADS,
    Merkelize(..),
    GMerkelize(..),
  ) where

import qualified Data.ByteString as B
import GHC.Generics
import Data.Store
import Crypto.Hash
import GHC.TypeLits
import Data.Proxy
import Data.Word
import qualified Data.ByteString.Unsafe as UB

data ADS alg = Hash (Digest alg)
             | Unit
             | Leaf B.ByteString
             | Sum B.ByteString (ADS alg)
             | Prod (ADS alg) (ADS alg)

class Merkelize a where
  toADS :: a -> ADS alg

  default toADS :: (Generic a, GMerkelize (Rep a)) => a -> ADS alg
  toADS = gToADS . from

  fromADS :: ADS alg -> Either String a

  default fromADS :: (Generic a, GMerkelize (Rep a)) => ADS alg -> Either String a
  fromADS x = fmap to (gFromADS x)

class GMerkelize f where
  gToADS :: f p -> ADS alg
  gFromADS :: ADS alg -> Either String (f p)

hashVal :: Either String a
hashVal = Right $ error "Attempting to evaluate a hash summary."

instance GMerkelize U1 where
  gToADS U1 = Unit

  gFromADS Unit = Right U1
  gFromADS (Hash _) = hashVal
  gFromADS _        = Left $ "Invalid encoding for U1."

instance GMerkelize a => GMerkelize (M1 i c a) where
  gToADS = gToADS . unM1

  gFromADS (Hash _) = hashVal
  gFromADS x = fmap M1 (gFromADS x)

instance (GMerkelize f, GMerkelize g) => GMerkelize (f :*: g) where
  gToADS (f :*: g) = Prod (gToADS f) (gToADS g)

  gFromADS h@(Hash _) = (:*:) <$> gFromADS h <*> gFromADS h
  gFromADS (Prod l r) = (:*:) <$> gFromADS l <*> gFromADS r
  gFromADS _        = Left $ "Invalid encoding for :*:."

instance (Merkelize c) => GMerkelize (K1 i c) where
  gToADS (K1 x) = toADS x

  gFromADS (Hash _) = hashVal
  gFromADS t = fmap K1 (fromADS t)

type family SumArity (a :: * -> *) :: Nat where
  SumArity (C1 c a) = 1
  SumArity (x :+: y) = SumArity x + SumArity y

class KnownNat n => GMerkelizeSum (n :: Nat) (f :: * -> *) where
  gToADSSum :: f p -> Proxy n -> ADS alg
  gFromADSSum :: Word8 -> Proxy n -> ADS alg -> Either String (f p)

instance (GMerkelizeSum n f, GMerkelizeSum (n + SumArity f) g, KnownNat n)
         => GMerkelizeSum n (f :+: g) where
  gToADSSum (L1 x) _ = gToADSSum x (Proxy :: Proxy n)
  gToADSSum (R1 x) _ = gToADSSum x (Proxy :: Proxy (n + SumArity f))

  gFromADSSum w p a
        | w < sizeL = L1 <$> gFromADSSum w p a
        | otherwise = R1 <$> gFromADSSum w (Proxy :: Proxy (n + SumArity f)) a
      where
        sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity f)))

instance (GMerkelize a, KnownNat n) => GMerkelizeSum n (C1 c a) where
  gToADSSum x _ | Unit <- gToADS x = Leaf b
                | otherwise = Sum b (gToADS x)
    where b = B.singleton (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)

  gFromADSSum w _ a | w == cur = gFromADS a
                    | w > cur = Left "Sum tag mismatch."
                    | otherwise = Left "This shouldn't happen."
    where cur = fromInteger (natVal (Proxy :: Proxy n))

getByte :: B.ByteString -> Either String Word8
getByte b | B.length b == 1 = Right $ UB.unsafeHead b
          | otherwise = Left $ "Sum encoded with " ++ show (B.length b) ++ " bytes."

instance (SumArity (f :+: g) <= 255, GMerkelizeSum 0 (f :+: g)) => GMerkelize (f :+: g) where
  gToADS x = gToADSSum x (Proxy :: Proxy 0)

  gFromADS (Hash _) = hashVal
  gFromADS (Leaf b) = do
    w <- getByte b
    gFromADSSum w (Proxy :: Proxy 0) Unit
  gFromADS (Sum b a) = do
    w <- getByte b
    gFromADSSum w (Proxy :: Proxy 0) a
  gFromADS _ = Left "Invalid encoding for :+:."
