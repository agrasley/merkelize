# Merkelize

This project presents authenticated data structures for free via a Haskell library.
Authenticated data structures provide a mechanism for proving that information
in a data structure has remained unmodified by untrusted third parties by providing
a unique summary digest of all the information in the data structure.

## Setup

The simplest way to get started with this library is using [Stack](https://docs.haskellstack.org/en/stable/README/).
Make sure you have Stack installed and then call `stack setup && stack build`
from the directory of this project. This will install the necessary dependencies.
From there you can use `stack ghci` to test the library in the REPL.

## Usage

### Structure

The library is all contained in the `src/Data/Merkelize.hs` file.

### Creating an ADS

To create an ADS from a Haskell value, the value's type must implement the `ToTree`
class. This class provides the method `toTree` that converts values of a particular
type into an isomorphic tree value tagged with bytestring values (type `BTree alg`
where `alg` is the type of the cryptographic hash function used).

The simplest way to declare an instance of `ToTree` is to use the default generic
implementation. As long as the type is an instance of the `Generic` class, the
instance for `ToTree` is derived automatically:

```haskell
{-# LANGUAGE DeriveGeneric #-}

data Test1 = A1 | B1
  deriving Generic

instance ToTree Test1

instance ToTree a => ToTree [a]
```

Built-in instances are provided by the library for a number of common types
of kind `*`, including `Int`, `Char`, `Bool`, `Double`, etc.

Once you have an instance of ToTree, conversion to an ADS is fairly straightforward.
You may need to provide a specific type for the cryptographic hash function used
if it cannot be inferred, as this library allows for a choice of hash functions
based on typeclasses. For instance, if you want to use the `SHA256` hash function,
you would execute this in ghci:

```
> import Crypto.Hash
> toTree [True,False,False] :: BTree SHA256
Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
       Leaf "\SOH"
    Two
       Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
              Leaf "\NUL"
           Two
              Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
                     Leaf "\NUL"
                  Two
                     Leaf "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
```

The full list of possible cryptographic hash functions for use with this library
is available in the documentation for the
[`crytonite`](https://hackage.haskell.org/package/cryptonite-0.23/docs/Crypto-Hash-Algorithms.html)
package on hackage. I recommend SHA256 as a good default.

### Manipulating an ADS

Once you have an ADS value (of type `BTree alg` where `alg` is some hash function),
you can get the unique root digest for that ADS with the `getDigest` function:

```
> import Crypto.Hash
> let ads = toTree [True,False,False] :: BTree SHA256
> getDigest ads
65c0b3dc9c9f99dc416de7181cee0b021a0132af3bc4ed1284c7e3e52eecb5a7
```

The `fromPath` function creates a new ADS value from a path through another ADS.
All parts of the original ADS value not included in the path are converted to
their digest values. The original and the new ADS will always share the
same root digest.

A `Path` value marks elements of an ADS tree for inclusion or summarization. The
value `None` causes the ADS value to be replaced with its hash digest. The value
`One` includes any `Leaf` or `Empty` value. A `Some p` value includes a `Node` value,
recursively calling `fromPath` with `p` on its child. `L p` and `R p` include the left
and right alternatives of a `Two` value, respectively, while replacing the other
alternative with its hash digest and calling `fromPath` with `p` on the corresponding
child. `Both pl pr` includes both alternatives for a `Two` value rescursively.
Any `Path` value will include a value of `Hash`. Paths that do not match the given
ADS value (such as attempting to use the path `L p` on a `Leaf`) result in a runtime
error.

As an example, consider the value `ads` given above:

```
>  ads
Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
       Leaf "\SOH"
    Two
       Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
              Leaf "\NUL"
           Two
              Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
                     Leaf "\NUL"
                  Two
                     Leaf "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
```

Suppose we want to create an ADS that only contains the head of the list. We could
create it like so:

```
> let ads' = fromPath (Some (L One)) ads
> ads'
Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
       Leaf "\SOH"
    Two
       Hash a77a48985c...
```

Similarly we can create an ADS that only contains the second value in the list:

```
> let ads'' = fromPath (Some (R (Some (L One)))) ads
> ads''
Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
       Hash 4bf5122f34...
    Two
       Node "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH"
              Leaf "\NUL"
           Two
              Hash 2d02191fe2...

```

You can check if two ADS values share the same root digest with the `validate` function,
which returns `True` if the root digests are the same. `validate` only returns `True`
if the two ADS values are derived from the same underlying data structure:

```
> validate ads ads'
True
> validate ads' ads''
True
> validate ads (toTree [True,False,True])
False
```

### Converting an ADS Back to a Normal Value

To convert an ADS value back to its typical Haskell representation, you need an
instance of the `FromTree` class. This class provides the method `fromTree` which
attempts to convert the ADS representation back to its canonical form. If the ADS
is not a proper encoding for the given type, it results in an error message contained in
the `Either` error monad.

The simplest way to declare an instance of `FromTree` is to use the default generic
implementation. As long as the type is an instance of the `Generic` class, the
instance for `FromTree` is derived automatically:

```haskell
{-# LANGUAGE DeriveGeneric #-}

data Test1 = A1 | B1
  deriving Generic

instance FromTree Test1

instance FromTree a => FromTree [a]
```

Again, built-in instances are provided by the library for a number of common types
of kind `*`, including `Int`, `Char`, `Bool`, `Double`, etc.

It is important to remember that if the type of the result of `fromTree` cannot
be inferred that you will likely fail to successfully convert the ADS back to its
canonical representation. As such, when working in the REPL, it often necessary
to provide type annotations:

```
-- Failed due to type mismatch
> fromTree ads
Left "Expected Empty."

-- Succeeds
> fromTree ads :: Either String [Bool]
Right [True,False,False]
```

Any ADS that includes "holes" with hash digests summarizing the missing information
will convert the hole into the value `error "Attempting to evaluate a hash summary."`.
In this way a well-typed data structure with holes can still be produced that fails
when an attempt is made to evaluate information that is not present.

```
> let rightHead (Right x) = head x
> rightHead (fromTree ads :: Either String [Bool])
True
> rightHead (fromTree ads' :: Either String [Bool])
True
> rightHead (fromTree ads'' :: Either String [Bool])
*** Exception: Attempting to evaluate a hash summary.
```

### testgenerics.hs

There are more examples that can be run in ghci in the `src/testgenerics.hs` file.
