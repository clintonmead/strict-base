{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.List.NonEmpty
-- Copyright   :  (c) 2017 Daniel Mendler
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Daniel Mendler <mail@daniel-mendler.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict @NonEmpty@.
--
-- Same as the standard Haskell NonEmpty, but strict.
--
-----------------------------------------------------------------------------

module Data.Strict.List.NonEmpty (
  NonEmpty(..)
) where

import qualified Data.List.NonEmpty as L
import Data.Strict.List
import GHC.Generics (Generic, Generic1)
import Data.Data (Data, Typeable)
import Data.Strict.Trustworthy
import Data.Strict.Class
import Data.Semigroup (Semigroup, (<>))

infixr 5 :|

-- | The strict list type.
data NonEmpty a = !a :| !(List a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance IsStrict (NonEmpty a) where
  type Lazy (NonEmpty a) = L.NonEmpty a
  toStrict   (a L.:| as) = a   :| toStrict as
  fromStrict (a   :| as) = a L.:| fromStrict as

instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  fromList (a : as)  = a :| fromList as
  fromList []        = error "NonEmpty.fromList: empty list"
  toList  ~(a :| as) = a : toList as

instance Applicative NonEmpty where
  {-# INLINE pure #-}
  pure x = x :| Nil
  {-# INLINE (<*>) #-}
  (fi:|fl) <*> (xi:|xl) = fi xi :| go1 xl where
    go1 (x:!xs) = fi x :! go1 xs
    go1 Nil = go2 fl
    go2 (f:!fs) = f xi :! go2' xl where
      go2' (x:!xs) = f x :! go2' xs
      go2' Nil = go2 fs
    go2 Nil = Nil

#if MIN_VERSION_base(4,10,0)
  {-# INLINE liftA2 #-}
  liftA2 f (xi:|xl) (yi:|yl) = f xi yi :| go1 yl where
    go1 (y:!ys) = f xy y :! go1 ys
    go1 Nil = go2 xl
    go2 (x:!xs) = f x yi :! go2' yl where
      go2' (y:!ys) = f x y :! go2' ys
      go2' Nil = go2 xs
    go2 Nil = Nil
#endif

instance Semigroup (NonEmpty a) where
  (xi:|xl) <> (yi:|yl) = xi :| go xl where
    go (x:!xs) = x :! go xs
    go Nil = yi :! yl
