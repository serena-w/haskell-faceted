{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.Pure (
  Label,
  Faceted,
  makePrivate,
  makeFacets,
  makePublic,
  bottom
  ) where

import Faceted.Internal

import Control.Applicative

bottom :: (Eq a) => Faceted a
bottom = Prim (CBottom)

-- | < k ? x : bottom >   ====>  makePrivate k x

makePrivate :: (Eq a) => Label -> a -> Faceted a
makePrivate k x = Prim (CFaceted k (Raw x) (CBottom))

makeFacets :: (Eq a) => Label -> a -> a -> Faceted a
makeFacets k x y = Prim (CFaceted k (Raw x) (Raw y))

-- | x ==> Raw x ===> makePublic x

makePublic :: (Eq a) => a -> Faceted a
makePublic x = Prim (Raw x)
