{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor,DeriveDataTypeable #-}

module Faceted.Internal(
  Label,
  CFaceted(Raw,CFaceted,CBottom,CBind),
  Faceted(Prim,Return,Bind,Bottom),
  run,
  PC,
  Branch(Private,Public),
  View,
  ExtView,
  PolicyEnv,
  FIO(FIO),
  runFIO,
  pcCF,
  pcF,
  projectC,
  project,
  projectExt,
  visibleTo,
  runCFaceted,
  runFaceted,
  getView,
  ) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import System.IO
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

-- | A security label is any string.
-- Labels need not be secrets; they
-- may be readable strings. Information flow security is ensured by a
-- combination of the type system and dynamic checks.
type Label = String

-- | A _view_ is any set of labels.
-- In enforcing information flow security Each view may see a different value.
type View = [Label]

type ExtView = String
type PolicyEnv = Map Label (ExtView -> CFaceted Bool)

-- | Type 'Faceted a' represents (possibly) faceted values.
--
-- <k ? x : y>   ====>  Faceted k x y

{-
data Faceted a =
    Raw a
  | Faceted Label (Faceted a) (Faceted a)
  | Bottom
  | Join (Faceted (Faceted a))
  deriving (Show, Eq, Typeable)
-}

data CFaceted a where
  Raw :: Eq a => a -> CFaceted a
  CFaceted :: Eq a => Label -> CFaceted a -> CFaceted a -> CFaceted a
  CBottom :: Eq a => CFaceted a
  CBind :: Eq a => CFaceted a -> (a -> CFaceted b) -> CFaceted b

instance Eq (CFaceted a) where
  (==) (Raw a) (Raw b) = (a==b)
  (==) (CFaceted k1 priv1 pub1) (CFaceted k2 priv2 pub2) = (k1==k2) && (priv1==priv2) && (pub1==pub2)
  (==) CBottom CBottom = True
  (==) _facet1 _facet2 = False -- what about CBind?

fmapC :: (Eq b) => (a -> b) -> CFaceted a -> CFaceted b
fmapC f (Raw v)               = Raw (f v)
fmapC f (CFaceted k priv pub) = CFaceted k (fmapC f priv) (fmapC f pub)
fmapC f CBottom                = CBottom

data Faceted a where
  Prim   :: (Eq a) => CFaceted a -> Faceted a
  Return :: a -> Faceted a
  Bind   :: Faceted a -> (a -> Faceted b) -> Faceted b -- ??????
  Bottom :: Faceted a -- ?????

run :: (Eq a) => Faceted a -> CFaceted a
run (Prim fv)            = fv
run (Return x)           = Raw x
run (Bottom)             = CBottom
run (Bind (Prim fv) f)   = CBind fv (run . f) -- ??????
run (Bind (Return a) f)  = run (f a)
run (Bind Bottom _)      = CBottom
run (Bind (Bind ma f) g) = run (Bind ma (\a -> Bind (f a) g))

instance Functor Faceted where
  fmap = liftM

instance Applicative Faceted where
  pure  = return
  (<*>) = ap

instance Monad Faceted where
  return = Return
  (>>=)  = Bind

{- old code for when Faceted was not an Eq instance and was a monad itself
-- | Functor: For when the function is pure but the argument has facets.
instance Functor Faceted where
  fmap f (Raw v)              = Raw (f v)
  fmap f (Faceted k priv pub) = Faceted k (fmap f priv) (fmap f pub)
  fmap f Bottom               = Bottom
--  fmap f (Join ffa)           = Join $ fmap (\fa -> fmap f fa) ffa

-- | Applicative: For when the function and argument both have facets.
instance Applicative Faceted where
  pure x  = Raw x
  (Raw f) <*> x  =  fmap f x
  (Faceted k priv pub) <*> x  =  Faceted k (priv <*> x) (pub <*> x)
  Bottom <*> x  =  Bottom

-- | Monad: Like applicative, but even more powerful. 'Faceted' the free monad
-- over the function 'Facets a = F Label a a | B'.
instance Monad Faceted where
  return x = Raw x
--  (>>=) = flip ((.) Join . fmap)
  (>>=) = Bind
-}

-- | A Branch is a principal or its negatives, and a pc is a set of branches.

data Branch = Public Label | Private Label deriving (Eq, Show)
type PC = [Branch]

-- | << pc ? x : y >>  =====>   pcF pc x y

pcCF :: (Eq a) => PC -> CFaceted a -> CFaceted a -> CFaceted a
pcCF []                     x _ = x
pcCF (Private k : branches) x y = CFaceted k (pcCF branches x y) y
pcCF (Public k  : branches) x y = CFaceted k y (pcCF branches x y)

pcF :: (Eq a) => PC -> Faceted a -> Faceted a -> Faceted a
pcF pc x y = Prim (pcCF pc (run x) (run y))

-- Private
projectC :: (Eq a) => View -> CFaceted a -> Maybe a
projectC view CBottom  = Nothing
projectC view (Raw v) = Just v
projectC view (CFaceted k priv pub)
  | k `elem`    view  = projectC view priv
  | k `notElem` view  = projectC view pub

project :: (Eq a) => View -> Faceted a -> Maybe a
project view = (projectC view) . run

runCFaceted :: (Eq a) => CFaceted a -> PC -> CFaceted a
runCFaceted = f where
  f :: (Eq a) => CFaceted a -> PC -> CFaceted a
  f (CBind ua c) pc = g (f ua pc) where
    g (Raw a) = f (c a) pc
    g (CFaceted k ua1 ua2)
        | Private k `elem` pc = f (CBind ua1 c) pc
        | Public k  `elem` pc = f (CBind ua2 c) pc
        | otherwise           = CFaceted k (f (CBind ua1 c) (Private k : pc))
                                           (f (CBind ua2 c) (Public k : pc))
    g CBottom = CBottom
  f anythingElse pc = anythingElse

runFaceted :: (Eq a) => Faceted a -> PC -> Faceted a
runFaceted x pc = Prim (runCFaceted (run x) pc)

projectExt :: ExtView -> PolicyEnv -> CFaceted a -> Maybe a
projectExt view env CBottom  = Nothing
projectExt view env (Raw v) = Just v
projectExt view env (CFaceted k priv pub) =
  if checkPolicy k env
    then projectExt view env priv
    else projectExt view env pub
  where checkPolicy k env = case Map.lookup k env of
                              Just(policy) -> case policy(view) of
                                                Raw b -> b
                                                anythingelse -> False -- TODO CHANGE!!
                              Nothing -> True

-- Private
visibleTo :: PC -> View -> Bool
visibleTo pc view = all consistent pc
  where consistent (Private k) = k `elem` view
        consistent (Public k)  = k `notElem` view

getView :: ExtView -> PolicyEnv -> View
getView extView env =
  let (assgns,_) = Map.mapAccumWithKey accumF [] env
      view = map (\(l,_) -> l) (filter (\(_,b) -> filterF b) assgns)
  in view
  where
  accumF :: [(Label,CFaceted Bool)] -> Label -> (ExtView -> CFaceted Bool) -> ([(Label,CFaceted Bool)],())
  accumF acc keyL policy = ((keyL,policy(extView)) : acc,())
  filterF :: CFaceted Bool -> Bool
  filterF (Raw b) = b
  filterF anythingElse = False -- TODO CHANGE!!

-- | Faceted IO
data FIO a = FIO { runFIO :: PC -> IO a }

instance Functor FIO where
  fmap = liftM

instance Applicative FIO where
  pure = return
  (<*>) = ap

-- | Monad is straightforward
instance Monad FIO where
  return x = FIO (\pc -> return x)
  x >>= f  = FIO (\pc -> do v <- runFIO x pc
                            runFIO (f v) pc)

