{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.FIO (
  FIO,
  runFIO,
  -- swap,
  -- primitive,
  evalStrF,
  evalIntF,
  evalStrP,
  evalIntP,
) where

import Faceted.Internal

import Control.Monad(liftM)

-- | With an empty context it is safe to run
secureRunFIO fio = runFIO fio []

{-prod :: (Eq a) => Faceted (FIO (Faceted a)) -> FIO (Faceted a)
prod ua = FIO f where
  f pc = g (run (runFaceted ua pc)) where
    g :: (Eq a) => CFaceted (FIO (Faceted a)) -> IO (Faceted a)
    g (Raw fio) = runFIO fio pc
    g (CFaceted k priv pub)
        | Private k `elem` pc = runFIO (prod (Prim priv)) pc
        | Public k  `elem` pc = runFIO (prod (Prim pub)) pc
        | otherwise           = do privV <- runFIO (prod (Prim priv)) (Private k : pc)
                                   pubV  <- runFIO (prod (Prim pub))  (Public k : pc)
                                   return (Prim(CFaceted k (run privV) (run pubV)))
    g CBottom = return Bottom

primitive :: FIO Int
primitive = FIO $ \pc ->
  let result | Private "H" `elem` pc = return 42
             | Public  "H" `elem` pc = return (-1)
             | otherwise             = return (-1)
  in result

swap :: (Eq a) => Faceted (FIO a) -> FIO (Faceted a)
swap = prod . liftM (liftM return)-}

evalStrF :: View -> Faceted [Char] -> FIO [Char]
evalStrF view ch = FIO f where
  f :: PC -> IO [Char]
  f pc | pc `visibleTo` view = case project view (runFaceted ch pc) of
                                 Just s -> return s
                                 Nothing -> return "" -- default value for strings
       | otherwise           = return ""

evalIntF :: View -> Faceted Int -> FIO Int
evalIntF view i = FIO f where
  f :: PC -> IO Int
  f pc | pc `visibleTo` view = case project view (runFaceted i pc) of
                                 Just v -> return v
                                 Nothing -> return 0 -- default value for ints
       | otherwise           = return 0

evalStrP :: ExtView -> PolicyEnv -> Faceted [Char] -> FIO [Char]
evalStrP view env ch =
  let intView = getView view env -- internal version of external view
      f :: PC -> IO [Char]
      f pc | pc `visibleTo` intView = -- PC matches internal version of external view
              case projectExt view env (runCFaceted (run ch) pc) of
                Just v -> return v -- got value for this external view and PC
                Nothing -> return "" -- default value for strings, got Bottom
           | otherwise              = return "" -- PC doesn't match internal version of external view
  in FIO f

evalIntP :: ExtView -> PolicyEnv -> Faceted Int -> FIO Int
evalIntP view env i =
  let intView = getView view env -- internal version of external view
      f :: PC -> IO Int
      f pc | pc `visibleTo` intView = -- PC matches internal version of external view
              case projectExt view env (runCFaceted (run i) pc) of
                Just v -> return v -- got value for this external view and PC
                Nothing -> return 0 -- default value for ints, got Bottom
           | otherwise              = return 0 -- PC doesn't match internal version of external view
  in FIO f

{-evalBoolP :: ExtView -> PolicyEnv -> Faceted Bool -> FIO Bool
evalBoolP view env b =
  let intView = getView view env -- internal version of external view
      f :: PC -> IO Bool
      f pc | pc `visibleTo` intView = -- PC matches internal version of external view
              case projectExt view env (runFaceted b pc) of
                Just v -> return v -- got value for this external view and PC
                Nothing -> return False -- default value for bools, got Bottom
           | otherwise              = return False -- PC doesn't match internal version of external view
  in FIO f-}
