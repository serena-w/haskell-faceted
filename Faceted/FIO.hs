{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.FIO (
  FIO,
  runFIO,
  swap,
  primitive,
  evalStrF,
  evalIntF,
) where

import Faceted.Internal

import Control.Monad(liftM)

-- | With an empty context it is safe to run
secureRunFIO fio = runFIO fio []

prod :: Faceted (FIO (Faceted a)) -> FIO (Faceted a)
prod ua = FIO f where
  f pc = g (runFaceted ua pc) where
    g (Raw fio) = runFIO fio pc
    g (Faceted k priv pub)
        | Private k `elem` pc = runFIO (prod priv) pc
        | Public k  `elem` pc = runFIO (prod pub) pc
        | otherwise           = do privV <- runFIO (prod priv) (Private k : pc)
                                   pubV  <- runFIO (prod pub)  (Public k : pc)
                                   return (Faceted k privV pubV)
    g Bottom = return Bottom

primitive :: FIO Int
primitive = FIO $ \pc ->
  let result | Private "H" `elem` pc = return 42
             | Public  "H" `elem` pc = return (-1)
             | otherwise             = return (-1)
  in result

swap :: Faceted (FIO a) -> FIO (Faceted a)
swap = prod . liftM (liftM return)

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

