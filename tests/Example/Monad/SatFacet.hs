module Example.Monad.SatFacet
  ( run )
  where

import Control.Applicative
import Control.Monad ( join )
import Control.Monad.Trans (liftIO)
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad

run :: IO ()
run = evalZ3 script {-evalZ3With Nothing opts script >>= \mbSol ->
        case mbSol of
             Nothing  -> error "No solution found."
             Just sol -> putStr "Solution: " >> print sol
  where opts = opt "MODEL" True -}

script :: Z3 ()
script = do
  b1 <- mkFreshBoolVar "x"
  b2 <- mkFreshBoolVar "y"
  _1 <- mkInteger 1
  _10 <- mkInteger 10
  _11 <- mkInteger 11
  _100 <- mkInteger 100
  _101 <- mkInteger 101
  _110 <- mkInteger 110
  _1000 <- mkInteger 1000
  _1001 <- mkInteger 1001
  _1010 <- mkInteger 1010
  f1 <- mkIte b1 _1 _10
  f2 <- mkIte b2 _100 _1000
  fSum <- mkAdd [f1,f2]
  push
  assert =<< mkEq fSum _101
  (fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) [b1,b2]) >>= liftIO . (\mbSol ->
    case mbSol of
             Nothing  -> putStr "No solution found.\n"
             Just sol -> putStr "Solution: " >> print sol)
  pop 1
  push
  assert =<< mkEq fSum _11
  (fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) [b1,b2]) >>= liftIO . (\mbSol ->
    case mbSol of
             Nothing  -> putStr "No solution found.\n"
             Just sol -> putStr "Solution: " >> print sol)
  pop 1
  push
  assert =<< mkEq fSum _1001
  (fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) [b1,b2]) >>= liftIO . (\mbSol ->
    case mbSol of
             Nothing  -> putStr "No solution found.\n"
             Just sol -> putStr "Solution: " >> print sol)
  pop 1
  push
  assert =<< mkEq fSum _110
  (fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) [b1,b2]) >>= liftIO . (\mbSol ->
    case mbSol of
             Nothing  -> putStr "No solution found.\n"
             Just sol -> putStr "Solution: " >> print sol)
  pop 1
  push
  assert =<< mkEq fSum _1010
  (fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) [b1,b2]) >>= liftIO . (\mbSol ->
    case mbSol of
             Nothing  -> putStr "No solution found.\n"
             Just sol -> putStr "Solution: " >> print sol)
  pop 1
  push
  assert =<< mkAnd =<< T.sequence
    [ mkEq fSum _1010, mkEq fSum _1001
    ]
  (fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) [b1,b2]) >>= liftIO . (\mbSol ->
    case mbSol of
             Nothing  -> putStr "No solution found.\n"
             Just sol -> putStr "Solution: " >> print sol)
  pop 1
  push
  assert =<< mkAnd =<< T.sequence
    [ mkEq fSum _1001, mkNot b1
    ]
  (fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) [b1,b2]) >>= liftIO . (\mbSol ->
    case mbSol of
             Nothing  -> putStr "No solution found.\n"
             Just sol -> putStr "Solution: " >> print sol)
  pop 1
