module Example.Monad.MultipleVars
  ( run )
  where

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad

run :: IO ()
run = evalZ3With Nothing opts script >>= \mbSol ->
        case mbSol of
             Nothing  -> error "No solution found."
             Just sol -> putStr "Solution: " >> print sol
  where opts = opt "MODEL" True

script :: Z3 (Maybe [Integer])
script = do
  x0 <- mkFreshIntVar "x0"
  x1 <- mkFreshIntVar "x1"
  _2 <- mkInteger 2
  assert =<< mkAnd =<< T.sequence
    [ mkGt x0 _2, mkLt x1 _2
    ]
  fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalInt m) [x0,x1]

