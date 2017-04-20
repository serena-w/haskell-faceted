module Example.Monad.NotSatInt
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
             Nothing  -> putStr "No solution found.\n"
             Just sol -> putStr "Solution: " >> print sol
  where opts = opt "MODEL" True

script :: Z3 (Maybe [Integer])
script = do
  x <- mkFreshIntVar "x"
  _2 <- mkInteger 2
  assert =<< mkAnd =<< T.sequence
    [ mkGt x _2, mkLt x _2  -- 1 <= q1 <= 4
    ]
  fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalInt m) [x]

