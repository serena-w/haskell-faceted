module Example.Monad.SatInt
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
  x <- mkFreshIntVar "x"
  _0 <- mkInteger 0
  _2 <- mkInteger 2
  assert =<< mkAnd =<< T.sequence
    [ mkGt x _0, mkLt x _2
    ]
  fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalInt m) [x]
