module Example.Monad.SatBool
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

script :: Z3 (Maybe [Bool])
script = do
  x <- mkFreshBoolVar "x"
  _true <- mkBool True
  assert =<< mkAnd =<< T.sequence
    [ mkNot x ]
  fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalBool m) [x]

