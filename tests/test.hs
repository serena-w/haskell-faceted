import Test.HUnit
import Faceted
import Control.Monad(liftM, join)

catF = do x <- (makePrivate "k" "abc")
          y <- (makePrivate "l" "def")
          return (x ++ y)

test1 = TestCase (assertEqual "for (1+2)," (3) (1+2))
test2 = TestCase (do s <- runFIO (evalStrF [] catF) []
                     assertEqual "for view with no labels" s (""))
test3 = TestCase (do s <- runFIO (evalStrF ["k","l"] catF) []
                     assertEqual "for view with k and l labels" s ("abcdef"))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]
