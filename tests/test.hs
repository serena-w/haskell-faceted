import Test.HUnit
import Faceted
import Control.Monad(liftM, join)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Exit as Exit

catF = do x <- (makePrivate "k" "abc")
          y <- (makePrivate "l" "def")
          return (x ++ y)

test0 = TestCase (assertEqual "for (1+2)," (3) (1+2))
test1 = TestCase (do s <- runFIO (evalStrF [] catF) []
                     assertEqual "for view with no labels" s (""))
test2 = TestCase (do s <- runFIO (evalStrF ["k","l"] catF) []
                     assertEqual "for view with k and l labels" s ("abcdef"))

e1 = makeFacets "l" 1 2
policyEnv1 = Map.fromList [("l",(\s -> s == "Alice"))]
policyEnv2 = Map.fromList [("l",(\s -> True))]
policyEnv3 = Map.fromList [("l",(\s -> False))]

e1Test0 = TestCase (do i <- runFIO (evalIntF [] e1) []
                       assertEqual "<l ? 1 : 2>, view with no labels" i 2)
e1Test1 = TestCase (do i <- runFIO (evalIntF ["l"] e1) []
                       assertEqual "<l ? 1 : 2>, view with label l" i 1)
e1Test2 = TestCase (do i <- runFIO (evalIntF ["l1"] e1) []
                       assertEqual "<l ? 1 : 2>, view with label l1" i 2)
e1TestP1 = TestCase (do i <- runFIO (evalIntP "Alice" policyEnv1 e1) []
                        assertEqual "<l ? 1 : 2>, viewing as Alice" i 1)
e1TestP2 = TestCase (do i <- runFIO (evalIntP "Bob" policyEnv1 e1) []
                        assertEqual "<l ? 1 : 2>, viewing as Bob" i 2)
e1TestP3 = TestCase (do i <- runFIO (evalIntP "Alice" policyEnv2 e1) []
                        assertEqual "<l ? 1 : 2>, viewing as Alice" i 1)
e1TestP4 = TestCase (do i <- runFIO (evalIntP "Bob" policyEnv2 e1) []
                        assertEqual "<l ? 1 : 2>, viewing as Bob" i 1)
e1TestP5 = TestCase (do i <- runFIO (evalIntP "Alice" policyEnv3 e1) []
                        assertEqual "<l ? 1 : 2>, viewing as Alice" i 2)
e1TestP6 = TestCase (do i <- runFIO (evalIntP "Bob" policyEnv3 e1) []
                        assertEqual "<l ? 1 : 2>, viewing as Bob" i 2)

e2 = makeFacets "l" (1-1) 2
e2Test0 = TestCase (do i <- runFIO (evalIntF [] e2) []
                       assertEqual "<l ? 1-1 : 2>, view with no labels" i 2)
e2Test1 = TestCase (do i <- runFIO (evalIntF ["l"] e2) []
                       assertEqual "<l ? 1-1 : 2>, view with label l" i 0)
e2Test2 = TestCase (do i <- runFIO (evalIntF ["l1"] e2) []
                       assertEqual "<l ? 1-1 : 2>, view with label l1" i 2)

e3 = do x <- (makeFacets "l" 1 10)
        y <- (makeFacets "l" 100 1000)
        return (x + y)
e3Test0 = TestCase (do i <- runFIO (evalIntF [] e3) []
                       assertEqual "<l ? 1 : 10> + <l ? 100 : 1000>, view with no labels" i 1010)
e3Test1 = TestCase (do i <- runFIO (evalIntF ["l"] e3) []
                       assertEqual "<l ? 1 : 10> + <l ? 100 : 1000>, view with label l" i 101)
e3Test2 = TestCase (do i <- runFIO (evalIntF ["l1"] e3) []
                       assertEqual "<l ? 1 : 10> + <l ? 100 : 1000>, view with label l1" i 1010)

e4 = do x <- (makeFacets "l1" 1 10)
        y <- (makeFacets "l2" 100 1000)
        return (x + y)
policyEnv4 = Map.fromList [("l1",(\s -> s == "Alice")),("l2",(\s -> s == "Alice"))]
policyEnv5 = Map.fromList [("l1",(\s -> s == "Alice")),("l2",(\s -> s == "Bob"))]
policyEnv6 = Map.fromList [("l1",(\s -> s == "Alice")),("l2",(\s -> s == "Bob")),("l3",(\s -> True))]
e4Test0 = TestCase (do i <- runFIO (evalIntF [] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with no labels" i 1010)
e4Test1 = TestCase (do i <- runFIO (evalIntF ["l1"] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with label l1" i 1001)
e4Test2 = TestCase (do i <- runFIO (evalIntF ["l2"] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with label l2" i 110)
e4Test3 = TestCase (do i <- runFIO (evalIntF ["l1","l2"] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with labels l1, l2" i 101)
e4Test4 = TestCase (do i <- runFIO (evalIntF ["l3","l4"] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with labels l3, l4" i 1010)
e4Test5 = TestCase (do i <- runFIO (evalIntF ["l2","l1"] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with labels l2, l1" i 101)
e4Test6 = TestCase (do i <- runFIO (evalIntF ["l3","l1"] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with label l3, l1" i 1001)
e4Test7 = TestCase (do i <- runFIO (evalIntF ["l3","l2"] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with label l3, l2" i 110)
e4Test8 = TestCase (do i <- runFIO (evalIntF ["l3","l2","l1","l4"] e4) []
                       assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, view with labels l3, l2, l1, l4" i 101)
e4TestP1 = TestCase (do i <- runFIO (evalIntP "Alice" policyEnv4 e4) []
                        assertEqual "<l1 ? 1 : 10> + <l2 ? 100 : 1000>, viewing as Alice" i 101)
e4TestP2 = TestCase (do i <- runFIO (evalIntP "Bob" policyEnv4 e4) []
                        assertEqual "<l ? 1 : 2>, viewing as Bob" i 1010)
e4TestP3 = TestCase (do i <- runFIO (evalIntP "Alice" policyEnv5 e4) []
                        assertEqual "<l ? 1 : 2>, viewing as Alice" i 1001)
e4TestP4 = TestCase (do i <- runFIO (evalIntP "Bob" policyEnv5 e4) []
                        assertEqual "<l ? 1 : 2>, viewing as Bob" i 110)

tests = TestList [TestLabel "test0" test0,
                  TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test 0 for e1" e1Test0,
                  TestLabel "test 1 for e1" e1Test1,
                  TestLabel "test 2 for e1" e1Test2,
                  TestLabel "policy env test 1 for e1" e1TestP1,
                  TestLabel "policy env test 2 for e1" e1TestP2,
                  TestLabel "policy env test 3 for e1" e1TestP3,
                  TestLabel "policy env test 4 for e1" e1TestP4,
                  TestLabel "policy env test 5 for e1" e1TestP5,
                  TestLabel "policy env test 6 for e1" e1TestP6,
                  TestLabel "test 0 for e2" e2Test0,
                  TestLabel "test 1 for e2" e2Test1,
                  TestLabel "test 2 for e2" e2Test2,
                  TestLabel "test 0 for e3" e3Test0,
                  TestLabel "test 1 for e3" e3Test1,
                  TestLabel "test 2 for e3" e3Test2,
                  TestLabel "test 0 for e4" e4Test0,
                  TestLabel "test 1 for e4" e4Test1,
                  TestLabel "test 2 for e4" e4Test2,
                  TestLabel "test 3 for e4" e4Test3,
                  TestLabel "test 4 for e4" e4Test4,
                  TestLabel "test 5 for e4" e4Test5,
                  TestLabel "test 6 for e4" e4Test6,
                  TestLabel "test 7 for e4" e4Test7,
                  TestLabel "test 8 for e4" e4Test8,
                  TestLabel "policy env test 1 for e4" e4TestP1,
                  TestLabel "policy env test 2 for e4" e4TestP2,
                  TestLabel "policy env test 3 for e4" e4TestP3,
                  TestLabel "policy env test 4 for e4" e4TestP4
                ]

main :: IO ()
main = do count <- runTestTT tests
          if failures count > 0 then Exit.exitFailure else return ()
