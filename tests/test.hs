import Test.HUnit
import Faceted
import Control.Monad(liftM, join)

catF = do x <- (makePrivate "k" "abc")
          y <- (makePrivate "l" "def")
          return (x ++ y)

test0 = TestCase (assertEqual "for (1+2)," (3) (1+2))
test1 = TestCase (do s <- runFIO (evalStrF [] catF) []
                     assertEqual "for view with no labels" s (""))
test2 = TestCase (do s <- runFIO (evalStrF ["k","l"] catF) []
                     assertEqual "for view with k and l labels" s ("abcdef"))

e1 = makeFacets "l" 1 2
e1Test0 = TestCase (do i <- runFIO (evalIntF [] e1) []
                       assertEqual "<l ? 1 : 2>, view with no labels" i 2)
e1Test1 = TestCase (do i <- runFIO (evalIntF ["l"] e1) []
                       assertEqual "<l ? 1 : 2>, view with label l" i 1)
e1Test2 = TestCase (do i <- runFIO (evalIntF ["l1"] e1) []
                       assertEqual "<l ? 1 : 2>, view with label l1" i 2)

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

tests = TestList [TestLabel "test0" test0,
                  TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test 0 for e1" e1Test0,
                  TestLabel "test 1 for e1" e1Test1,
                  TestLabel "test 2 for e1" e1Test2,
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
                  TestLabel "test 8 for e4" e4Test8
                ]
