import Faceted
import Control.Monad(liftM, join)
import System.IO(IOMode(WriteMode), IOMode(AppendMode))

prod = liftM join . swap

catF = do x <- (makePrivate "k" "abc")
          y <- (makePrivate "l" "def")
          return (x ++ y)

main1 = flip runFIO [] $ do
  h <- openFileF [] "output.txt" WriteMode
  hPutStrF h (makePublic "Test 1: view with no labels\n")
  hPutStrF h catF
  hCloseF h

main2 = flip runFIO [] $ do
  h <- openFileF ["k"] "output.txt" AppendMode
  hPutStrF h (makePublic "\nTest 2: view with label \"k\"\n")
  hPutStrF h catF
  hCloseF h

main3 = flip runFIO [] $ do
  h <- openFileF ["l"] "output.txt" AppendMode
  hPutStrF h (makePublic "\nTest 3: view with label \"l\"\n")
  hPutStrF h catF
  hCloseF h

main4 = flip runFIO [] $ do
  h <- openFileF ["k","l"] "output.txt" AppendMode
  hPutStrF h (makePublic "\nTest 4: view with labels \"k\" and \"l\"\n")
  hPutStrF h catF
  hCloseF h

addF = do x <- (makePrivate "k" (24 :: Int))
          y <- (makePrivate "l" (18 :: Int))
          return (x + y)

main5 = flip runFIO [] $ do
  h <- openFileF ["k","l"] "output.txt" AppendMode
  hPutStrF h (makePublic "\nTest 5: view with labels \"k\" and \"l\"\n")
  hPutIntF h addF
  hCloseF h

e3 = do x <- (makeFacets "l" 1 10)
        y <- (makeFacets "l" 100 1000)
        return (x + y)

main6 = flip runFIO [] $ do
  h <- openFileF [] "output.txt" AppendMode
  hPutStrF h (makePublic "\nTest 6: view with no labels\n")
  hPutIntF h e3
  hCloseF h

main = do
  main1
  main2
  main3
  main4
  main5
  main6
