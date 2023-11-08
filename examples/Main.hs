module Main where

import Foreign.RSDD

main :: IO ()
main = do
  mgr <- roBddBuilderDefaultOrder 0
  (l, a) <- newVar mgr True
  printBdd a >>= putStrLn
  b <- newBddPtr mgr False
  printBdd b >>= putStrLn
  let t = ptrTrue mgr
  ite mgr a b t >>= printBdd >>= putStrLn
  ite mgr t t t >>= printBdd >>= putStrLn
  let wmc = newWmc
  setWeight wmc l 0.2 0.8
  print $ varWeight wmc l
