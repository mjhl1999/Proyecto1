module Main where

import Syntax
import Semantics
import Tableux
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import System.TimeIt
import Control.Monad

timeItTPure :: (a -> b) -> a -> IO (Double,a)
timeItTPure p a = timeItT $ p a `seq` return a

rInt :: Int -> Int -> Int
rInt l h = unsafePerformIO (randomRIO (l, h))

-- Devuelve una formula aleatoria con v variables y
-- c conectivos
randF :: Int -> Int -> Prop
randF v c = case c of
  0 -> V (rInt 1 v)
  _ -> case rInt 0 4 of
    0 -> Neg (randF v (c - 1))
    1 -> Conj (randF v l) (randF v (c - 1 - l))
    2 -> Disy (randF v l) (randF v (c - 1 - l))
    3 -> Imp (randF v l) (randF v (c - 1 - l))
    4 -> Equiv (randF v l) (randF v (c - 1 - l))
    where l = rInt 0 (c - 1)

solve :: Prop -> IO ()
solve phi = do
  putStrLn "Resolviendo:"
  print phi
  putStrLn "Semantica:"
  sem_t <- timeItTPure satisf phi
  print $ fst sem_t
  putStrLn "Tableux:"
  tab_t <- timeItTPure satisf_tab phi
  print $ fst tab_t
  putStrLn ""

main = do
  let randList = [randF i 30 | i <- [1, 2, 3, 5, 10, 20]]
  forM_ randList solve
