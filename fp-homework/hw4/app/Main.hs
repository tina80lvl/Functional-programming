module Main where

--import Lib
import Task1(multiply)

gen :: Int -> [Int]
gen 0 = []
gen 1 = [1]
gen i = 1 : (gen $ i - 1)

generate :: Int -> [[Int]]
generate 0 = []
generate 1 = [[1]]
generate n = let nxpn = (gen $ n - 1) : generate (n - 1) in
  map (1:) nxpn

main :: IO ()
main = do
  s <- getLine
  let m = generate $ read s
  print $ multiply m m
