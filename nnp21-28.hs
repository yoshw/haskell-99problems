module NinetyNineProblems where

import System.Random
import Data.List (sortOn)

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt elt list n = f list 1
  where f [] _ = error "'n' greater than list length"
        f (x:xs) count
          | count == n = elt:x:xs
          | count < n  = x:(f xs (count+1))
          | otherwise  = error "'n' less than 1"

-- 22
range :: Integer -> Integer -> [Integer]
range mini maxi
  | mini > maxi = []
  | otherwise   = mini:(range (mini+1) maxi)


-- 23
rnd_select :: [a] -> Int -> [a]
rnd_select [] 0 = []
rnd_select [] _ = error "'n' is greater than list length"
rnd_select list n = f list n (mkStdGen 0)
  where f xs m g
          | m < 0  = error "'n' is negative"
          | m == 0 = []
          | otherwise =
              let (indx,ng) = randomR (0, (length xs)-1) g
                  elt       = head $ drop indx xs
                  remainder = (take indx xs) ++ (tail $ drop indx xs)
                  elts      = f remainder (m-1) ng
              in (elt:elts)

-- 24
diff_select :: Int -> Int -> [Int]
diff_select n maxi = rnd_select [1..maxi] n

-- 25
rnd_permu :: [a] -> [a]
rnd_permu xs = rnd_select xs (length xs)


-- the above, but now impure (using global stdGen)
rnd_select2 :: [a] -> Int -> IO [a]
rnd_select2 [] 0 = return []
rnd_select2 [] _ = error "'n' is greater than list length"
rnd_select2 list n = f list n
  where f xs m
          | m < 0  = error "'n' is negative"
          | m == 0 = return []
          | otherwise = do
              g <- newStdGen
              let (indx,_)  = randomR (0, (length xs)-1) g
              let elt       = head $ drop indx xs
              let remainder = (take indx xs) ++ (tail $ drop indx xs)
              elts <- f remainder (m-1)
              return (elt:elts)

diff_select2 :: Int -> Int -> IO [Int]
diff_select2 n maxi = rnd_select2 [1..maxi] n

rnd_permu2 :: [a] -> IO [a]
rnd_permu2 xs = rnd_select2 xs (length xs)

-- 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x:xs)
  | n > 0 = map (x:) (combinations (n-1) xs) ++ combinations n xs
  | otherwise = error "'n' is negative"

-- 27

-- 28
lsort :: [[a]] -> [[a]]
lsort list = sortOn length list
