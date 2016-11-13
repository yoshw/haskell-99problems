module NinetyNineProblems where

-- 11
data RunLengthEncoding a = Single a | Multiple Int a
  deriving Show

encodeModified :: Eq a => [a] -> [RunLengthEncoding a]
encodeModified = map helper . pack
  where helper [x] = Single x
        helper list = Multiple (length list) (head list)

-- from earlier problem
pack :: Eq a => [a] -> [[a]]
pack = foldr step []
  where step x [] = [[x]]
        step x list@(xs:xss) = if x == head xs
                               then ((x:xs):xss)
                               else [x]:list

-- 12
decodeModified :: [RunLengthEncoding a] -> [a]
decodeModified = foldr step []
  where step (Single x)     list = x:list
        step (Multiple n x) list = (replicate n x) ++ list

-- 13
encodeDirect :: Eq a => [a] -> [RunLengthEncoding a]
encodeDirect = foldr step []
  where step x [] = [Single x]
        step x ((Single y):rest) = if x == y
                                   then (Multiple 2 x):rest
                                   else (Single x):(Single y):rest
        step x ((Multiple n y):rest) = if x == y
                                       then (Multiple (n+1) x):rest
                                       else (Single x):(Multiple n y):rest

-- 14
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- 15
repli :: [a] -> Int -> [a]
repli list n = concatMap (replicate n) list

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery list n = f list n 1
  where f []     _ _ = []
        f (x:xs) m count
          | count == m = f xs m 1
          | otherwise  = x:(f xs m (count+1))

-- 17
split :: [a] -> Int -> ([a], [a])
split list 0 = ([], list)
split list n = f list n 1
  where f [] _ _ = ([],[])
        f (x:xs) m count
          | count == m = ([x],xs)
          | otherwise = let (front, back) = f xs m (count+1)
                        in (x:front, back)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice list start end = f list 1
  where f [] _ = []
        f (x:xs) count
          | count < start = f xs (count+1)
          | count `elem` [start..end] = x:(f xs (count+1))
          | otherwise = []

-- 19
rotate :: [a] -> Int -> [a]
rotate xs n = drop n' xs ++ take n' xs
  where n' = n `mod` length xs

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = f xs 1
  where f [] _ = error "'n' greater than list length"
        f (y:ys) count
          | count == n = (y, ys)
          | count < n  = let (deleted, rest) = f ys (count+1)
                         in  (deleted, y:rest)
          | otherwise  = error "'n' less than 1"
