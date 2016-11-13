module NinetyNineProblems where

-- problem 01
myLast :: [a] -> a
myLast []     = error "empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

-- 02
myButLast :: [a] -> a
myButLast []     = error "empty list"
myButLast [_]    = error "singleton list"
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

-- 03
elementAt :: Int -> [a] -> a
elementAt n = head . (drop $ n-1)

-- 04
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 05
myReverse :: [a] -> [a]
myReverse list = helper [] list
  where helper acc []     = acc
        helper acc (x:xs) = helper (x:acc) xs

-- 06
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

-- 07
data NestedList a = Elem a | List [NestedList a]
  deriving Show

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List nlists) = concatMap flatten nlists

-- 08
compress :: Eq a => [a] -> [a]
compress = foldr step []
  where step x [] = [x]
        step x list@(y:_) = if x == y then list
                            else x:list

-- 09
pack :: Eq a => [a] -> [[a]]
pack = foldr step []
  where step x [] = [[x]]
        step x list@(xs:xss) = if x == head xs
                               then ((x:xs):xss)
                               else [x]:list

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map helper . pack
  where helper list = (length list, head list)
