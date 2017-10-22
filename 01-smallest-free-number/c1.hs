import Data.Array
import Data.Array.ST
import Data.List (partition, nub)

import System.Random
import Criterion.Main

minfree    :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

(\\)     :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist    :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n)
               [(i, True) | i <- xs, i < n]
--               (zip (filter (<=n) xs) (repeat True))
               where n = length xs

minfree2 = search . checklist

countlist    :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0,50) (zip xs (repeat 1))

sort xs = concat [replicate k x | (x,k) <- assocs . countlist $ xs ]

checklist'    :: [Int] -> Array Int Bool

               a <- newArray (0,n) False
               sequence_ [writeArray a x True | x <- xs, x<=n]
               return a
             where n = length xs 

minfree3 = search . checklist'

--minfrom      :: Int -> [Int]->Int
--minfrom a xs = head ([a..]\\xs)

minfree4 xs = minfrom 0 (length xs, xs)
minfrom a (n,xs) | n == 0    = a
                 | m == b-a  = minfrom b (n-m,vs)
                 | otherwise = minfrom a (m,us)
                   where (us, vs) = partition (<b) xs
                         b        = a + 1 + n `div` 2
                         m        = length us

main = defaultMain [
  bgroup "minfree" [
--      bench "specification" $ whnf minfree list
      bench "accumArray" $ whnf minfree2 list
    , bench "mutable ST" $ whnf minfree3 list
    , bench "divide and conq" $ whnf minfree4 list
                   ]
 ]
  where
    list = nub $ filter (/= (n - 444)) $ take (n*10) $ randomRs (0, n) stdGen
    n = 20000
    stdGen = mkStdGen 42


