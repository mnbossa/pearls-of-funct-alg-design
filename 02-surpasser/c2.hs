import System.Random
import Criterion.Main
import Test.QuickCheck

msc         :: Ord a => [a] -> Int
msc xs      = maximum [scount z zs | z:zs <- tails xs]
scount x xs = length (filter (x <) xs)

tails [] = []
tails (x:xs) = (x:xs): tails xs

table xs = [(z,scount z zs) | z:zs <- tails xs] 

msc' :: Ord a => [a] -> Int
msc' = maximum . map snd . table

table' [x] = [(x,0)]
table'  xs = join (m-n) (table' ys) (table' zs)
             where m       = length xs
                   n       = m `div` 2
                   (ys,zs) = splitAt n xs

join 0 txs [] = txs
join n [] tys = tys
join n txs@((x,c):txs') tys@((y,d):tys')
         | x<y  = (x,c+n):join n txs' tys
         | x>=y = (y,d):join (n-1) txs tys'


msc'' :: Ord a => [a] -> Int
msc'' =  maximum . map snd . table'

-- Test correctness ----------------------------------
testDivConq :: (NonEmptyList Int) -> Bool
testDivConq (NonEmpty xs) =  msc'' xs == msc xs

check = do
  quickCheckWith opts testDivConq
  where opts = stdArgs {maxSuccess = 1500}

-- Benchmarking ---------------------------------------
main = defaultMain [
  bgroup "surpass" [
      bench "specification" $ whnf msc list
    , bench "table" $ whnf msc' list
    , bench "divide and conq" $ whnf msc'' list
                   ]
 ]
  where
    list = take n $ randomRs (0, m) stdGen :: [Int]
    n = 2000
    m = 50
    stdGen = mkStdGen 42



