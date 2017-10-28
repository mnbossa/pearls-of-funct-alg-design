import Test.QuickCheck
import Data.List (sort, nub)
import System.Random
import Criterion.Main
import Data.Array
import Debug.Trace

-- specification
smallest            :: Ord a => Int -> ([a], [a]) -> a
smallest k (xs, ys) = union (xs, ys) !! k

union              :: Ord a => ([a],[a]) -> [a]
union (xs,[])      = xs
union ([],ys)      = ys
union (x:xs, y:ys) | x<y = x:union (  xs,y:ys)
                   | x>y = y:union (x:xs,  ys)

smallest'            :: Ord a => Int -> ([a], [a]) -> a
smallest' k ([], ws) = ws !! k
smallest' k (zs, []) = zs !! k
smallest' k (zs, ws) =
  case (a<b, k<=p+q) of
       (True , True ) -> smallest' k       (zs,us)
       (True , False) -> smallest' (k-p-1) (ys,ws)
       (False, True ) -> smallest' k       (xs,ws)
       (False, False) -> smallest' (k-q-1) (zs,vs)
  where p        = (length zs) `div` 2
        q        = (length ws) `div` 2
        (xs, a:ys) = splitAt p zs
        (us, b:vs) = splitAt q ws

 --(a, [(a,a,Int,Int, Int,Int, Int, Int, Int, Int)])

smallestp'            :: Ord a => Int -> ([a], [a]) -> [(a,a,Int,Int,Int)] -> (a,[(a,a,Int,Int, Int)])
smallestp' k ([], ws) ls = (ws !! k,ls)
smallestp' k (zs, []) ls = (zs !! k,ls)
smallestp' k (zs, ws) ls =
  case (a<b, k<=p+q) of
       (True , True ) -> smallestp' k       (zs,us) ((a,b,111,k,k):ls)
       (True , False) -> smallestp' (k-p-1) (ys,ws) ((a,b,222,k,k-p-1):ls)
       (False, True ) -> smallestp' k       (xs,ws) ((a,b,333,k,k):ls)
       (False, False) -> smallestp' (k-q-1) (zs,vs) ((a,b,444,k,k-q-1):ls)
  where p        = (length zs) `div` 2
        q        = (length ws) `div` 2
        (xs, a:ys) = splitAt p zs
        (us, b:vs) = splitAt q ws


smallest''            :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest'' k (xa, ya) = search k (0, m+1) (0,n+1) 
   where (0,m) = bounds xa
         (0,n) = bounds ya
         search k (lx, rx) (ly, ry)
           | lx == rx  = ya!(k+ly)
           | ly == ry  = xa!(k+lx)
           | otherwise = case (xa!mx < ya!my, (k+lx+ly)<= mx+my) of
                         (True , True ) -> search k           (lx  ,rx) (ly  ,my)
                         (True , False) -> search (k-mx+lx-1) (mx+1,rx) (ly  ,ry)
                         (False, True ) -> search k           (lx  ,mx) (ly  ,ry)
                         (False, False) -> search (k-my+ly-1) (lx  ,rx) (my+1,ry)
                         where mx = (lx+rx) `div` 2
                               my = (ly+ry) `div` 2

smallestp''            :: Ord a => Int -> (Array Int a, Array Int a) -> (a, [(a,a,Int,Int,Int, Int,Int, Int, Int, Int, Int)])
smallestp'' k (xa, ya) = search k (0, m+1) (0,n+1) [] 
   where (0,m) = bounds xa
         (0,n) = bounds ya
         search k (lx, rx) (ly, ry) ls
           | lx == rx  = (ya!(k+ly), (xa!0,ya!0,555,k,k+lx+ly,0,0,lx,rx,ly,ry):ls)
           | ly == ry  = (xa!(k+lx), (xa!0,ya!0,666,k,k+lx+ly,0,0,lx,rx,ly,ry):ls)
           | otherwise = case (xa!mx < ya!my, (k+lx+ly)<= mx+my) of
                         (True , True ) -> search k        (lx,rx) (ly,my) ((xa!mx, ya!my,111,k,k,mx,my, lx, rx, ly, my):ls)
                         (True , False) -> search (k-mx+lx-1) (mx+1,rx) (ly,ry) ((xa!mx, ya!my,222,k,k-mx-1, mx, my, mx, rx, ly, ry):ls)
                         (False, True ) -> search k        (lx,mx) (ly,ry) ((xa!mx, ya!my,333,k,k, mx, my, lx, mx, ly, ry):ls)
                         (False, False) -> search (k-my+ly-1) (lx,rx) (my+1,ry) ((xa!mx, ya!my,444,k,k-my-1, mx, my, lx, rx, my, ry):ls)
                         where mx = (lx+rx) `div` 2
                               my = (ly+ry) `div` 2


-- Test correctness ----------------------------------
testsmallest :: (Int -> ([Int], [Int]) -> Int) -> Int -> (NonEmptyList Int, NonEmptyList Int)  -> Bool
testsmallest f k (NonEmpty xs, NonEmpty ys) = f kk (xxs, yys) == smallest kk (xxs, yys)
        where xxs = sort $ nub xs
              yys = sort $ filter (not . (`elem` xxs)) ( nub ys )
              kk = max 0 (min k (length xxs + length yys - 1))

check :: IO ()
check = do
  quickCheckWith opts (testsmallest smallest' )
  quickCheckWith opts (testsmallest smallesta)
    where opts = stdArgs {maxSuccess = 2000}
          smallesta k (xs, ys) = smallest'' k (xa,ya)
             where xa = listArray (0, length xs - 1) xs
                   ya = listArray (0, length ys - 1) ys

main :: IO ()
main = do 
  defaultMain [
   bgroup "selection" [
      bench "specification"       $ nf (\x -> smallest   x (xs,ys)) k
      ,bench "div and conq lists" $ nf (\x -> smallest'  x (xs,ys)) k
      ,bench "div and conq array" $ nf (\x -> smallest'' x (xa,ya)) k
                      ]
   ]
  where xs = sort $ nub $ take 1000 $ randomRs (0, 5000 ) (mkStdGen 42) :: [Int]
        ys = sort $ filter (not . (`elem` xs)) $ nub $ take 1000 $ randomRs (0, 5000) (mkStdGen 10) :: [Int]
        k = 700 :: Int
        xa = listArray (0, length xs - 1) xs
        ya = listArray (0, length ys - 1) ys






