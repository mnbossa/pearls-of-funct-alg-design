--import System.Random
import Criterion.Main
import Test.QuickCheck
import Data.List (sort)

--type Nat = Int
type Nat = Integer 
type FType = (Nat , Nat) -> Nat 
type InvType = FType -> Nat -> [(Nat,Nat)]

invert :: InvType
invert f z = [(x,y) | x<- [0..z], y<-[0..z], f (x,y) == z]

invert' :: InvType
invert' f z = [(x,y) | x<- [0..z], y<-[0..z-x], f (x,y) == z]

invert'' :: InvType
invert'' f z = find (0,z) f z

find :: (Nat,Nat) -> FType -> Nat -> [(Nat,Nat)]
find (u,v) f z
       | u > z || v<0 = []
       | z'<z         = find (u+1,v) f z
       | z'==z        = (u,v):find (u+1,v-1) f z
       | z'>z         = find (u,v-1) f z
         where z' = f (u,v)

find' :: Nat -> (Nat,Nat) -> FType -> Nat -> [(Nat,Nat)]
find' n (u,v) f z
       | u > n || v<0 = []
       | z'<z         = find (u+1,v) f z
       | z'==z        = (u,v):find (u+1,v-1) f z
       | z'>z         = find (u,v-1) f z
         where z' = f (u,v)

bsearch :: (Nat -> Nat) -> (Nat, Nat) -> Nat -> Nat
bsearch g (a, b) z
     | a+1 == b    = a
     | g m <= z    = bsearch g (m, b) z
     | otherwise   = bsearch g (a, m) z
       where m = (a + b) `div` 2

invert''' :: InvType
invert''' f z  =  find' n (0,m) f z 
  where m = bsearch (\y -> f (0,y)) (-1,z+1) z
        n = bsearch (\x -> f (x,0)) (-1,z+1) z
     -- m = maximum $ filter (\y -> f (0,y) <= z) [0..z]
     -- n = maximum $ filter (\x -> f (x,0) <= z) [0..z]

find'' :: (Nat,Nat) -> (Nat,Nat) -> FType -> Nat -> [(Nat,Nat)]
find'' (u,v) (r,s) f z
       | u>r || s>v   = []
       | v-s<=r-u     = rfind (bsearch (\x -> f (x,q)) (u-1, r+1) z )
       | otherwise    = cfind (bsearch (\y -> f (p,y)) (s-1, v+1) z )
    where
          p       = (u+r) `div` 2
          q       = (v+s) `div` 2
          rfind p' = (if f (p',q) == z then (p',q) : find'' (u,v) (p'-1,q+1) f z
                    else find'' (u,v) (p', q+1) f z) ++
                    find'' (p'+1, q-1) (r,s) f z 
          cfind q' = find'' (u,v) (p-1,q'+1) f z ++
                    (if f (p,q') == z then (p,q'): find'' (p+1,q'-1) (r,s) f z
                    else find'' (p+1,q') (r,s) f z)


invert'''' :: InvType
invert'''' f z  =  find'' (0,m) (n,0) f z 
  where m = bsearch (\y -> f (0,y)) (-1,z+1) z
        n = bsearch (\x -> f (x,0)) (-1,z+1) z

-- Test correctness ----------------------------------
f0 :: FType
f0 (x,y) = 2^y*(2*x+1)-1

f1 :: FType
f1 (x,y) = x*2^x + y*2^y+2*x+y

f2 :: FType
f2 (x,y) = 3*x+27*y+y*y

f3 :: FType
f3 (x,y) = x*x+y*y+x+y

f4 :: FType
f4 (x,y) = x+2^y+y-1

printf :: FType -> Nat -> IO ()
printf f maxz = mapM_ putStrLn [show [f (x,y) | y<-[0..maxz]] | x<-[maxz,maxz-1..0]]


printflevel :: FType -> Nat -> [Nat] -> IO ()  
printflevel f maxz ls = mapM_ putStrLn [line x | x<-[maxz,maxz-1..0]] where
    line w = show [level (f (w,y)) ls | y<-[0..maxz]]
    level z xs = length $ filter (z>) xs

testinv :: InvType -> Nat ->Bool
testinv inv x =  and [areEqual (inv f (abs x)) (invert f (abs x)) | f <- fs] where
        areEqual a b = sort a == sort b
        fs = [f0, f1, f2, f3, f4]

check :: IO ()
check = do
  quickCheckWith opts (testinv invert')
  quickCheckWith opts (testinv invert'')
  quickCheckWith opts (testinv invert''')
  quickCheckWith opts (testinv invert'''')
    where opts = stdArgs {maxSuccess = 150}

-- Benchmarking ---------------------------------------
main :: IO()
main = defaultMain [
--  bgroup "specification" 
--      [bench ("f"++(show i)) $ nf (invert     (fs!!i)) z | i<-[0..length fs-1]]
--  ,bgroup "diagonal" 
--      [bench ("f"++(show i)) $ nf (invert'    (fs!!i)) z | i<-[0..length fs-1]]
  bgroup "saddleback1" 
      [bench ("f"++(show i)) $ nf (invert''   (fs!!i)) z | i<-[0..length fs-1]]
  ,bgroup "saddleback2" 
      [bench ("f"++(show i)) $ nf (invert'''  (fs!!i)) z | i<-[0..length fs-1]]
  ,bgroup "div and conq" 
      [bench ("f"++(show i)) $ nf (invert'''' (fs!!i)) z | i<-[0..length fs-1]]
 ]
  where
    z = 1000
    fs = [f0, f1, f2, f3, f4]
