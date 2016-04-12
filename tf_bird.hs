-- Thinking Functionally With Haskell by R.Bird, excercies
module Main where
import Data.List


-- $setup
-- >>> import Test.QuickCheck

-- | Returns the head of the list or the given default.
-- >>> cp1 [[2],[1,3]]
-- [[2,1],[2,3]]

cp1 :: [[a]] -> [[a]]
cp1 [] = [[]]
cp1 (xs:xss) = [x:ys | x <- xs, ys <- yss]
  where yss = cp1 xss

foo :: [[Int]]
foo = [[1,2,3],[4,5]]

-- | Test
-- prop> cp foo == cp1 foo
-- prop> (length . cp) foo == ex6d1 foo
-- prop> (length . cp) foo == ex6d2 foo
-- prop> (length . cp) foo == ex6d3 foo

-- Tigerbook P.131 Cartesian product
cp :: [[a]] -> [[a]]
cp = foldr f [[]]
  where f xs acc= [ x:ys | x <- xs, ys <- acc ]

-- fusion: f. foldr g a = foldr h b
-- f is strict, f a = b
-- f (g x y) = h x (f y)
-- length (f x y) = h x (length y)
{-length . cp
  = length . foldr f [[]]
  = foldr h 1
    where h x acc = length x * acc
-}  

ex6d1 :: [[a]] -> Int
ex6d1 = foldr h 1
  where h x acc = length x * acc
-- product . map length
ex6d2 :: [[a]] -> Int
ex6d2 = product . foldr f []
  where f x acc = length x : acc
ex6d3 :: [[a]] -> Int
-- product (f x y) = h x (f y)
-- length x * y = h x y
ex6d3 = foldr h 1
  where h x y = length x * y

data Nat = Zero | Succ Nat
data NEList a = One a | Cons a (NEList a)
three :: Nat
three = Succ $ Succ $ Succ Zero
fourList :: NEList Int
fourList = Cons 1 $ Cons 2 $ Cons 3 $ One 4
  
-- | Test
-- prop> foldNat (+1) 0 three == 3
-- >>> foldNEList (+) id fourList
-- 10

foldNat :: (a->a) -> a -> Nat -> a
foldNat f e Zero = e
foldNat f e (Succ n) = foldNat f (f e) n

foldNEList :: (a->b->b) -> (a->b) -> NEList a -> b
foldNEList f g (One a) = g a
foldNEList f g (Cons a as) = f a (foldNEList f g as)

-- | Test
-- prop> takePrefix (all even) x == takeWhile even x

takePrefix :: ([a] -> Bool) -> [a] -> [a]
takePrefix p = foldr1 (\x acc -> acc) . filter p . inits

-- none .f == none
-- map f . none == none
-- map f . one = one . f
one x = [x]
none x = []
-- fst . fork (f,g) == f
-- snd . fork (f,g) == g
-- fork (f,g) . h   == fork (f.h,g.h)
fork (f,g) x = (f x, g x)
-- test p (f,g) . h == test (p . h) (f . h, g . h)
-- h . test p (f,g) == test p (h . f, h . g)
test p (f,g) x = if p x then f x else g x
{-
filter1 p = concat . map (test p (one, none))

filter1 p = map fst . filter snd . map (fork (id,p))
          = map fst . concat . map (test snd (one, none)) . map (fork (id,p))
          = concat . map (map fst . test snd (one,none) . fork (id,p))
          = concat . map (map fst . test (snd . fork (id,p)) (one . fork (id,p), none . fork(id.p)))
          = concat . map (map fst . test p (one . fork (id,p), none))
          = concat . map (test p (map fst . one . fork (id, p), none))
          = concat . map (test p (one . fst . fork (id, p), none))
          = concat . map (test p (one . id, none))
          = concat . map (test p (one, none))
map (fork (f,g)) = uncurry zip . fork (map f, map g)

takePrefix p = last . filter p . inits
takePrefix (p . foldl f e)
  = last . filter (p . foldl f e)  . inits
-}


-- Thinking Functionally with Haskell by R. Bird
-- Chapter 7.2 Controlling space

sumlen1 = foldl' g (0,0)
  where g (s,n) x = (s+x,n+1)

sumlen2 = foldl' g (0,0)
  where g (s,n) x = s `seq` n `seq` (s+x,n+1)

mean1 [] = 0
mean1 xs = s / fromIntegral n
  where (s,n) = sumlen1 xs        

mean2 [] = 0
mean2 xs = s / fromIntegral n
  where (s,n) = sumlen2 xs        

main = print $ mean1 [1..1000000]

-- chapter 8 pretty printting

type Layout = String

pretty :: Int -> Doc -> Layout
layouts :: Doc -> [Layout]
(<>) :: Doc -> Doc -> Doc
nil :: Doc
text :: String -> Doc
line :: Doc
nest :: Int -> Doc -> Doc
group :: Doc -> Doc
