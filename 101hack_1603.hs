foo:: Int
foo=3
{-
foo :: Integer -> Integer -> Integer
foo m n = bar m n k
  where k = floor $ fromInteger m ** (1 / fromInteger n)
bar :: Integer -> Integer -> Integer -> Integer
bar m n k
  | m == 0    = 1
  | k == 0    = 0
  | k^n > m   = bar m n (k-1)
  | otherwise = bar (m - k^n) n (k-1) + bar m n (k-1)

main = do
  x <- readLn :: IO Integer
  n <- readLn :: IO Integer

main = do
    [n, nr_mango] <- readInts
    appetite <- readInts
    happiness <- readInts
    print $ maxMango 0 n n nr_mango appetite happiness
    where readInts = fmap (map (read :: String->Int) . words) getLine

main = do
    n <- readLn :: IO Int
    l <- fmap (sortByDescending . map readIntBs . B.words) B.getLine
    nr_t <- readLn :: IO Int
    t <- fmap( sortBy tupleValComp. zip [1..] . map (read :: String->Int) . lines) getContents
    mapM_ (\(_, cnt) -> print cnt) . sortBy tupleIdComp $ scanList 0 0 t l

main = do
    n <- readLn :: IO Int
    inputdata <- getContents
    (mapM_ (putStrLn . checkPreorder . convToInt) . splitEvery 2 . lines) inputdata

main = do
    inputdata <- B.getContents
    let (n:pp:m:qq:others) = B.lines inputdata
        p = map readIntBs. B.words $ pp
        q = map readIntBs. B.words $ qq
        startArray = (head p, array (0,100) [(i,(0,0)) | i <- [0..100]] )
    putStrLn . unwords . map show . sort $ difference startArray p q

memoized_fib :: Int -> Int -> Int -> Integer
memoized_fib a b = (map fib [0 ..] !!)
   where
         fib 1 = fromIntegral a
         fib 2 = fromIntegral b
         fib n = memoized_fib a b (n-2) + ((memoized_fib a b (n-1)) ^ 2)

main = do
    s <- getLine
    let [m,n,s] = map (read :: String->Int) $ words s
    print $ memoized_fib a b n

go :: String -> String
go s = let [m,n,s] = map (read :: String->Int) $ word s
       in show s

main = do
    n <- readLn :: IO Int
    inputdata <- getContents
    (mapM_ (putStrLn . go) . lines) inputdata



go :: String -> String
go str = let [n,m,s] = map (read :: String->Int) $ words str
             v = (m + (s-1)) `mod` n
                      in show (if v == 0 then n else v)

                         main = do
      n <- readLn :: IO Int
          inputdata <- getContents
            (mapM_ (putStrLn . go) . lines) inputdata
-}


check :: Ord a => [a] -> Bool
check (x:y:z:[]) = if x < z && y < z then True else False
select (x:y:z:[]) = if r == [] then [x,y,z] else head r
  where r = filter check [[x,y,z], [y,z,x], [z,x,y]]


gogo :: (Num a, Ord a) => [a] -> Bool
gogo l = check (select (a:b:c:[]))
  where go 0 x = x
        go n x = go (n - 1) (sort x)
        (a:b:c:_) = go (length l) l

sort :: (Num a, Ord a) => [a] -> [a]
sort (x:y:z:t) = a:(sort (b:c:t))
  where (a:b:c:_) = select (x:y:z:[])
sort x = x

solve str = if gogo l then "YES" else "NO"
  where l = map (read :: String->Int) $ words str
