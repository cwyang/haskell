-- number composition
{-
fact :: Int -> Integer
fact = (factTable !!)
factTable :: [Integer]
factTable = 1:zipWith (*) [1..] factTable
choose :: Int -> Int -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * fromIntegral n `div` fromIntegral k
choose' :: Int -> Int -> Int
choose' n 0 = 1
choose' 0 k = 0
choose' n k = choose' (n-1) (k-1) * n `div` fromIntegral k
go :: [Int] -> Integer
go [n,k] = (choose (n-1) (k-1)) `rem` 1000000007
main = getLine >> (mapM_ (putStrLn . show . go .map (read :: String->Int).words) . lines =<< getContents)
-}
{-
-- Angels in Space
import Data.List

select :: Int -> [a] -> [[a]]
select 0 _ = [[]]
select _ [] = [[]]
select n (h:t) = filter ((==n) . length) $ (map (h:) (select (n-1) t)) ++ select n t

type Point = [Double]
angle :: [Point] -> Double
angle [[xa,ya,za],[xb,yb,zb],[xc,yc,zc]] =
  acos(vnx*wnx+vny*wny+vnz*wnz)
  where (vx,vy,vz) = (xa-xb,ya-yb,za-zb)
        (wx,wy,wz) = (xc-xb,yc-yb,zc-zb)
        vmag = sqrt(vx^2+vy^2+vz^2)
        (vnx,vny,vnz) = (vx/vmag,vy/vmag,vz/vmag)
        wmag = sqrt(wx^2+wy^2+wz^2)
        (wnx,wny,wnz) = (wx/wmag,wy/wmag,wz/wmag)
        
main = getLine >> (putStrLn . show . go . select 3 . conv . lines =<< getContents)
  where conv = map (map (read :: String->Double) . words)
        go l = (sum . map angle) l / (fromIntegral . length) l
-}
{-
-- Modular Roots
import Data.List

modularPow :: Int -> Int -> Int -> Int
modularPow _ _ 1 = 0
modularPow base exp modulus = foldl' (\acc x -> (acc * base) `mod` modulus) 1 [1..exp]

go :: Int-> [Int] -> String
go p [k,n] = if res == [] then "NONE"
             else unwords $ map show res
  where m = if n `mod` p < 0 then n `mod` p + p else n `mod` p
        res = filter (\x -> modularPow x k p == m) [0..p-1]
main = do
  [p,_] <- return . (map (read :: String -> Int) . words) =<< getLine
  mapM_ (putStrLn . go p . map read . words) . lines =<< getContents

-}

-- Maximum Values
sqrt6 = sqrt(6)
sqrt3 = sqrt(3)
sqrt2 = sqrt(2)
s :: Integer -> Double
s k = a^2 + 6*a*k' - b + 4*c
  where k' = fromInteger k
        x = sqrt(k'/3)
        y = sqrt(k'/2)
        z = sqrt(k'/6)
        a = 12*(x+z)*(y+z) - 6*(x+y)^2
        b = 96 * sqrt3 * x * y^2 * z
        c = (3*x*z+sqrt2*y^2)^2/2 + sqrt6*x*y - 1
go :: [Integer] -> Integer
go [l,r] = round(sum (map s [l..r])) `rem` 1000000007

main = getLine >> (mapM_ (putStrLn . show . go) . conv . lines =<< getContents)
  where conv = map (map (read :: String->Integer) . words)
