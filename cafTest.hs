module Main where
import Debug.Trace

cafA :: [Integer]
cafA = trace "hi" (map (+1) $ [1..])
noCafB :: a -> [Integer]
noCafB _ = trace "hi" (map (+1) $ [1..])
noCafC :: a -> [Integer]
noCafC _ = trace "hi" (map (+1) $ [1..])
{-# NOINLINE noCafC #-}
noCafD :: a -> [Integer]
noCafD _ = trace "hi" (map (+1) $ myEnumFrom 0 1)
{-# NOINLINE noCafD #-}
myEnumFrom :: a -> Integer -> [Integer]
myEnumFrom _ n =  enumFrom n
{-# NOINLINE myEnumFrom #-}
    
main :: IO ()
main = do
  putStrLn "cafA"
  print $ (cafA !! 1 + cafA !! 2)
  putStrLn "noCafB"
  print $ (noCafB 0 !! 1 + noCafB 0 !! 2)
  putStrLn "noCafC"
  print $ (noCafC 0 !! 1 + noCafC 0 !! 2)
  putStrLn "noCafD"
  print $ (noCafD 0 !! 1 + noCafD 0 !! 2)
  
