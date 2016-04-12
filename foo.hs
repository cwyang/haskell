{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.State
import System.Random hiding (next)

newtype Supply s a = S (State [s] a)
    deriving (Monad)
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply (S m) xs = runState m xs

randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)
