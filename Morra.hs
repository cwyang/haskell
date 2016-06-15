import System.IO
import System.Random
import Text.Printf
import Control.Monad.State
import Control.Monad.Trans.Maybe

data GameMode = P1 | P2 deriving Eq

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

crlf :: IO ()
crlf = putChar '\n'

getInput :: Int -> MaybeT IO Int
getInput playerno = do
  lift $ putStr ("Player " ++ show playerno ++ ": choose 1 or 2 (q to quit) > ")
  go
  where go = MaybeT $ do
          c <- getChar
          if c == '\n' then return () else crlf
          case c of
            '1' -> return $ Just 1
            '2' -> return $ Just 2
            'q' -> return Nothing
            '\n' -> runMaybeT go
            _ -> runMaybeT $ getInput playerno
getAiInput :: MaybeT IO Int
getAiInput = MaybeT $ (Just . fst . randomR (1,2) . mkStdGen) <$> randomIO

getInputs :: GameMode -> MaybeT IO (Int,Int)
getInputs mode = do
  x <- getInput 1
  y <- if mode == P1 then getAiInput
       else lift clearScreen >> getInput 2
  return (x,y)
  
{- 1st normal
doRound :: Int -> Int -> IO (Int, Int)
doRound round p1win = do
  v1 <- getInput 1
  case v1 of
    Nothing  -> return (round, p1win)
    Just v1' -> do
      v2 <- getAiInput
      putStrLn $ "Computer chooses " ++ show v2
      if odd (v1' + v2)
        then putStrLn "Player 1 wins this round.\n"
        else putStrLn "Computer wins this round.\n"
      doRound (round + 1) (p1win + if odd (v1'+v2) then 1 else 0)
-}  
-- StateT
doRound :: StateT (Int, Int, GameMode) IO ()
doRound = do
  (round, p1win, mode) <- get
  v <- lift . runMaybeT $ getInputs mode
  case v of
    Nothing  -> return ()
    Just (v1,v2) -> do
      lift $ showRoundResult v1 v2 mode
      put (round+1, p1win + if odd (v1+v2) then 1 else 0, mode)
      doRound
  where showRoundResult v1 v2 mode = do
          if mode == P1 then return ()
            else  putStrLn . printf "Player 1 chooses %d" $ v1
          putStrLn (printf "%s chooses %d" p2 v2)
          if odd (v1 + v2)
            then putStrLn "Player 1 wins this round.\n"
            else putStrLn . printf "%s wins this round.\n" $ p2
          where p2 = if mode == P1 then "Computer" else "Player 2"

morra :: Int -> IO ()
morra mode = do
  putStrLn "Let's play Morra!"
  putStrLn "Players choose 1 or 2."
  putStrLn "Player 1 wins when total sum of numbers is odd, and"
  putStrLn "player 2 wins when total sum of numbers is even."
  putStrLn "Let's begin!"

  (round, p1wins, _) <- (execStateT doRound) (0,0, if mode == 1 then P1 else P2)
  let p2wins = round - p1wins
      msg = case compare p1wins p2wins of
        GT -> printf "player 1 wins! (%d:%d)" p1wins p2wins
        LT -> printf "player 2 wins! (%d:%d)" p2wins p1wins
        EQ -> printf "draw! (%d:%d)" p1wins p2wins

  putStrLn msg

         
