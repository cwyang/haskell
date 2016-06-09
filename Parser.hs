{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Text.Parsec (parse)
import Data.Ratio ((%))
import Text.Trifecta
import Text.Parser.Combinators
import Data.String (IsString)

stop :: Parser Char
stop = unexpected "stop"

one = string "1234" <* eof
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

oneTwoThree = string' "123"
string' :: [Char] -> Parser [Char]
string' [c] = sequence [char c]
string' cs = traverse char cs
  
eofKeepVal :: a -> Parser a
eofKeepVal x = eof >> return x

testParse :: Show a => Parser a -> IO ()
testParse p =
  print $ parseString p mempty "1234"

pNL s = putStrLn ('\n':s)
test = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoThree:"
  testParse oneTwoThree

badFraction, alsoBad, shouldWork, shouldAlsoWork :: IsString s=> s
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "divide by zero"
    _ -> return (numerator % denominator)

type FracOrInt = Either Integer Rational

parseFraction' :: Parser FracOrInt
parseFraction' =
  try (Right <$> parseFraction) <|>
  (Left <$> integer)

testFra = do
  -- parseString is Trifecta
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction
  -- parseOnly is Attoparsec
  print $ parseOnly parseFraction shouldWork
  print $ parseOnly parseFraction shouldAlsoWork
  print $ parseOnly parseFraction alsoBad
  print $ parseOnly parseFraction badFraction

p' :: Parser [Integer]
p' = some $ do
  i <- token digit
  return (read [i])
f x y = parseString x mempty  y

oP :: (Monad m, CharParsing m) => m Char
oP = string "ha" <|> string "hi" >> char '1'
oP' = try (string "ha") <|> string "hi" >> char '1'
foo = do
  putStrLn "Parsec"
  print $ parse oP mempty ("ha1" :: String)
  print $ parse oP mempty ("hi1" :: String)
  putStrLn "AttoParsec"
  print $ parseOnly oP ("ha1" :: IsString s => s)
  print $ parseOnly oP ("hi1" :: IsString s => s)
  putStrLn "Trifecta"
  print $ parseString oP mempty ("ha1" :: String)
  print $ parseString oP mempty ("hi1" :: String)
  putStrLn "Parsec with try"
  print $ parse oP' mempty ("ha1" :: String)
  print $ parse oP' mempty ("hi1" :: String)
  
