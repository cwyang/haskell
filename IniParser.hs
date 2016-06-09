{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Ini where

import qualified Data.Aeson as A
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Char (isAlpha, ord)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta
import Text.Trifecta (Result)
import Data.Scientific (floatingOrInteger)
import Data.Maybe (fromMaybe)

headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair $ Header <$> some letter

assignmentEx :: ByteString
assignmentEx = "foo=123"

type Name = String
type Value' = String
type Assignments = Map Name Value'

parseAssignment :: Parser (Name, Value')
parseAssignment = do
  name <- some letter
  char '='
  val <- some (noneOf "\n")
  skipEOL -- important!
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

commentEx, commentEx' :: ByteString
commentEx = "; helloooo"
commentEx' = "; blah ; sdkjfsdkl ; sdkfjslkdfjlsdj"

skipComments :: Parser ()
skipComments =
  skipMany $ do
    char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

sectionEx, sectionEx' :: ByteString
sectionEx = [r|
; ignore me
# this also
[country]
cwyang=Korea
Curry=US
|]
sectionEx' = [r|
; comment
[section]
foo=123
bar=abcdef
[alsosection]
baz=helloo
|]

data Section = Section Header Assignments deriving (Eq, Show)
newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

tt = parseByteString parseIni mempty sectionEx'

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

ttt :: IO ()
ttt = hspec $ do
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
    let m = parseByteString parseAssignment mempty assignmentEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just ("foo", "123")
  describe "Header Parsing" $
    it "can parse a simple header" $ do
    let m = parseByteString parseHeader mempty headerEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")
  describe "Comment Parsing" $
    it "can parse a comment before a header" $ do
    let p = skipComments >> parseHeader
        i = "; foo\n[bar]"
        m = parseByteString p mempty i
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "bar")
  describe "INI parsing" $
    it "can parse ini file" $ do
    let m = parseByteString parseIni mempty sectionEx'
        r' = maybeSuccess m
        sectionV = M.fromList [ ("foo", "123")
                              , ("bar", "abcdef") ]
        alsoV = M.fromList [ ("baz", "helloo") ]
        expected' = Just (Config
                          (M.fromList
                           [ (Header "section", sectionV)
                           , (Header "alsosection", alsoV)]))
    print m
    r' `shouldBe` expected'

sectionJson :: LB.ByteString
sectionJson = [r|
{ "section": {"host": "ns.aratech.co.kr"},
  "content": {"favorite": "basketball"}
}
|]
data TestData = TestData { section :: Host
                         , content :: Content
                         } deriving (Eq, Show)
newtype Host = Host String deriving (Eq, Show)
type Annotation = String
data Content = Favorite Annotation
               | Memo Annotation
                 deriving (Eq, Show)

testJson = do
  let d = A.decode sectionJson :: Maybe TestData
  print d

instance A.FromJSON TestData where
  parseJSON (A.Object v) =
    TestData <$> v A..: "section"
    <*> v A..: "content"
  parseJSON _ = fail "expecting object for testdata"

instance A.FromJSON Host where
  parseJSON (A.Object v) =
    Host <$> v A..: "host"
  parseJSON _ = fail "expecting object for host"

instance A.FromJSON Content where
  parseJSON (A.Object v) =
    Favorite <$> v A..: "favorite"
    <|> Memo <$> v A..: "memo"
  parseJSON _ = fail "expecting object for Content"

ff = A.decode [r|{"memo": "good luck"}|] :: Maybe Content

data NumberOrString' = Numba Integer | Strong Text deriving (Eq, Show)
instance A.FromJSON NumberOrString' where
  parseJSON (A.Number i) =
    case floatingOrInteger i of
      Left _ -> fail "should be integral"
      Right i -> return $ Numba i
  parseJSON (A.String i) = return $ Strong i
  parseJSON _ = fail "error!!!"

-- SemVer http://semver.org

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)
type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)
instance Ord NumberOrString where
  NOSI _ <= NOSS _ = True
  NOSI a <= NOSI b = a <= b
  NOSS a <= NOSS b = a <= b
instance Ord SemVer where
  SemVer a b c d _ <= SemVer a' b' c' d' _
    | (a,b,c) /= (a',b',c') = (a,b,c) <= (a',b',c')
    | d' == []              = True
    | d  == []              = False
    | otherwise = d <= d'
instance Eq (Result SemVer) where
  Success a == Success a' = a == a'
  _ == _ = False
parseSemVer :: Parser SemVer
parseSemVer = SemVer <$> (integer <* dott) <*> (integer <* dott) <*> integer <*> rel <*> (meta <* eof)

dott :: Parser Char
dott = char '.'
identifier :: Parser String
identifier = some (satisfy (flip elem $ '-':['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']))
numOrStr :: Parser NumberOrString
numOrStr = NOSI <$> integer <|> NOSS <$> identifier
rel, meta :: Parser [NumberOrString]
parseEtc :: Char -> Parser [NumberOrString]
--parseEtc c = many ((dott <|> char c) >> numOrStr)
parseEtc c = (fromMaybe []) <$> optional (char c >> sepBy numOrStr dott)
parseEtc' c = optional (char c >> numOrStr) >>= \x -> case x of
  Nothing -> return []
  Just x  -> (x:) <$> many (dott >> numOrStr)
--((dott <|> char c) >> numOrStr)
rel = parseEtc '-'
meta = parseEtc '+'

fff, ffff :: SemVer
fff = SemVer 0 0 0 [] []
ffff = SemVer 1 0 0 [] []
f x = SemVer 1 0 0 x []

semverTest :: IO ()
semverTest = hspec $ do
  describe "parse test" $
    it "can parse" $ do
      let f = parseString parseSemVer mempty :: String -> Result SemVer
      f "2.1.1" `shouldBe` Success (SemVer 2 1 1 [] [])
      f "1.0.0-x.7.z.92" `shouldBe` Success (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
  describe "order test" $
    it "can order" $ do
    SemVer 2 1 1 [] [] > SemVer 2 1 0 [] [] `shouldBe` True
    (f [NOSS "alpha"] < f [NOSS "alpha", NOSI 1]) `shouldBe` True
    (f [NOSS "alpha", NOSI 1] < f [NOSS "alpha", NOSS "beta"]) `shouldBe` True
    (f [NOSS "alpha", NOSS "beta"] < f [NOSS "alpha", NOSS "beta", NOSI 2]) `shouldBe` True
    (f [NOSS "alpha", NOSS "beta", NOSI 2] < f [NOSS "alpha", NOSS "beta", NOSI 11]) `shouldBe` True
    (f [NOSS "alpha", NOSS "beta", NOSI 11] < f [NOSS "alpha", NOSS "rc", NOSI 1]) `shouldBe` True
    (f [NOSS "alpha", NOSS "rc", NOSI 1] < f []) `shouldBe` True
-- 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0

g :: Parser a -> String -> Result a
g x = parseString x mempty

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "parseDigit"
base10Integer, base10Integer' :: Parser Integer
base10Integer = do
  l <- some $ (\x -> ord x - ord '0') <$> parseDigit <?> "integer"
  return $ foldl (\acc x -> acc * 10 + fromIntegral x) 0 l
base10Integer' = do
  sign <- optional (char '-')
  case sign of
    Just _ ->  negate <$> base10Integer
    _      ->  optional (char '+') >> base10Integer

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
                   deriving (Eq, Show)

threeDigit, fourDigit :: Parser Int
threeDigit = read <$> count 3 digit
fourDigit = read <$> count 4 digit
d :: Parser Char
d = char '-'
form1, form2, form3, form4, parsePhone :: Parser PhoneNumber
form1 = PhoneNumber
  <$> threeDigit <* d
  <*> (try fourDigit <|> threeDigit) <* d
  <*> fourDigit
form2 = PhoneNumber <$> threeDigit <*> fourDigit <*> fourDigit
form2' = PhoneNumber <$> threeDigit <*> threeDigit <*> fourDigit
form3 = PhoneNumber
  <$> between (char '(') (char ')') threeDigit <* char ' '
  <*> (try fourDigit <|> threeDigit) <* d
  <*> fourDigit
form4 = some digit >> d >> form1
parsePhone = (form3 <|> try form1 <|> try form2 <|> try form2' <|> form4) <* eof
