{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Exercises24 where

import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlpha, isDigit, digitToInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio ((%))
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Word
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta


one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

three :: Parser Char
three = char '3'

one' :: Parser a
one' = one >> stop

stop :: Parser a
stop = unexpected "stop"

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

testEOF :: Parser () -> IO ()
testEOF p = print $ parseString p mempty "123"

-- main :: IO ()
-- main = do
--   pNL "stop:"
--   testParse stop
--   pNL "one:"
--   testParse one
--   pNL "one':"
--   testParse one'
--   pNL "oneTwo:"
--   testParse oneTwo
--   pNL "oneTwo':"
--   testParse oneTwo'

-------------------------------------------
-- Exercises: Parsing Practice

-- 1:
main' :: IO ()
main' = do
  pNL "one:"
  testEOF $ one >> eof
  pNL "oneTwo:"
  testEOF $ oneTwo >> eof

-- 2:
ones :: Parser String
ones = string "1"

oneTwos :: Parser String
oneTwos = string "12"

oneTwoThrees :: Parser String
oneTwoThrees = string "123"

testParseS :: Parser String -> String -> IO ()
testParseS p s = print $ parseString p mempty s

test2 :: IO ()
test2 = do
  pNL "oneS 1:"
  testParseS ones "1"
  pNL "oneS 12:"
  testParseS ones "12"
  pNL "oneS 123:"
  testParseS ones "123"
  pNL "oneS choice:"
  testParseS (choice [ones, oneTwos, oneTwoThrees]) "123"

-- 3:
oneStr' :: Parser ()
oneStr' = one >> two >> three >> eof

test3 :: IO ()
test3 = do
  pNL "using char as string parser:"
  testEOF oneStr'

badFraction = "1/0"

alsoBad = "10"

shouldWork = "1/2"

shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

testFractions :: IO ()
testFractions = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' = parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork



-------------------------------------------
-- Exercise: Unit of Success

intParser :: Parser Integer
intParser = do
  n <- integer
  _ <- eof
  pure n

testIntParser :: IO ()
testIntParser = do
  print $ parseString intParser mempty "123"
  print $ parseString intParser mempty "123abc"


-------------------------------------------
-- Alternative:

type NumberOrString = Either Integer String

aa = "blah"
bb = "123"
cc = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

testAlternative :: IO ()
testAlternative = do
  let p f i = parseString f mempty i
  print $ p (some letter) aa
  print $ p integer bb
  print $ p parseNos aa
  print $ p parseNos bb
  print $ p (many parseNos) cc
  print $ p (some parseNos) cc



-------------------------------------------
-- Exercise: Try Try
type DecOrFract = Either Rational Integer

parseNum :: Parser DecOrFract
parseNum =
  skipMany (oneOf "\n") >>
  (Left <$> try parseFraction)
  <|>
  (Right <$> decimal)



-------------------------------------------
-- Parsing configuration files:

headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
  char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Val = String
type Assignments = Map Name Val

parseAssignment :: Parser (Name, Val)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

skipComments :: Parser ()
skipComments =
  skipMany $ do
      _ <- char ';' <|> char '#'
      skipMany (noneOf "\n")
      skipEOL

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' =
  [r|
  ; ignore me
  [states]
  Chris=Texas |]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Show, Eq)

skipWhiteSpace :: Parser ()
skipWhiteSpace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhiteSpace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

maybeSuccess :: Text.Trifecta.Result a -> Maybe a
maybeSuccess ( Text.Trifecta.Success a) = Just a
maybeSuccess _ = Nothing

testINI :: IO ()
testINI = hspec $ do
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
      let m  = parseByteString parseAssignment mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")

  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m  = parseByteString parseHeader mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Comment parsing" $
    it "Skips comment before header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
  describe "Section parsing" $
    it "can parse a simple section" $ do
      let m  = parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList [("Chris", "Texas")]
          expected' = Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'
  describe "INI parsing" $
    it "Can parse multiple sections" $ do
      let m =
            parseByteString parseIni
            mempty sectionEx''
          r' = maybeSuccess m
          sectionValues =
            M.fromList
            [ ("alias", "claw")
            , ("host", "wikipedia.org")
            ]
          whatisitValues =
            M.fromList
            [("red", "intoothandclaw")]
          expected' =
            Just (Config
                   (M.fromList
                    [ (Header "section", sectionValues)
                    , (Header "whatisit", whatisitValues)
                    ]))
      print m
      r' `shouldBe` expected'


-------------------------------------------
-- Marshalling and unmarshalling JSON data:

sectionJson :: ByteString
sectionJson =
  [r|
    { "section": {"host": "wikipedia.org"},
      "whatisit": {"red": "intoothandclaw"}
    }
  |]

data TestData = TestData
  { section :: Host
  , what    :: Color
  } deriving (Eq, Show)

newtype Host =
  Host String
  deriving (Eq, Show)

type Annotation = String

data Color
  = Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) =
    Host <$> v .: "host"
  parseJSON _ = fail "Exepected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
        (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Expected an object for Color"

data NumbersOrStrings
  = Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumbersOrStrings where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _) -> fail "Must be integral number"
      (Right int) -> return $ Numba int
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = fail "NumberOrString\
                     \ be number or string"

dec :: LBS.ByteString -> Maybe NumbersOrStrings
dec =  decode

eitherDec :: LBS.ByteString -> Either String NumbersOrStrings
eitherDec = eitherDecode


-------------------------------------------
-- Chapter Exercises:

-- 1:
data NumbaOrString =
    NOSS String
  | NOSI Integer

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumbaOrString]
type Metadata = [NumbaOrString]

data Semver = Semver Major Minor Patch Release Metadata

parseNoz :: Parser NumbaOrString
parseNoz =
      (NOSI <$> try (decimal <* notFollowedBy letter))
  <|> (NOSS <$> some (letter <|> digit))

parseRelease :: Parser NumbaOrString
parseRelease = skipMany (oneOf ".") >> parseNoz

parseSemver :: Parser Semver
parseSemver = Semver
  <$> decimal
  <*> (char '.' *> decimal)
  <*> (char '.' *> decimal)
  <*> (char '-' *> some parseRelease <|> mempty)
  <*> (char '-' *> some parseRelease <|> mempty)


-- 2:

parseDigit :: Parser Char
parseDigit = satisfy isDigit <?> "a number between 0 and 9."
-- parseDigit = oneOf "1234567890" <?> "a number between 0 and 9."

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

-- 3:
negs :: Parser Integer
negs = char '-' >> fmap negate base10Integer

base10Integer' :: Parser Integer
base10Integer' = negs <|> base10Integer

-- 4:
-- Phone Number
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

notDigit :: Parser Char
notDigit = satisfy (not . isDigit)

parsePhone :: Parser PhoneNumber
parsePhone = PhoneNumber <$> parseInt 3 <*> parseInt 3 <*> parseInt 4
  where
    getNum n = count n digit
    skipDash = skipMany (char '-')
    skipWhiteSpaces = skipMany (char ' ')
    skipParen = skipMany (char '(') *> skipMany (char ')')
    parseInt n = skipDash >> skipParen >> skipWhiteSpaces >> read <$> getNum n



-- 5:
logFile :: ByteString
logFile =
  [r|
    -- wheee a comment

    # 2025-02-05
    08:00 Breakfast
    09:00 Sanitizing moisture collector
    11:00 Exercising in high-grav gym
    12:00 Lunch
    13:00 Programming
    17:00 Commuting home in rover
    17:30 R&R
    19:00 Dinner
    21:00 Shower
    21:15 Read
    22:00 Sleep

    # 2025-02-07 -- dates not nececessarily sequential

    08:00 Breakfast -- should I try skippin bfast?
    09:00 Bumped head, passed out
    13:36 Wake up, headache
    13:37 Go to medbay
    13:40 Patch self up
    13:45 Commute home for rest
    14:15 Read
    21:00 Dinner
    21:15 Read
    22:00 Sleep”
  |]

subLog :: ByteString
subLog =
  [r|
    08:00 Breakfast
    09:00 Bumped head, passed out
    13:36 Wake up, headache
    13:37 Go to medbay
    13:40 Patch self up
    13:45 Commute home for rest
    14:15 Read
    21:00 Dinner
    21:15 Read
    22:00 Sleep”
  |]

timeLog :: ByteString
timeLog =
  [r|
    08:00
    09:00
    13:36
    13:37
    13:40
    13:45
    14:15
    21:00
    21:15
    22:00
|]

type Day = Integer
type Month = Integer
type Year = Integer
type Activity = String
type Hours = Integer
type Minutes = Integer

data Time =
  Time Hours Minutes deriving (Show, Eq)

data Entry = Entry Time Activity deriving (Eq, Show)

skipComment :: Parser ()
skipComment = whiteSpace *> skipMany (string "--") *> whiteSpace

parseTime :: Parser Time
parseTime = do
  hr  <- readDigits
  _   <- char ':'
  min <- readDigits
  pure $ Time hr min
  where
    readDigits :: Parser Integer
    readDigits = read <$> count 2 digit

parseActivity :: Parser Activity
parseActivity = do
  l <- some letter
  whiteSpace
  pure l

timeLogParser :: Parser Time
timeLogParser = do
  skipEOL
  whiteSpace
  t <- parseTime
  skipMany parseActivity
  whiteSpace
  skipEOL
  pure t

parseLogTime :: Parser [Time]
parseLogTime = do
  t <- many timeLogParser
  parseActivity
  pure t

parseTimeLog :: Parser [Time]
parseTimeLog = do
  skipEOL
  whiteSpace
  t <- some parseTime
  whiteSpace
  skipEOL
  pure t

getHours :: Time -> Float
getHours (Time h m) = fromInteger h + (fromInteger m / 60)


-- 6:
data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parseIPList :: Parser [Integer]
parseIPList = many $ try (integer <* char '.') <|> (integer <* eof)

parseIPAddress :: Parser IPAddress
parseIPAddress =
  (IPAddress .
   fromIntegral . sum . zipWith (*) (((2 ^ 8) ^) <$> [0 ..]) . reverse) <$>
  parseIPList


-- 7:
data IPAddress6 =
  IPAddress6 Word64
             Word64
  deriving (Eq, Ord, Show)

type Base16 = Char

type Byte = [Base16]

byteToInt :: Byte -> Int
byteToInt []     = 0
byteToInt (h:hs) = digitToInt h * 16 ^ length hs + byteToInt hs

blockToInteger :: [Byte] -> Integer
blockToInteger [] = 0
blockToInteger (b:bs) =
  (fromIntegral $ byteToInt b) * (16 ^ 4) ^ (length bs) + blockToInteger bs

blockToW64 :: [Byte] -> Word64
blockToW64 [] = 0
blockToW64 (b:bs) =
  (fromIntegral $ byteToInt b) * (16 ^ 4) ^ (length bs) + blockToW64 bs

splitBlocks :: [Byte] -> ([Byte], [Byte])
splitBlocks = splitAt 4

blocksToIP6 :: ([Byte], [Byte]) -> IPAddress6
blocksToIP6 (b1, b2) = IPAddress6 (blockToW64 b1) (blockToW64 b2)

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  bytes <- many $ try ((many hexDigit) <* char ':') <|> ((some hexDigit) <* eof)
  return $ blocksToIP6 $ splitBlocks $ mkFull bytes

mkFull :: [Byte] -> [Byte]
mkFull xs
  | length xs == 8 = xs
  | otherwise = go xs []
  where
    go (y:ys) acc
      | length y == 0 = mkFull (acc ++ (y : "" : ys))
      | otherwise = go ys (acc ++ [y])

ipEx1 :: String
ipEx1 = "0:0:0:0:0:ffff:ac10:fe01"

ipEx2 :: String
ipEx2 = "0:0:0:0:0:ffff:cc78:f"

ipEx3 :: String
ipEx3 = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"

ipEx3' :: String
ipEx3' = "FE80::0202:B3FF:FE1E:8329"

ipEx4 :: String
ipEx4 = "2001:DB8::8:800:200C:417A"
