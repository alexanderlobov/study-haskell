module Scheme where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Char

-- import Data.List as L

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
    deriving Show

escapedChar :: Parser Char
escapedChar =
    choice $ zipWith (\code replacement -> char code >> return replacement)
                     ['n',  'r',  't',  '\\', '"']
                     ['\n', '\r', '\t', '\\', '"']

stringChar :: Parser Char
stringChar = do
    char '\\' >> (escapedChar <|> return '\\')
    <|> noneOf "\""

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many stringChar
    char '"'
    return $ String x

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom (first:rest)

parseBool :: Parser LispVal
parseBool =
    char '#' >>
    ((char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False)))

readBin :: Num a => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

readNumber :: (ReadS Integer) -> String -> String -> Parser Integer
readNumber reader name s =
    case reader s of
        [(n,"")] -> return n
        _ -> fail "wrong"

decNumber :: Parser Integer
decNumber = liftM read $ many1 digit

hexNumber :: Parser Integer
hexNumber = many1 hexDigit >>= readNumber readHex "hex"

octNumber :: Parser Integer
octNumber = many1 octDigit >>= readNumber readOct "oct"

binNumber :: Parser Integer
binNumber = (many1 $ oneOf "01") >>= readNumber readBin "bin"

numberWithRadixPrefix :: Parser Integer
numberWithRadixPrefix =
      (char 'd' >> decNumber)
  <|> (char 'x' >> hexNumber)
  <|> (char 'o' >> octNumber)
  <|> (char 'b' >> binNumber)

parseNumber :: Parser LispVal
parseNumber = (liftM (Number . read) $ many1 digit)
          <|> try (liftM Number (char '#' >> numberWithRadixPrefix))

parseCharacter :: Parser LispVal
parseCharacter = string "#\\" >>
     (   (try (string "space") >> return (Character ' '))
     <|> (try (string "newline") >> return (Character '\n'))
     <|> (anyChar >>= \c -> notFollowedBy alphaNum >> return (Character c))
     )

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseNumber
        <|> parseAtom
        <|> try parseBool
        <|> parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

