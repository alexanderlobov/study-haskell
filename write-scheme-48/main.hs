module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
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
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom atom

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

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseNumber
        <|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
