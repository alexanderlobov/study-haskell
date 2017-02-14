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

escapeChar :: Parser Char
escapeChar =
    choice $ zipWith (\code replacement -> char code >> return replacement)
                     ['n',  'r',  't',  '\\', '"']
                     ['\n', '\r', '\t', '\\', '"']

stringChar :: Parser Char
stringChar = do
    char '\\' >> (char '"' <|> return '\\')
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

decNumber :: Parser Integer
decNumber = liftM read $ many1 digit

hexNumber :: Parser Integer
hexNumber = do
    s <- many1 hexDigit
    case readHex s of
        [(n, "")] -> return n
        _ -> fail "wrong hex number"

octNumber :: Parser Integer
octNumber = do
    s <- many1 octDigit
    case readOct s of
        [(n,"")] -> return n
        _ -> fail "wrong oct number"

readBin :: Num a => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

binNumber :: Parser Integer
binNumber = do
    s <- many1 $ oneOf "01"
    case readBin s of
        [(n,"")] -> return n
        _ -> fail "wrong bin number"

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
