module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

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

simpleNumber :: Parser Integer
simpleNumber = liftM read $ many1 digit

hexNumber :: Parser Integer
hexNumber = do
    s <- many1 hexDigit
    case readHex s of
        [(n, "")] -> return n
        _ -> fail "wrong hex number"

numberWithRadixPrefix :: Parser Integer
numberWithRadixPrefix =
      (char 'd' >> simpleNumber)
  <|> (char 'x' >> hexNumber)

parseNumber :: Parser LispVal
parseNumber = (liftM (Number . read) $ many1 digit)
          <|> (liftM Number (char '#' >> numberWithRadixPrefix))

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
