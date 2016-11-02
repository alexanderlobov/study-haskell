{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits
import Data.Maybe
import Data.Word8
import Data.Functor
import Data.List
import Data.Ord
import Control.Applicative
import qualified Data.Foldable as F

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalFileName modifiedFileName = do
    originalFile <- BS.readFile originalFileName
    modifiedFile <- BS.readFile modifiedFileName
    return $ BS.pack $ catMaybes $ getSecretHelper originalFile modifiedFile

getSecretHelper :: ByteString -> ByteString -> [Maybe Word8]
getSecretHelper s s' = BS.zipWith
                       (\x x' -> let xored = xor x x' in
                                 if xored == 0
                                 then Nothing
                                 else Just xored)
                       s s'


-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key outputFile = do
    let inputFile = outputFile ++ ".enc"
    input <- BS.readFile inputFile
    BS.writeFile outputFile $ decrypt key input

decrypt :: ByteString -> ByteString -> ByteString
decrypt key input = BS.pack $ BS.zipWith xor input (BS.cycle key)


-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filename = BS.readFile filename >>= return . decode

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsFile transactionsFile = do
    victims <- parseFile victimsFile
    transactions <- parseFile transactionsFile
    return $ filterBadTransactions <$> victims <*> transactions

filterBadTransactions :: [TId] -> [Transaction] -> [Transaction]
filterBadTransactions ids = filter (\t -> tid t `elem` ids)

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = F.foldl' getTransactionFlow Map.empty

getTransactionFlow :: Map String Integer -> Transaction -> Map String Integer
getTransactionFlow m t = Map.insertWith (-) (from t) (amount t) $
                         Map.insertWith (+) (to t) (amount t) m

-- Exercise 6 -----------------------------------------

getFlowMap :: IO (Map String Integer)
getFlowMap = do
    badTs <- getBadTs "data/victims.json" "data/transactions.json"
    return $ getFlow $ maybe [] id badTs

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldlWithKey' f ("", 0)
    where f = (\cur name amount -> if (snd cur) < amount then (name, amount) else cur)

-- Exercise 7 -----------------------------------------

type Payer = (String, Integer)
type Payee = (String, Integer)

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = let l = separatePeople m
                    payeers = sortPayers $ fst l
                    payees = sortPayees $ snd l
                in undoTsHelper payeers payees tids

undoTsHelper :: [Payer] -> [Payee] -> [TId] -> [Transaction]
undoTsHelper [] _ _ = []
undoTsHelper _ [] _ = []
undoTsHelper _ _ [] = []
undoTsHelper (payer:payers) (payee:payees) (tid:tids) =
    if amount == 0 then undoTsHelper payers' payees' tids
    else t : undoTsHelper payers' payees' tids
    where t = Transaction { from = fst payer
                          , to = fst payee
                          , amount = amount
                          , tid = tid
                          }
          payerAmount = snd payer
          payeeAmount = snd payee
          amount = min payerAmount (abs payeeAmount)
          payerResidual = payerAmount - amount
          payeeResidual = payeeAmount + amount
          payers' = if payerResidual == 0
                    then payers
                    else sortPayers $ (fst payer, payerResidual) : payers
          payees' = if payeeResidual == 0
                    then payees
                    else sortPayees $ (fst payee, payeeResidual) : payees

testUndoTs :: IO [Transaction]
testUndoTs = do
    m <- getFlowMap
    tids <- parseFile "data/new-ids.json"
    return $ undoTs m (maybe (repeat "") id tids)

separatePeople :: Map String Integer -> ([Payer], [Payee])
separatePeople = partition (\(_, amount) -> amount > 0) . Map.toList

sortPayers :: [Payer] -> [Payer]
sortPayers = sortBy (flip $ comparing snd)

sortPayees :: [Payee] -> [Payee]
sortPayees = sortBy (comparing $ snd)

testEx7 :: IO ()
testEx7 = do
    m <- getFlowMap
    let l = separatePeople m
    let payeers = sortPayers $ fst l
    let payees = sortPayees $ snd l
    putStrLn "Payeers"
    print payeers
    putStrLn "Payees"
    print payees
    return ()

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path x = BS.writeFile path $ encode x

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

