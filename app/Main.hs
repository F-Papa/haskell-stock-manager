module Main (main) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Network.Socket
import System.Environment (getArgs)
import System.IO
import System.Random

type StockMap = M.Map ProductId ProductStock

type ProductId = Int

type Quantity = Int

data ProductStock = Stock {getReserved :: Quantity, getAvailable :: Quantity} deriving (Show)

productNotFound :: ProductStock
productNotFound = Stock 0 0

emptyStock :: StockMap
emptyStock = M.empty

main :: IO ()
main = do
  args <- getArgs
  let serverPort = parsePort (listToMaybe args)
  serverSock <- bindSockAndListen serverPort
  mutex <- newEmptyMVar
  stock <- readStockFromFile
  putMVar mutex stock
  putStrLn $ "Listening on: " ++ show serverPort
  serverLoop serverSock mutex
  where
    serverLoop serverSock mutex = do
      (clientSock, addr) <- accept serverSock
      putStrLn $ "Connected to: " ++ show addr
      _ <- forkIO (handleClient (show addr) clientSock mutex)
      serverLoop serverSock mutex

bindSockAndListen :: PortNumber -> IO Socket
bindSockAndListen port = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet port 0)
  listen sock backlog
  return sock
  where
    backlog = 5

parsePort :: Maybe String -> PortNumber
parsePort = maybe 3000 read

readStockFromFile :: IO StockMap
readStockFromFile = do
  fHandle <- openFile "data/stock.txt" ReadMode
  addStockFromLine emptyStock fHandle
  where
    addStockFromLine :: StockMap -> Handle -> IO StockMap
    addStockFromLine stockMap fHandle = do
      result <- try (hGetLine fHandle) :: IO (Either SomeException String)
      case result of
        Left _ -> return stockMap
        Right line ->
          let (p, q) = parseLine line
           in addStockFromLine (M.insert p q stockMap) fHandle

parseLine :: String -> (ProductId, ProductStock)
parseLine l = fmap mkStruct (splitOnComma l)
  where
    mkStruct :: Quantity -> ProductStock
    mkStruct x = Stock {getReserved = 0, getAvailable = x}

splitOnComma :: String -> (ProductId, Quantity)
splitOnComma l = (read pId, read quantity)
  where
    pId = take sepIdx l
    quantity = drop (sepIdx + 1) l
    sepIdx = fromJust (elemIndex ',' l)

handleClient :: String -> Socket -> MVar StockMap -> IO ()
handleClient clientId sock mutex = do
  sockHandle <- socketToHandle sock ReadWriteMode
  void $ runExceptT (clientLoop sockHandle)
  putStrLn ("Client " ++ show clientId ++ " has disconnected.")
  where
    clientLoop :: Handle -> ExceptT SomeException IO String
    clientLoop sockHandle = do
      result <- ExceptT $ try (hGetLine sockHandle)
      let (pId, quant) = splitOnComma result
      lift $ print (pId, quant)
      lift $ takeReservation clientId mutex pId quant
      clientLoop sockHandle

takeReservation :: String -> MVar StockMap -> ProductId -> Quantity -> IO ()
takeReservation cId mutex prod quant = do
  stock <- takeMVar mutex
  case reserveStock prod quant stock of
    Just updatedStock -> do
      notifyResSuccess cId quant prod
      putMVar mutex updatedStock
      void $ forkIO (executePayment mutex prod quant)
    Nothing -> do
      notifyResFailed cId quant prod
      putMVar mutex stock

reserveStock :: ProductId -> Quantity -> StockMap -> Maybe StockMap
reserveStock prod quant stock =
  if avail >= quant
    then Just $ M.insert prod result stock
    else Nothing
  where
    current = fromMaybe productNotFound (M.lookup prod stock)
    avail = getAvailable current
    resvd = getReserved current
    result = current {getAvailable = avail - quant, getReserved = resvd + quant}

sellReservedStock :: ProductId -> Quantity -> StockMap -> StockMap
sellReservedStock prod quant stock = M.insert prod result stock
  where
    current = fromJust $ M.lookup prod stock
    result = current {getReserved = getReserved current - quant}

releaseStock :: ProductId -> Quantity -> StockMap -> StockMap
releaseStock prod quant stock = M.insert prod result stock
  where
    current = fromJust $ M.lookup prod stock
    result =
      current
        { getAvailable = getAvailable current + quant,
          getReserved = getReserved current - quant
        }

notifyResSuccess :: String -> Quantity -> ProductId -> IO ()
notifyResSuccess cId quant prod =
  putStrLn $ "[" ++ show cId ++ "] Reserved " ++ show quant ++ " of " ++ show prod

notifyResFailed :: String -> Quantity -> ProductId -> IO ()
notifyResFailed clientId quant prod =
  putStrLn $ "[" ++ clientId ++ "] Insufficient stock for reserving " ++ show quant ++ " of " ++ show prod

notifyTransResult :: Quantity -> ProductId -> String -> IO ()
notifyTransResult quant prod result =
  putStrLn $
    "Transaction of "
      ++ show quant
      ++ " of "
      ++ show prod
      ++ ": "
      ++ result

executePayment :: MVar StockMap -> ProductId -> Quantity -> IO ()
executePayment mutex prod quant = do
  threadDelay 1000000
  num <- randomRIO (0, 1) :: IO Float
  stock <- takeMVar mutex
  if num >= 0.5
    then do
      notifyTransResult quant prod "SUCCESS"
      putMVar mutex (sellReservedStock prod quant stock)
    else do
      notifyTransResult quant prod "FAILED"
      putMVar mutex (releaseStock prod quant stock)