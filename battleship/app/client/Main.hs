module Main where

import Lib
import GameParams
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Data.Binary (decode, encode)
import Control.Exception
import System.Environment
import System.Exit

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  (ip, port) <- case args of ip:port:[] -> return (ip, port)
                             _          -> do
                                             putStrLn "Incorrect Arguments"
                                             putStrLn "Expected <ip> <port>"
                                             exitFailure

  addr <- resolve ip port
  bracket (open addr) close runClient

resolve :: String -> String -> IO AddrInfo
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addrInfo:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addrInfo

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  return sock

runClient :: Socket -> IO ()
runClient sock = do
  putStrLn "Enter name"
  pname <- getLine

  putStrLn "Enter ships"
  ships <- inputShips minShipSize []
  sendAll sock (encode pname)

  ans <- fmap decode $ recv sock 20000
  putStrLn ans
  case ans of
    "ok" -> sendAll sock (encode . show $ ships)
    _    -> undefined

  goPlay sock

goPlay :: Socket -> IO ()
goPlay sock = do
  msg <- fmap decode $ recv sock 20000
  putStrLn msg

  (sock, name, (fieldsL, shipsL), oldShipsL, shipsH) <-
    fmap decode $ recv sock 20000
  (newField, newShipList) <-
    fireWithEveryShip sock name (fieldsL, shipsL) (oldShipsL) (shipsH)
  sendAll sock (encode . show $ (newField, newShipList))

  goPlay sock
