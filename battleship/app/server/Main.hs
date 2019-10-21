{-# LANGUAGE ScopedTypeVariables #-}

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
  args            <- getArgs
  (port, players) <- case args of port:players:[] -> return (port, read players :: Int)
                                  _               -> do
                                                       putStrLn "Incorrect Arguments"
                                                       putStrLn "Expected <port>"
                                                       exitFailure
  addr <- resolve port
  bracket (open players addr) close (runServer)

resolve :: String -> IO AddrInfo
resolve port = do
 let hints = defaultHints { addrFlags = [AI_PASSIVE]
                          , addrSocketType = Stream
                          }
 addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
 return addr

open :: Int -> AddrInfo -> IO Socket
open players addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr players
    bind sock (addrAddress addr)
    listen sock players
    return sock

getNameAndShips :: Socket -> IO (String, [Ship])
getNameAndShips sock = do
  name :: String <- fmap decode $ recv sock 100
  sendAll sock (encode "ok")
  shipAns1 :: String <- fmap decode $ recv sock 10000
  let ships1 = read (shipAns1) :: [Ship]
  return (name, ships1)

runServer :: Socket -> IO ()
runServer sock = do
  (p1Sock, _) <- accept sock
  (p2Sock, _) <- accept sock
  (name1, ships1) <- getNameAndShips p1Sock
  (name2, ships2) <- getNameAndShips p2Sock
  putStrLn $ show ships1
  putStrLn $ show ships2
  play (p1Sock, p2Sock) (name1, name2) [initField, initField] [ships1, ships2] [ships1, ships2]

-- Play the game, one turn at a time
-- Input:
--    name1, name2: Players' names
--    fields: List of fields belonging to the players
--    oldShips: List of input ships belonging to the player
--    ships:  List of ships belonging to the player
-- The first element in the tuple, is from the player whose turn it currently is
play :: (Socket, Socket) -> (String, String) -> [Field] -> [[Ship]] -> [[Ship]] -> IO ()
play (sock1, sock2) (name1, name2) fields oldShips ships =
  do
    putStrLn ("\n" ++ name1 ++ "'s turn")
    sendAll sock1 (encode "Your turn")

    sendAll sock1 (encode . show $ (name2, (last fields, last ships), (last oldShips), (head ships)))

    ans <- fmap decode $ recv sock1 20000
    let (newField, newShipList) = read (ans) :: (Field, [Ship])
    if length newShipList == 0 then
      do
        putStrLn ("\n🏆  " ++ name1 ++ " won!🎉\n")
        printField name2 newField (last oldShips)
        printField name1 (head fields) (head oldShips)
    else
        play (sock2, sock1) (name2, name1) [newField, head fields] [last oldShips, head oldShips] [newShipList, head ships]
