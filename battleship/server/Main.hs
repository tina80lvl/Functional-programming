module Main where

import Lib
import GameParams

main :: IO ()
main = do
  mySocket <- createSocket 5555

  player1 <- acceptPlayer mySocket
  putStrLn ("Player 1 is connected! Name: " ++ (snd player1))

  player2 <- acceptPlayer mySocket
  putStrLn ("Player 2 is connected! Name: " ++ (snd player2))



  putStrLn ("➡️  " ++ name1 ++ ", enter your ships by coordinates\n")
  shipsPlayer1 <- inputShips minShipSize []

  putStrLn ("\n➡️  " ++ name2 ++ ", enter your ships by coordinates\n")
  shipsPlayer2 <- inputShips minShipSize []

  play (player1, snd player2) [initField, initField] [shipsPlayer1, shipsPlayer2] [shipsPlayer1, shipsPlayer2]


createSocket :: PortNumber -> IO Socket
createSocket port = do
  let hints = defaultHints {
    addrFlags = [AI_PASSIVE],
    addrSocketType = Stream
  }
  addr : _ <- getAddrInfo (Just hints) Nothing (Just $ show port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 1
  return sock

acceptPlayer :: Socket -> IO (MySocket, String)
acceptPlayer sock = untilJust do
  newSock <- fst <$> accept sock
  mySock <- mySocket newSock
  recvString mySock >>= \case
    Nothing -> return Nothing
    Just name -> Just (mySock, name)
