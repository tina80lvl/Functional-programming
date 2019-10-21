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
  -- putStrLn $ show players
  -- putStrLn $ show addr
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


-- inputShip :: Socket -> [Ship] -> Int -> IO Ship
-- inputShip sock placedShips len =
--   do
--     -- putStrLn ("🚢  Enter the coordinates of the ship of length " ++ show len)
--     -- string <- getLine
--     -- shipString <- fmap (read . decode) $ recv sock 200000
--     shipString :: String <- fmap decode $ recv sock 20000
--     let stringCoords = splitCoordinatesInString shipString
--     let coords = map convertStringToCoordinates stringCoords
--     if validateShipCoordinates placedShips coords len then
--       return coords
--     else do
--       -- putStrLn "❗️ Correct format of coordinates: (x1,y1);(x2,y2);... Try again💪"
--       Main.inputShip sock placedShips len
--
-- inputShips :: Socket -> Int -> [Ship] -> IO [Ship]
-- inputShips sock shipSize placedShips =
--   if shipSize <= maxShipSize then
--       do
--         ship <- Main.inputShip sock placedShips shipSize
--         shipList <- Main.inputShips sock (shipSize + 1) (ship : placedShips)
--         return (ship : shipList)
--   else
--       return []


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
  -- name1 <- fmap (read . decode) $ recv p1Sock 10000
  -- name1 <- fmap (read) (decode $ recv p1Sock 10000)
  (name1, ships1) <- getNameAndShips p1Sock
  (name2, ships2) <- getNameAndShips p2Sock
  putStrLn $ show ships1
  putStrLn $ show ships2
  play (p1Sock, p2Sock) (name1, name2) [initField, initField] [ships1, ships2] [ships1, ships2]


-- loop :: Socket -> Socket -> [Ship] -> [Ship] -> IO ()
-- loop (sock1 sock2 s1 s2 = undefined
-- send to first player "your turn"
-- recieve from first player his move

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

    sendAll sock1 (encode . show $ (sock1, name2, (last fields, last ships), (last oldShips), (head ships)))

    -- printField name2 (last fields) (last oldShips)
    (newField, newShipList) <- fmap decode $ recv sock 20000
    --   fireWithEveryShip sock1 name2 (last fields, last ships) (last oldShips) (head ships)
    if length newShipList == 0 then
      do
        putStrLn ("\n🏆  " ++ name1 ++ " won!🎉\n")
        printField name2 newField (last oldShips)
        printField name1 (head fields) (head oldShips)
    else
        play (sock2, sock1) (name2, name1) [newField, head fields] [last oldShips, head oldShips] [newShipList, head ships]


-- -- Fire at the opponent once for every ship you have left
-- -- Input:
-- --    enemyField: Current field of the opponent
-- --    enemyShips: The list of all ships from the opponent
-- --    oldShips: List of input ships belonging to the player
-- --    ourShips:   List of ship that we have left that can still fire
-- -- Output:
-- --    Tuple containing the updated field and ships of the opponent
-- fireWithEveryShip :: Socket -> String -> (Field, [Ship]) -> [Ship] -> [Ship] -> IO (Field, [Ship])
-- fireWithEveryShip sock name (enemyField, enemyShips) oldShips [] =
--   return (enemyField, enemyShips)
-- fireWithEveryShip sock name (enemyField, enemyShips) oldShips ourShips = do
--   putStrLn ("🎯  Enter the coordinates to fire shot (" ++
--     show (length ourShips) ++ " shots left)")
--   string <- getLine
--   let coord = convertStringToCoordinates string
--   if validateCoordinate coord then
--     do
--       let (newEnemyField, newEnemyShips, hit) = fire (enemyField, enemyShips) coord
--
--       if hit then
--         putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++
--           "," ++ show ((snd coord) - 1) ++ "), Hit ❌")
--       else
--         putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++
--           "," ++ show ((snd coord) - 1) ++ "), Miss ⚪️")
--       printField name newEnemyField oldShips
--
--       if length newEnemyShips < length enemyShips then
--         do
--           putStrLn "💣  You sunk my battleship!"
--           if length newEnemyShips == 0 then
--             return (newEnemyField, newEnemyShips)
--           else
--             fireWithEveryShip sock name (newEnemyField, newEnemyShips) oldShips (tail ourShips)
--       else
--         fireWithEveryShip sock name (newEnemyField, newEnemyShips) oldShips (tail ourShips)
--   else do
--     putStrLn "❗️Correct format of coordinate: (x,y). Try again💪"
--     fireWithEveryShip sock name (enemyField, enemyShips) oldShips ourShips
