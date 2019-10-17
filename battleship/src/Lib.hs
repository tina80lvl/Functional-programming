module Lib
    ( select
    , replace
    , initField
    , convertStringToCoordinates
    , splitCoordinatesInString
    , validateCoordinate
    , validateShipCoordinates
    , convertFieldToString
    , printField
    , markShot
    , removeDestroyedShips
    , checkShipDestroyed
    , fire
    , fireWithEveryShip
    , play
    , inputShip
    , inputShips
    , inputNames
    ) where

import Data.Char (ord)
import Data.List (permutations)

import GameParams

type Coordinate = (Int, Int)
type Ship = [Coordinate]
type Field = [[Bool]]
type Player = String

select :: [a] -> Int -> a
select xs n = head (drop (n - 1) (take n xs))

replace :: Int -> [a] -> a -> [a]
replace n xs x = take (n - 1) xs ++ [x] ++ drop n xs

initField :: Field
initField = take fieldSize (repeat (take fieldSize (repeat False)))

convertStringToCoordinates :: String -> Coordinate
convertStringToCoordinates ['(', x, ',', y, ')'] =
  ((ord x) - (ord '0') + 1, (ord y) - (ord '0') + 1)
convertStringToCoordinates _ = (-1, -1)

splitCoordinatesInString :: String -> [String]
splitCoordinatesInString [] = [[]]
splitCoordinatesInString (x:xs) =
  if x == ';' then
    [] : splitCoordinatesInString xs
  else
    (x : head (splitCoordinatesInString xs)) : tail (splitCoordinatesInString xs)

validateCoordinate :: Coordinate -> Bool
validateCoordinate coord =
  and [
    fst coord >= 1,
    snd coord >= 1,
    fst coord <= fieldSize,
    snd coord <= fieldSize
  ]

validateShipCoordinates :: [Ship] -> Ship -> Int -> Bool
validateShipCoordinates placedShips ship shipLength
    | length ship /= shipLength = False
    | or [coord1 == coord2 |
      ship2 <- placedShips, coord1 <- ship, coord2 <- ship2] = False
    | not (and [validateCoordinate coord | coord <- ship]) = False
    | and (map (==0) [abs ((fst coord1) - (fst coord2)) |
      coord1 <- ship, coord2 <- ship])
        = (sum [abs ((snd coord1) - (snd coord2)) |
          coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) *
            (shipLength ^ 2 + shipLength)
    | and (map (==0) [abs ((snd coord1) - (snd coord2)) |
      coord1 <- ship, coord2 <- ship])
        = (sum [abs ((fst coord1) - (fst coord2)) |
          coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) *
            (shipLength ^ 2 + shipLength)
    | otherwise = False

convertFieldToString :: Field -> [Ship] -> Coordinate -> String
convertFieldToString field ships coordinate
        | fst coordinate <= fieldSize
          && snd coordinate <= fieldSize =
            if select (select field (snd coordinate)) (fst coordinate) == True then
               if or [coordinate == coord | ship <- ships, coord <- ship] then
                 'x' : convertFieldToString field ships (fst coordinate + 1,
                                                         snd coordinate)
               else 'o' : convertFieldToString field ships (fst coordinate + 1,
                                                            snd coordinate)
            else ' ' : convertFieldToString field ships (fst coordinate + 1,
                                                         snd coordinate)

        | snd coordinate <= fieldSize = "+\n+" ++
          convertFieldToString field ships (1, snd coordinate + 1)
        | otherwise = []

printField :: String -> Field -> [Ship] -> IO ()
printField playerName field ships = do
  putStrLn (playerName ++ "'s field:")
  putStrLn (take (fieldSize+2) (repeat '+') ++ "\n+" ++
    convertFieldToString field ships (1, 1) ++ take (fieldSize+1) (repeat '+') )
  putStrLn ""

markShot :: Field -> Int -> Int -> Field
markShot field x y = replace x field (replace y (select field x) True)

removeDestroyedShips :: [Ship] -> [Ship]
removeDestroyedShips [] = []
removeDestroyedShips (x:xs) | null x    = removeDestroyedShips xs
                            | otherwise = x : removeDestroyedShips xs

--    field, ship, coordinate
checkShipDestroyed :: Field -> Ship -> Coordinate -> (Ship, Bool)
checkShipDestroyed field ship coordinate =
  if or [coordinate == coord | coord <- ship] == False then do
     (ship, False)
  else do
     if and [select (select field (snd coord)) (fst coord) == True |
       coord <- ship, coord /= coordinate] == False then
         (ship, True)
     else
         ([], True)

fire :: (Field, [Ship]) -> Coordinate -> (Field, [Ship], Bool)
fire (enemyField, enemyShips) coordinate =
  (markShot enemyField (snd coordinate) (fst coordinate),
  removeDestroyedShips [fst (checkShipDestroyed enemyField ship coordinate) |
  ship <- enemyShips],
  or [snd (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips])

fireWithEveryShip :: String -> (Field, [Ship]) -> [Ship] -> [Ship] -> IO (Field, [Ship])
fireWithEveryShip name (enemyField, enemyShips) oldShips [] =
  return (enemyField, enemyShips)
fireWithEveryShip name (enemyField, enemyShips) oldShips ourShips = do
  putStrLn ("🎯  Enter the coordinates to fire shot (" ++
    show (length ourShips) ++ " shots left)")
  string <- getLine
  let coord = convertStringToCoordinates string
  if validateCoordinate coord then
    do
      let (newEnemyField, newEnemyShips, hit) = fire (enemyField, enemyShips) coord

      if hit then
        putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++
          "," ++ show ((snd coord) - 1) ++ "), Hit ❌")
      else
        putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++
          "," ++ show ((snd coord) - 1) ++ "), Miss ⚪️")
      printField name newEnemyField oldShips

      if length newEnemyShips < length enemyShips then
        do
          putStrLn "💣  You sunk my battleship!"
          if length newEnemyShips == 0 then
            return (newEnemyField, newEnemyShips)
          else
            fireWithEveryShip name (newEnemyField, newEnemyShips) oldShips (tail ourShips)
      else
        fireWithEveryShip name (newEnemyField, newEnemyShips) oldShips (tail ourShips)
  else do
    putStrLn "❗️Correct format of coordinate: (x,y). Try again💪"
    fireWithEveryShip name (enemyField, enemyShips) oldShips ourShips

--    names, fields, ships
play :: [String] -> [Field] -> [[Ship]] -> [[Ship]] -> IO ()
play names fields oldShips ships =
  -- TODO play (name1, name2) fields ships =
  do
    putStrLn ("\n" ++ head names ++ "'s turn")
    printField (last names) (last fields) (last oldShips)
    (newField, newShipList) <-
      fireWithEveryShip (last names) (last fields, last ships) (last oldShips) (head ships)
    if length newShipList == 0 then
      do
        putStrLn ("\n🏆  " ++ head names ++ " won!🎉\n")
        printField (last names) newField (last oldShips)
        printField (head names) (head fields) (head oldShips)
    else
        play [last names, head names] [newField, head fields] [last oldShips, head oldShips] [newShipList, head ships]

-- readLines :: FilePath -> IO [String]
-- readLines = fmap lines . readFile

inputShip :: [Ship] -> Int -> IO Ship
inputShip placedShips len =
  do
    putStrLn ("🚢  Enter the coordinates of the ship of length " ++ show len)
    string <- getLine
    let stringCoords = splitCoordinatesInString string
    let coords = map convertStringToCoordinates stringCoords
    if validateShipCoordinates placedShips coords len then
      return coords
    else do
      putStrLn "❗️ Correct format of coordinates: (x1,y1);(x2,y2);... Try again💪"
      inputShip placedShips len

inputShips :: Int -> [Ship] -> IO [Ship]
inputShips shipSize placedShips =
  if shipSize <= maxShipSize then
      do
        ship <- inputShip placedShips shipSize
        shipList <- inputShips (shipSize + 1) (ship : placedShips)
        return (ship : shipList)
  else
      return []

inputNames :: IO [String]
inputNames = do
  putStrLn "⚠️  First player: "
  name1 <- getLine
  putStrLn "⚠️  Second player: "
  name2 <- getLine
  return [name1, name2]
