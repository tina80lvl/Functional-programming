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
  -- Check if ship was given enough coordinates
  | length ship /= shipLength = False
  -- The coordinates may not overlap with another ship
  | or [coord1 == coord2 |
    ship2 <- placedShips, coord1 <- ship, coord2 <- ship2] = False
  -- Check if coordinates lie in the field
  | not (and [validateCoordinate coord | coord <- ship]) = False
  -- Check if  coordinates are neighbors (vertical)
  | and (map (==0) [abs ((fst coord1) - (fst coord2)) |
    coord1 <- ship, coord2 <- ship])
      = (sum [abs ((snd coord1) - (snd coord2)) |
        coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) *
          (shipLength^2 + shipLength)
  -- Check if  coordinates are neighbors (horizontal)
  | and (map (==0) [abs ((snd coord1) - (snd coord2)) |
    coord1 <- ship, coord2 <- ship])
      = (sum [abs ((fst coord1) - (fst coord2)) |
        coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) *
          (shipLength^2 + shipLength)
  -- Coordinates are not on the same line
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
  putStrLn (take (fieldSize + 2) (repeat '+') ++ "\n+" ++
    convertFieldToString field ships (1, 1) ++ take (fieldSize + 1) (repeat '+'))
  putStrLn ""

markShot :: Field -> Int -> Int -> Field
markShot field x y = replace x field (replace y (select field x) True)

removeDestroyedShips :: [Ship] -> [Ship]
removeDestroyedShips [] = []
removeDestroyedShips (x:xs) | null x    = removeDestroyedShips xs
                            | otherwise = x : removeDestroyedShips xs

-- Check if the ship has been destroyed and remove it from the game when it is
-- Input:
--    field:      The field on which the ship is located
--    ship:       The ship that we should check the coordinate against
--    coordinate: The coordinate that is being shot at
-- Output:
--    Tuple of the ship that was given as input and a boolean that indicates if
-- the shot was a hit or miss.
--    When the ship is sunk, an empty list will be returned instead of the ship
-- that was given as input.
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

-- Fire a shot at a given coordinate
-- Input:
--    enemyField: The 10x10 field of the opponent
--    enemyShips: A list of all the opponent ships
--    coordinate: The position that we are shooting at
-- Output:
--    Tuple with the updated enemyField, enemyShips and a boolean to indicate a
-- hit or miss
fire :: (Field, [Ship]) -> Coordinate -> (Field, [Ship], Bool)
fire (enemyField, enemyShips) coordinate =
  (markShot enemyField (snd coordinate) (fst coordinate),
  removeDestroyedShips [fst (checkShipDestroyed enemyField ship coordinate) |
  ship <- enemyShips],
  or [snd (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips])

-- Fire at the opponent once for every ship you have left
-- Input:
--    enemyField: Current field of the opponent
--    enemyShips: The list of all ships from the opponent
--    oldShips: List of input ships belonging to the player
--    ourShips:   List of ship that we have left that can still fire
-- Output:
--    Tuple containing the updated field and ships of the opponent
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

-- Play the game, one turn at a time
-- Input:
--    name1, name2: Players' names
--    fields: List of fields belonging to the players
--    oldShips: List of input ships belonging to the player
--    ships:  List of ships belonging to the player
-- The first element in the tuple, is from the player whose turn it currently is
play :: (String, String) -> [Field] -> [[Ship]] -> [[Ship]] -> IO ()
play (name1, name2) fields oldShips ships =
  do
    putStrLn ("\n" ++ name1 ++ "'s turn")
    printField name2 (last fields) (last oldShips)
    (newField, newShipList) <-
      fireWithEveryShip name2 (last fields, last ships) (last oldShips) (head ships)
    if length newShipList == 0 then
      do
        putStrLn ("\n🏆  " ++ name1 ++ " won!🎉\n")
        printField name2 newField (last oldShips)
        printField name1 (head fields) (head oldShips)
    else
        play (name2, name1) [newField, head fields] [last oldShips, head oldShips] [newShipList, head ships]

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

inputNames :: IO (String,String)
inputNames = do
  putStrLn "⚠️  First player: "
  name1 <- getLine
  putStrLn "⚠️  Second player: "
  name2 <- getLine
  return (name1, name2)
