module Main where

import Lib

fieldSize = 10
minShipSize = 2
maxShipSize = 5

main :: IO ()
main = do
  names <- inputNames

  putStrLn ("🚢  " ++ head names ++ ", enter your ships by coordinates (x,y)")
  shipsPlayer1 <- inputShips minShipSize []

  putStrLn ("\n🚢  " ++ last names ++ ", enter your ships by coordinates (x,y)")
  shipsPlayer2 <- inputShips minShipSize []

  play names [initField, initField] [shipsPlayer1, shipsPlayer2] [shipsPlayer1, shipsPlayer2]
