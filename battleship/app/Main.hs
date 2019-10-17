module Main where

import Lib
import GameParams

main :: IO ()
main = do
  names <- inputNames

  putStrLn ("➡️  " ++ head names ++ ", enter your ships by coordinates\n")
  shipsPlayer1 <- inputShips minShipSize []

  putStrLn ("\n➡️  " ++ last names ++ ", enter your ships by coordinates\n")
  shipsPlayer2 <- inputShips minShipSize []

  play names [initField, initField] [shipsPlayer1, shipsPlayer2] [shipsPlayer1, shipsPlayer2]
