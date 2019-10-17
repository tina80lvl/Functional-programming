module Main where

import Lib
import GameParams

main :: IO ()
main = do
  (name1, name2) <- inputNames

  putStrLn ("➡️  " ++ name1 ++ ", enter your ships by coordinates\n")
  shipsPlayer1 <- inputShips minShipSize []

  putStrLn ("\n➡️  " ++ name2 ++ ", enter your ships by coordinates\n")
  shipsPlayer2 <- inputShips minShipSize []

  play (name1, name2) [initField, initField] [shipsPlayer1, shipsPlayer2] [shipsPlayer1, shipsPlayer2]
