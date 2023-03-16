module Main where

import Constants
import User_management

-- This is the main function that runs when you run the program, it starts by running the game loop with the starting position
main :: IO ()
main = moveLoop startingBoard