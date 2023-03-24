module Main where

import Constants
import User_management
import Text.Read

-- This starts the game by asking for a depth and then running the game loop with the starting position
main :: IO ()
main = do
    putStrLn "Enter the depth (difficulty) of the engine you would like to play against:"
    engineDepthInput <- getLine
    let engineDepth = readMaybe engineDepthInput :: Maybe Int
    case engineDepthInput of
        "" -> moveLoop defaultEngineDepth startingChessPosition
        otherwise -> case engineDepth of
            Just depth -> moveLoop depth startingChessPosition
            Nothing -> main
        
    