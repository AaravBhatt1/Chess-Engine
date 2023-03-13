module User_management where

import Chess_engine
import Types
import Constants
import Data.List

-- This is the loop for each turn
moveLoop :: ChessPosition -> IO ()
moveLoop position = do
    userMove <- getUserMove White position
    if checkGameOver (newChessPos userMove)
        then do
            putStrLn "You won, congragulations!"
            return ()
        else do
            let moveTree = cutTree 4 $ generateMoveTree userMove
            let engineMove = getBestMove moveTree
            putStrLn "Engine move: "
            putStrLn $ prettifyMove engineMove
            if checkGameOver (newChessPos engineMove)
                then do
                    putStrLn "You lost, better luck next time."
                    return ()
                else moveLoop (newChessPos engineMove)
    
-- Checks if the user input is valid
checkUserInput :: String -> Color -> ChessPosition -> Maybe Move
checkUserInput text color chessPos = case index of
    Just i -> Just (possibleMoves !! i)
    Nothing -> Nothing
    where
        possibleMoves = getAllMoves color chessPos
        index = findIndex (== text) $ map prettifyMove possibleMoves

-- Asks for the user to type their move until they type in a valid move and then returns their move
getUserMove :: Color -> ChessPosition -> IO Move
getUserMove color position = do
    putStrLn "Enter a valid move: "
    userMove <- getLine
    case checkUserInput userMove color position of
        Just move -> return (move)
        Nothing -> do 
            putStrLn "Invalid move"
            getUserMove color position

-- This returns the position as a string as the coordinates of the board in chess notation
prettifyPosition :: PositionVector -> String
prettifyPosition (PositionVector x y) = "abcdefgh" !! x : show (y + 1)

-- This returns the move in a string of how it would be displayed to the user
prettifyMove :: Move -> String
prettifyMove (Move oldPiece newPiece newChessPos) = "Move " ++ prettyType ++ " from " ++ prettyOldPos ++ " to " ++ prettyNewPos
    where
        prettyType = show $ pieceType oldPiece
        prettyOldPos = prettifyPosition $ position oldPiece
        prettyNewPos = prettifyPosition $ position newPiece
