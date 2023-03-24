module User_management where

import Chess_engine
import Chess_rules
import Constants
import Types
import Data.List
import Data.Char

-- This is the loop for each turn
moveLoop :: Int -> ChessPosition -> IO ()
moveLoop engineDepth chessPosition = do
    userMove <- getUserMove White chessPosition
    let chessPos = newChessPos userMove
    if checkGameOver (chessPos)
        then do
            putStrLn "You won, congragulations!"
            return ()
        else do
            let adjustedEngineDepth = if (isEndgame Black chessPos && isEndgame White chessPos) then engineDepth + engineDepthEndgameIncrement else engineDepth
            let moveTree = cutTree adjustedEngineDepth $ generateMoveTree userMove
            let engineMove = getBestMove moveTree
            putStrLn "Engine move: "
            putStrLn $ prettifyMove engineMove
            if checkGameOver (newChessPos engineMove)
                then do
                    putStrLn "You lost, better luck next time."
                    return ()
                else moveLoop engineDepth (newChessPos engineMove)
    
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
getUserMove color chessPosition = do
    putStrLn "Enter a valid move: "
    userMove <- getLine
    case checkUserInput userMove color chessPosition of
        Just move -> return (move)
        Nothing -> do 
            putStrLn "Invalid move"
            getUserMove color chessPosition

-- This returns the position as a string as the coordinates of the board in chess notation
prettifyPosition :: Vector -> String
prettifyPosition (Vector x y) = "abcdefgh" !! x : show (y + 1)

-- This returns the move in a string of how it would be displayed to the user
prettifyMove :: Move -> String
prettifyMove (Move oldPiece newPiece newChessPos) = "Move " ++ prettyType ++ " from " ++ prettyOldPos ++ " to " ++ prettyNewPos
    where
        prettyType = map toLower $ show $ pieceType oldPiece
        prettyOldPos = prettifyPosition $ piecePosition oldPiece
        prettyNewPos = prettifyPosition $ piecePosition newPiece
