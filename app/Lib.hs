module Lib where

import Types
import Constants
import Data.List
import Data.Maybe

-- Gets the number of points a piece is worth
getPieceValue :: Piece -> Int
getPieceValue piece = case pieceType piece of
    Pawn -> 1
    Knight -> 3
    Bishop -> 3
    Rook -> 5
    Queen -> 9
    King -> 40

-- Gets the other color
getOtherColor :: PieceColor -> PieceColor
getOtherColor color = case color of
    White -> Black
    Black -> White

-- Gets the total number of points a player has
getTotalPoints :: PieceColor -> [Piece] -> Int
getTotalPoints playerColor allPieces =  let
    playerPoints = sum (map (getPieceValue) (filter (\piece -> (pieceColor piece) == playerColor) allPieces))
    opponentPoints = sum (map (getPieceValue) (filter (\piece -> (pieceColor piece) /= playerColor) allPieces))
    in playerPoints - opponentPoints

-- Check if a piece is actually on the board
checkOnBoard :: Piece -> Bool
checkOnBoard (Piece _ _ x y _) = (x >= 0) && (y >= 0) && (x <= 7) && (y <= 7)

-- Moves a piece with a movement vector
movePiece :: Piece -> Int -> Movement -> Piece
movePiece (Piece pieceType pieceColor pieceX pieceY _) turn (Movement movementX movementY) = 
    Piece pieceType pieceColor (pieceX + movementX) (pieceY + movementY) turn

-- Checks if 2 pieces have a capture
checkEnemyCollision :: Piece -> Piece -> Bool
checkEnemyCollision (Piece _ color1 x1 y1 _) (Piece _ color2 x2 y2 _) = (x1 == x2) && (y1 == y2) && (color1 /= color2)

-- Checks if 2 pieces have an invalid collision
checkFriendCollision :: Piece -> Piece -> Bool
checkFriendCollision (Piece _ color1 x1 y1 _) (Piece _ color2 x2 y2 _) = (x1, y1, color1) == (x2, y2, color2)

-- Replaces the old piece with the piece that just captured it
captureReplace :: Piece -> [Piece] -> [Piece]
captureReplace newPiece allOtherPieces = 
    map (\piece -> if (pieceX piece, pieceY piece) == (pieceX newPiece, pieceY newPiece) then newPiece else piece) allOtherPieces

-- Repeats a movement until it can collide with a piece or it is off the board and then returns all the possible new positions
repeatMovement :: Piece -> Int -> Movement -> [Piece] -> [[Piece]]
repeatMovement piece turn movement allOtherPieces 
    | not (checkOnBoard newPiece) || any (checkFriendCollision newPiece) allOtherPieces = []
    | any (checkEnemyCollision newPiece) allOtherPieces = [captureReplace newPiece allOtherPieces]
    | otherwise = ([newPiece] ++ allOtherPieces) : repeatMovement newPiece turn movement allOtherPieces
    where newPiece = movePiece piece turn movement

-- Manages movements by checking for collisions and invalid moves
singleMovement :: Piece -> [Piece] -> [[Piece]]
singleMovement newPiece allOtherPieces
    | not (checkOnBoard newPiece) || any (checkFriendCollision newPiece) allOtherPieces = []
    | any (checkEnemyCollision newPiece) allOtherPieces = [captureReplace newPiece allOtherPieces]
    | otherwise = [[newPiece] ++ allOtherPieces]

-- Get all king moves
getAllKingMoves :: Piece -> [Piece] -> Int -> [[Piece]]
getAllKingMoves piece allOtherPieces turn = 
    concat $ map (\movement -> singleMovement (movePiece piece turn movement) allOtherPieces) (singleAxial ++ singleDiagonal)

-- Get all rook moves
getAllRookMoves :: Piece -> [Piece] -> Int -> [[Piece]]
getAllRookMoves piece allOtherPieces turn =
    concat $ map (\movement -> repeatMovement piece turn movement allOtherPieces) singleAxial

-- Get all bishop moves
getAllBishopMoves :: Piece -> [Piece] -> Int -> [[Piece]]
getAllBishopMoves piece allOtherPieces turn =
    concat $ map (\movement -> repeatMovement piece turn movement allOtherPieces) singleDiagonal

-- Get all queen moves
getAllQueenMoves :: Piece -> [Piece] -> Int -> [[Piece]]
getAllQueenMoves piece allOtherPieces turn = 
    getAllRookMoves piece allOtherPieces turn ++ getAllBishopMoves piece allOtherPieces turn

-- Get all knight moves
getAllKnightMoves :: Piece -> [Piece] -> Int -> [[Piece]]
getAllKnightMoves piece allOtherPieces turn = 
    concat $ map (\movement -> singleMovement (movePiece piece turn movement) allOtherPieces) knightMoves
    
-- Gets all the possible move positions for the first piece in a list
getAllMovesForPiece :: Int -> [Piece] -> [[Piece]]
getAllMovesForPiece turn (piece : allOtherPieces) = case pieceType piece of
    Pawn -> [(piece : allOtherPieces)]
    Knight -> getAllKnightMoves piece allOtherPieces turn
    Bishop -> getAllBishopMoves piece allOtherPieces turn
    Rook -> getAllRookMoves piece allOtherPieces turn
    Queen -> getAllQueenMoves piece allOtherPieces turn
    King -> getAllKingMoves piece allOtherPieces turn

-- Get all the possible cycles of a list
getAllCycles :: [a] -> [[a]]
getAllCycles (x1 : xn) = take (length (x1 : xn)) ((x1 : xn) : getAllCycles (xn ++ [x1]))

-- Get all the possible moves
getAllMoves :: PieceColor -> Int -> [Piece] -> [[Piece]]
getAllMoves playerColor turn allPieces = concat $ map (getAllMovesForPiece turn) (filter (\pieces -> (pieceColor (pieces !! 0)) == playerColor) (getAllCycles allPieces))

-- Gets the worst case scenario evaluation for a list of positions
getPositionEval ::  PieceColor -> [[Piece]] -> Int
getPositionEval playerColor allPieces = minimum $ map (getTotalPoints playerColor) allPieces

-- Repeats the analysing process iteratively
-- It 
repeatAnalysis :: PieceColor -> Int -> Int -> [[Piece]] -> [[Piece]]
repeatAnalysis playerColor count turn positions 
    | count == 0 = positions
    | otherwise = let
        morePositions = concat $ map (getAllMoves playerColor turn) positions
        averageEval = div (sum $ map (getTotalPoints playerColor) morePositions) (length morePositions)
        in repeatAnalysis (getOtherColor playerColor) (count - 1) (turn + 1) (filter (\position -> getTotalPoints playerColor position >= averageEval) morePositions)

-- Finds the best engine move (which is not neccessarily good yet)
getEngineMove :: PieceColor -> Int -> [Piece] -> Int -> [Piece]
getEngineMove playerColor turn allPieces depth = let
    allMoves = getAllMoves playerColor turn allPieces
    firstAnalysis = map (getAllMoves (getOtherColor playerColor) (turn + 1)) allMoves
    lastAnalysis = map (repeatAnalysis playerColor depth (turn + 2)) firstAnalysis
    evals = map (getPositionEval playerColor) lastAnalysis
    bestMoveIndex = elemIndex (maximum evals) evals
    in case bestMoveIndex of
        Just index -> allMoves !! index
        Nothing -> []

-- Get the piece that has just been moved
getLastMoved :: [Piece] -> Piece
getLastMoved pieces = filter (\piece -> lastMoved piece == maximum (map lastMoved pieces)) pieces !! 0

-- Print the string of the last moved piece
printMove :: Piece -> IO ()
printMove piece = putStrLn ("Moved " ++ (show $ pieceType piece) ++ " to " ++ (showRow piece) ++ " " ++ (show $ pieceY piece))

showRow :: Piece -> String
showRow piece = show (1 + pieceX piece)




