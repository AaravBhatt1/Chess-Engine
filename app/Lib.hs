module Lib where

import Types
import Constants
import Data.List
import Data.Maybe

-- Gets the number of points a piece is worth
getPieceValue :: Piece -> Int
getPieceValue piece = case pieceType piece of
    Pawn moved -> 1
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
checkOnBoard (Piece _ _ x y) = (x >= 0) && (y >= 0) && (x <= 7) && (y <= 7)

-- Moves a piece with a movement vector
movePiece :: Piece -> Movement ->  Piece
movePiece (Piece pieceType pieceColor pieceX pieceY) (Movement movementX movementY) = 
    Piece pieceType pieceColor (pieceX + movementX) (pieceY + movementY)

-- Checks if 2 pieces have a capture
checkEnemyCollision :: Piece -> Piece -> Bool
checkEnemyCollision (Piece _ color1 x1 y1) (Piece _ color2 x2 y2) = (x1 == x2) && (y1 == y2) && (color1 /= color2)

-- Checks if 2 pieces have an invalid collision
checkFriendCollision :: Piece -> Piece -> Bool
checkFriendCollision (Piece _ color1 x1 y1) (Piece _ color2 x2 y2) = (x1, y1, color1) == (x2, y2, color2)

-- Replaces the old piece with the piece that just captured it
captureReplace :: Piece -> [Piece] -> [Piece]
captureReplace newPiece allOtherPieces = 
    map (\piece -> if (pieceX piece, pieceY piece) == (pieceX newPiece, pieceY newPiece) then newPiece else piece) allOtherPieces

-- Repeats a movement until it can collide with a piece or it is off the board and then returns all the possible new positions
repeatMovement :: Piece -> Movement -> [Piece] -> [[Piece]]
repeatMovement piece movement allOtherPieces 
    | not (checkOnBoard newPiece) || any (checkFriendCollision newPiece) allOtherPieces = []
    | any (checkEnemyCollision newPiece) allOtherPieces = [captureReplace newPiece allOtherPieces]
    | otherwise = ([newPiece] ++ allOtherPieces) : repeatMovement newPiece movement allOtherPieces
    where newPiece = movePiece piece movement

singleMovement :: Piece -> [Piece] -> [[Piece]]
singleMovement newPiece allOtherPieces
    | not (checkOnBoard newPiece) || any (checkFriendCollision newPiece) allOtherPieces = []
    | any (checkEnemyCollision newPiece) allOtherPieces = [captureReplace newPiece allOtherPieces]
    | otherwise = [[newPiece] ++ allOtherPieces]

-- Get all king moves
getAllKingMoves :: Piece -> [Piece] -> [[Piece]]
getAllKingMoves piece allOtherPieces = concat $ map (\movement -> singleMovement (movePiece piece movement) allOtherPieces) (singleAxial ++ singleDiagonal)

-- Get all rook moves
getAllRookMoves :: Piece -> [Piece] -> [[Piece]]
getAllRookMoves piece allOtherPieces =
    concat $ map (\movement -> repeatMovement piece movement allOtherPieces) singleAxial

-- Get all bishop moves
getAllBishopMoves :: Piece -> [Piece] -> [[Piece]]
getAllBishopMoves piece allOtherPieces =
    concat $ map (\movement -> repeatMovement piece movement allOtherPieces) singleDiagonal

-- Get all queen moves
getAllQueenMoves :: Piece -> [Piece] -> [[Piece]]
getAllQueenMoves piece allOtherPieces = getAllRookMoves piece allOtherPieces ++ getAllBishopMoves piece allOtherPieces
    
-- Gets all the possible move positions for the first piece in a list
getAllMovesForPiece :: [Piece] -> [[Piece]]
getAllMovesForPiece (piece : allOtherPieces) = case pieceType piece of
    Pawn moved -> [(piece : allOtherPieces)]
    Knight -> [(piece : allOtherPieces)]
    Bishop -> getAllBishopMoves piece allOtherPieces
    Rook -> getAllRookMoves piece allOtherPieces
    Queen -> getAllQueenMoves piece allOtherPieces
    King -> getAllKingMoves piece allOtherPieces

-- Get all the possible cycles of a list
getAllCycles :: [a] -> [[a]]
getAllCycles (x1 : xn) = take (length (x1 : xn)) ((x1 : xn) : getAllCycles (xn ++ [x1]))

-- Get all the possible moves
getAllMoves :: PieceColor -> [Piece] -> [[Piece]]
getAllMoves playerColor allPieces = concat $ map (getAllMovesForPiece) (filter (\pieces -> (pieceColor (pieces !! 0)) == playerColor) (getAllCycles allPieces))

-- Gets the worst case scenario evaluation for a list of positions
getTotalEval ::  PieceColor -> [[Piece]] -> Int
getTotalEval playerColor allPieces = sum $ map (getTotalPoints playerColor) allPieces

-- Repeats the analysing process iteratively
repeatAnalysis :: PieceColor -> Int -> [[Piece]] -> [[Piece]]
repeatAnalysis playerColor count positions 
    | count == 0 = positions
    | otherwise = let
        morePositions = concat $ map (getAllMoves playerColor) positions
        in repeatAnalysis (getOtherColor playerColor) (count - 1) morePositions

-- Finds the best engine move
getEngineMove :: PieceColor -> [Piece] -> Int -> [Piece]
getEngineMove playerColor allPieces depth = let
    allMoves = getAllMoves playerColor allPieces
    firstAnalysis = map (getAllMoves $ getOtherColor playerColor) allMoves
    lastAnalysis = map (repeatAnalysis playerColor (depth - 1)) firstAnalysis
    evals = map (getTotalEval playerColor) lastAnalysis
    bestMoveIndex = elemIndex (maximum evals) evals
    in case bestMoveIndex of
        Just index -> allMoves !! index
        Nothing -> []

