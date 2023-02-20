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
checkCapture :: Piece -> Piece -> Bool
checkCapture (Piece _ color1 x1 y1) (Piece _ color2 x2 y2) = (x1 == x2) && (y1 == y2) && (color1 /= color2)

-- Checks if 2 pieces have an invalid collision
checkInvalidCollision :: Piece -> Piece -> Bool
checkInvalidCollision (Piece _ color1 x1 y1) (Piece _ color2 x2 y2) = (x1, y1, color1) == (x2, y2, color2)

-- Replaces the old piece with the piece that just captured it
captureReplace :: Piece -> [Piece] -> [Piece]
captureReplace newPiece allOtherPieces = 
    map (\piece -> if (pieceX piece, pieceY piece) == (pieceX newPiece, pieceY newPiece) then newPiece else piece) allOtherPieces

-- Repeats a movement until it can collide with a piece or it is off the board and then returns all the possible new positions
repeatMovement :: Piece -> Movement -> [Piece] -> [[Piece]]
repeatMovement piece movement allOtherPieces= let
    newPiece = movePiece piece movement
    in if not (checkOnBoard newPiece) || any (checkInvalidCollision newPiece) allOtherPieces then []
    else if any (checkCapture newPiece) allOtherPieces then [captureReplace newPiece allOtherPieces]
    else ([newPiece] ++ allOtherPieces) : repeatMovement newPiece movement allOtherPieces

-- Get all rook moves
getAllRookMoves :: Piece -> [Piece] -> [[Piece]]
getAllRookMoves piece allOtherPieces =
    concat (map (\movement -> repeatMovement piece movement allOtherPieces) [moveUp, moveDown, moveLeft, moveRight])
    
-- Gets all the possible move positions for the first piece in a list
getAllMovesForPiece :: [Piece] -> [[Piece]]
getAllMovesForPiece (piece : allOtherPieces) = case pieceType piece of
    Pawn -> []
    Knight -> []
    Bishop -> []
    Rook -> getAllRookMoves piece allOtherPieces
    Queen -> []
    King -> []

-- Get all the possible cycles of a list
getAllCycles :: [a] -> [[a]]
getAllCycles (x1 : xn) = take (length (x1 : xn)) ((x1 : xn) : getAllCycles (xn ++ [x1]))
getAllCycles [] = []

-- Get all the possible moves
getAllMoves :: PieceColor -> [Piece] -> [[Piece]]
getAllMoves playerColor allPieces = concat $ map (getAllMovesForPiece) (filter (\pieces -> (pieceColor (pieces !! 0)) == playerColor) (getAllCycles allPieces))

-- Gets the worst case scenario evaluation for a list of positions
getWorstCaseEval ::  PieceColor -> [[Piece]] -> Int
getWorstCaseEval playerColor allPieces = minimum $ map (getTotalPoints playerColor) allPieces

-- Finds the best engine move
-- Currently can only do this at a depth of one
getEngineMove :: PieceColor -> [Piece] -> Int -> [Piece]
getEngineMove playerColor allPieces depth = let
    allMoves = getAllMoves playerColor allPieces
    allEvals = map (getTotalPoints playerColor) allMoves
    bestMoveIndex = elemIndex (maximum allEvals) allEvals
    in case bestMoveIndex of
        Just index -> allMoves !! index
        Nothing -> []
