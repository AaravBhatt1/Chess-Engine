module Lib where

import Types
import Constants

-- This function returns the number of points a piece is worth
getPieceValue :: Piece -> Int
getPieceValue piece = case pieceType piece of
    Pawn _ -> 1
    Knight -> 3
    Bishop -> 3
    Rook -> 5
    Queen -> 9
    King -> 40

-- Returns the other color
getOtherColor :: Color -> Color
getOtherColor color = case color of
    White -> Black
    Black -> White

-- Returns true if a piece is owned by a player otherwise returns false
isOwnedBy :: Color -> Piece -> Bool
isOwnedBy playerColor piece = playerColor == color piece

-- This function returns the total number of points a player has
getPlayerEval :: Color -> ChessPosition -> Int
getPlayerEval playerColor allPieces = let
    totalPlayerPoints = sum $ map (getPieceValue) $ filter (isOwnedBy playerColor) allPieces 
    totalEnemyPoints = sum $ map (getPieceValue) $ filter (isOwnedBy $ getOtherColor playerColor) allPieces
    in totalPlayerPoints - totalEnemyPoints

-- This adds 2 position vectors together
addPositionVector :: PositionVector -> PositionVector -> PositionVector
addPositionVector (PositionVector x1 y1) (PositionVector x2 y2) = PositionVector (x1 + x2) (y1 + y2)

-- This rotates a vector 90 degrees anticlockwise
rotateVector :: PositionVector -> PositionVector
rotateVector (PositionVector x y) = PositionVector y (-x)

-- This basically rotates a vector 90 degrees 4 times
getAllRotations :: PositionVector -> [PositionVector]
getAllRotations vector = take 4 $ iterate rotateVector vector

-- Check if a collision is invalid, meaning that a piece is colliding with a piece of its own color
checkInvalidCollision :: Piece -> Piece -> Bool
checkInvalidCollision newPiece piece = (color newPiece == color piece) && (position newPiece == position piece)

-- Check if a collision is valid, meaning that a piece is capturing a piece of the other color
checkValidCollision :: Piece -> Piece -> Bool
checkValidCollision newPiece piece = (color newPiece /= color piece) && (position newPiece == position piece)

-- Checks if a position vector is on the board or not
checkPositionVectorOnBoard :: PositionVector -> Bool
checkPositionVectorOnBoard (PositionVector x y) = (x >= 0) && (x <= 7) && (y >= 0) && (y <= 7)

-- Checks if the piece is on the board or not
checkPieceOnBoard :: Piece -> Bool
checkPieceOnBoard piece = checkPositionVectorOnBoard $ position piece

-- Carries out a movement
changePiecePosition :: Piece -> PositionVector -> Piece
changePiecePosition (Piece pieceType color position) movement = case pieceType of
    Pawn _ -> Piece (Pawn True) color (addPositionVector position movement)
    otherwise -> Piece pieceType color (addPositionVector position movement)

-- This removes the piece that was captured from the board, if any
captureRemove :: Piece -> Pieces -> Pieces
captureRemove _ [] = []
captureRemove newPiece (piece : otherPieces)
    | checkValidCollision newPiece piece = otherPieces
    | otherwise = piece : captureRemove newPiece otherPieces

-- This is a standard move, that a knight or king could make
standardMove :: Piece -> Pieces -> PositionVector -> Moves
standardMove piece allOtherPieces vector
    | (any (checkInvalidCollision newPiece) allOtherPieces) || (not $ checkPieceOnBoard newPiece) = []
    | otherwise = [Move piece newPiece (newPiece : captureRemove newPiece allOtherPieces)]
    where
        newPiece = changePiecePosition piece vector

-- Gets all the Knight moves
getAllKnightMoves :: Piece -> Pieces -> Moves
getAllKnightMoves piece allOtherPieces = concat $ map (standardMove piece allOtherPieces) (getAllRotations knightMove1 ++ getAllRotations knightMove2)

-- Gets all the King moves
getAllKingMoves :: Piece -> Pieces -> Moves
getAllKingMoves piece allOtherPieces = concat $ map (standardMove piece allOtherPieces) (getAllRotations singleRightMove)