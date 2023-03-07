module Lib where

import Types
import Constants
import Data.List
import Data.Tree

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
isOwnedBy playerColor piece = playerColor == pieceColor piece

-- This function returns the total number of points a player has
getPlayerEval :: Color -> ChessPosition -> Int
getPlayerEval playerColor allPieces = totalPlayerPoints - totalEnemyPoints
    where
        totalPlayerPoints = sum $ map (getPieceValue) $ filter (isOwnedBy playerColor) allPieces 
        totalEnemyPoints = sum $ map (getPieceValue) $ filter (isOwnedBy $ getOtherColor playerColor) allPieces

-- This adds 2 position vectors together
addPositionVector :: PositionVector -> PositionVector -> PositionVector
addPositionVector (PositionVector x1 y1) (PositionVector x2 y2) = PositionVector (x1 + x2) (y1 + y2)

-- This rotates a vector 90 degrees anticlockwise
rotateVector :: PositionVector -> PositionVector
rotateVector (PositionVector x y) = PositionVector y (-x)

-- This basically rotates a vector 90 degrees 4 times
getAllRotations :: PositionVector -> [PositionVector]
getAllRotations vector = take 4 $ iterate rotateVector vector

-- This reverses a vector so that we can get the previous move
inverseVector :: PositionVector -> PositionVector
inverseVector (PositionVector x y) = PositionVector (-x) (-y)

-- Check if a collision is invalid, meaning that a piece is colliding with a piece of its own color
checkInvalidCollision :: Piece -> Piece -> Bool
checkInvalidCollision newPiece piece = (pieceColor newPiece == pieceColor piece) && (position newPiece == position piece)

-- Check if a collision is valid, meaning that a piece is capturing a piece of the other color
checkValidCollision :: Piece -> Piece -> Bool
checkValidCollision newPiece piece = (pieceColor newPiece /= pieceColor piece) && (position newPiece == position piece)

-- Checks if a position vector is on the board or not
checkPositionVectorOnBoard :: PositionVector -> Bool
checkPositionVectorOnBoard (PositionVector x y) = (x >= 0) && (x <= 7) && (y >= 0) && (y <= 7)

-- Checks if the piece is on the board or not
checkPieceOnBoard :: Piece -> Bool
checkPieceOnBoard piece = checkPositionVectorOnBoard $ position piece

-- Carries out a movement
changePiecePosition :: PositionVector -> Piece -> Piece
changePiecePosition vector (Piece pieceType color position) = case pieceType of
    Pawn _ -> Piece (Pawn True) color (addPositionVector position vector)
    otherwise -> Piece pieceType color (addPositionVector position vector)

-- Checks for any invalid collisions for a piece with a list of pieces
checkAnyInvalidCollision :: Piece -> Pieces -> Bool
checkAnyInvalidCollision newPiece allOtherPieces = any (checkInvalidCollision newPiece) allOtherPieces

-- Checks for any valid collisions for a piece with a list of pieces
checkAnyValidCollision :: Piece -> Pieces -> Bool
checkAnyValidCollision newPiece allOtherPieces = any (checkValidCollision newPiece) allOtherPieces

-- This is a standard move, that a knight or king could make
standardMove :: Color -> Piece -> Pieces -> PositionVector -> Moves
standardMove color piece allOtherPieces vector
    | (checkAnyInvalidCollision newPiece allOtherPieces) || (not $ checkPieceOnBoard newPiece) = []
    | otherwise = [Move piece newPiece (newPiece : delete newPiece allOtherPieces) color]
    where
        newPiece = changePiecePosition vector piece

-- This returns the position as a string as the coordinates of the board in chess notation
prettifyPosition :: PositionVector -> String
prettifyPosition (PositionVector x y) = "abcdefgh" !! x : show (y + 1)

-- This returns the move in a string of how it would be displayed to the user
prettifyMove :: Move -> String
prettifyMove (Move oldPiece newPiece newChessPos color) = "Move " ++ prettyType ++ " from " ++ prettyOldPos ++ " to " ++ prettyNewPos
    where
        prettyType = show $ pieceType oldPiece
        prettyOldPos = prettifyPosition $ position oldPiece
        prettyNewPos = prettifyPosition $ position newPiece

-- Gets all the Knight moves
getAllKnightMoves :: Color -> Piece -> Pieces -> Moves
getAllKnightMoves color piece allOtherPieces = concat $ map (standardMove color piece allOtherPieces) (getAllRotations knightMove1 ++ getAllRotations knightMove2)

-- Gets all the King moves
getAllKingMoves :: Color -> Piece -> Pieces -> Moves
getAllKingMoves color piece allOtherPieces = concat $ map (standardMove color piece allOtherPieces) (getAllRotations singleRightMove ++ getAllRotations singleDiagonalMove)

-- This repeats a move until it collides with a piece or is off the board, think of how a rook or bishop moves
repeatMove :: Color -> Piece -> Pieces -> PositionVector -> Moves
repeatMove color piece allOtherPieces vector = map (\newPiece -> Move piece newPiece (newPiece : delete newPiece allOtherPieces) color) possiblePiecePos
    where 
        possiblePiecePos = takeWhile  canRepeat $ iterate (changePiecePosition vector) (changePiecePosition vector piece)
        canRepeat newPiece = not ((anyInvalidCollision newPiece) || (pieceNotOnBoard newPiece) || (anyValidcollisionBefore newPiece))
        anyInvalidCollision newPiece = checkAnyInvalidCollision newPiece allOtherPieces
        pieceNotOnBoard newPiece = not $ checkPieceOnBoard newPiece
        anyValidcollisionBefore newPiece = checkAnyValidCollision (changePiecePosition (inverseVector vector) newPiece) allOtherPieces

-- Gets all the Rook moves
getAllRookMoves :: Color -> Piece -> Pieces -> Moves
getAllRookMoves color piece allOtherPieces = concat $ map (repeatMove color piece allOtherPieces) (getAllRotations singleRightMove)

-- Gets all the Bishop moves
getAllBishopMoves :: Color -> Piece -> Pieces -> Moves
getAllBishopMoves color piece allOtherPieces = concat $ map (repeatMove color piece allOtherPieces) (getAllRotations singleDiagonalMove)

-- Gets all the Queen moves, noting that the Queen moves like a Bishop and Rook combined
getAllQueenMoves :: Color -> Piece -> Pieces -> Moves
getAllQueenMoves color piece allOtherPieces = getAllRookMoves color piece allOtherPieces ++ getAllBishopMoves color piece allOtherPieces

-- Gets all the moves for a particular piece
getAllMovesForPiece :: Color -> Piece -> Pieces -> Moves
getAllMovesForPiece color piece allOtherPieces = case pieceType piece of
    Pawn _ -> [] -- do this later cos pawns are special
    Knight -> getAllKnightMoves color piece allOtherPieces
    Bishop -> getAllBishopMoves color piece allOtherPieces
    Rook -> getAllRookMoves color piece allOtherPieces
    Queen -> getAllQueenMoves color piece allOtherPieces
    King -> getAllKingMoves color piece allOtherPieces

-- Gets all the moves for a position
getAllMoves :: Color -> ChessPosition -> Moves
getAllMoves color position = concat $ map (\piece -> if isOwnedBy color piece then getAllMovesForPiece color piece (delete piece position) else []) position

-- Checks if the game is over
checkGameOver :: ChessPosition -> Bool
checkGameOver position = (length $ filter (\piece -> pieceType piece == King) position) == 2

-- Creates a builder function so that we can create a tree of possible moves
moveTreeBuildNode :: Move -> (Move, Moves)
moveTreeBuildNode startingMove = if (not $ checkGameOver $ newChessPos startingMove) then (startingMove, getAllMoves (getOtherColor $ playerColor startingMove) (newChessPos startingMove))else (startingMove, [])

-- Creates a tree of possible moves
generateMoveTree :: Move -> Tree Move
generateMoveTree startingMove = generateMoveTree startingMove