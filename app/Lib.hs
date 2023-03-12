module Lib where

import Types
import Constants
import Data.List

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
standardMove :: Piece -> Pieces -> PositionVector -> Moves
standardMove piece allOtherPieces vector
    | (checkAnyInvalidCollision newPiece allOtherPieces) || (not $ checkPieceOnBoard newPiece) = []
    | otherwise = [Move piece newPiece (newPiece : deleteWhere (\piece -> position piece == position newPiece) allOtherPieces)]
    where
        newPiece = changePiecePosition vector piece

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

-- Gets all the Knight moves
getAllKnightMoves :: Piece -> Pieces -> Moves
getAllKnightMoves piece allOtherPieces = concat $ map (standardMove piece allOtherPieces) (getAllRotations knightMove1 ++ getAllRotations knightMove2)

-- Gets all the King moves
getAllKingMoves :: Piece -> Pieces -> Moves
getAllKingMoves piece allOtherPieces = concat $ map (standardMove piece allOtherPieces) (getAllRotations singleRightMove ++ getAllRotations singleDiagonalMove)

-- This repeats a move until it collides with a piece or is off the board, think of how a rook or bishop moves
repeatMove :: Piece -> Pieces -> PositionVector -> Moves
repeatMove piece allOtherPieces vector = map (\newPiece -> Move piece newPiece (newPiece : deleteWhere (\piece -> position piece == position newPiece) allOtherPieces)) possiblePiecePos
    where 
        possiblePiecePos = takeWhile  canRepeat $ iterate (changePiecePosition vector) (changePiecePosition vector piece)
        canRepeat newPiece = not ((anyInvalidCollision newPiece) || (pieceNotOnBoard newPiece) || (anyValidcollisionBefore newPiece))
        anyInvalidCollision newPiece = checkAnyInvalidCollision newPiece allOtherPieces
        pieceNotOnBoard newPiece = not $ checkPieceOnBoard newPiece
        anyValidcollisionBefore newPiece = checkAnyValidCollision (changePiecePosition (inverseVector vector) newPiece) allOtherPieces

-- Gets all the Rook moves
getAllRookMoves :: Piece -> Pieces -> Moves
getAllRookMoves piece allOtherPieces = concat $ map (repeatMove piece allOtherPieces) (getAllRotations singleRightMove)

-- Gets all the Bishop moves
getAllBishopMoves :: Piece -> Pieces -> Moves
getAllBishopMoves piece allOtherPieces = concat $ map (repeatMove piece allOtherPieces) (getAllRotations singleDiagonalMove)

-- Gets all the Queen moves, noting that the Queen moves like a Bishop and Rook combined
getAllQueenMoves :: Piece -> Pieces -> Moves
getAllQueenMoves piece allOtherPieces = getAllRookMoves piece allOtherPieces ++ getAllBishopMoves piece allOtherPieces

-- Gets all the moves for a particular piece
getAllMovesForPiece :: Piece -> Pieces -> Moves
getAllMovesForPiece piece allOtherPieces = case pieceType piece of
    Pawn _ -> [] -- do this later cos pawns are special
    Knight -> getAllKnightMoves piece allOtherPieces
    Bishop -> getAllBishopMoves piece allOtherPieces
    Rook -> getAllRookMoves piece allOtherPieces
    Queen -> getAllQueenMoves piece allOtherPieces
    King -> getAllKingMoves piece allOtherPieces

-- Gets all the moves for a position
getAllMoves :: Color -> ChessPosition -> Moves
getAllMoves color position = concat $ map (\piece -> if isOwnedBy color piece then getAllMovesForPiece piece (delete piece position) else []) position

-- Checks if the game is over
checkGameOver :: ChessPosition -> Bool
checkGameOver position = (length $ filter (\piece -> pieceType piece == King) position) /= 2

-- Generates a tree of possible moves from a single move and the color that made that move
generateMoveTree :: Move -> Tree Move
generateMoveTree move
    | checkGameOver newPos = Tree move []
    | otherwise = Tree move newMoveTrees
    where
        newPos = newChessPos move
        otherColor = getOtherColor $ getMoveColor move
        newMoveTrees = map generateMoveTree (getAllMoves otherColor newPos)

-- This cuts all branches of a tree after a tree after a set depth
-- This is so we can deal with the move tree being an infinite tree
cutTree :: Int -> Tree a -> Tree a
cutTree depth tree
    | depth == 0 = Tree move []
    | otherwise = Tree move cutBranches
    where
        move = node tree
        cutBranches = map (cutTree (depth - 1)) (branches tree)

-- Checks if the user input is valid
getUserMove :: String -> Color -> ChessPosition -> Maybe Move
getUserMove text color chessPos = case index of
    Just i -> Just (possibleMoves !! i)
    Nothing -> Nothing
    where
        possibleMoves = getAllMoves color chessPos
        index = findIndex (== text) $ map prettifyMove possibleMoves
        
-- Deletes all the items from a list where a function is true
deleteWhere :: (a -> Bool) -> [a] -> [a]
deleteWhere func [] = []
deleteWhere func (x : xn)
    | func x = deleteWhere func xn
    | otherwise = (x : deleteWhere func xn)

-- Gets the color that carried out a move
getMoveColor :: Move -> Color
getMoveColor (Move oldPiece newPiece newChessPos) = pieceColor oldPiece

-- Gets the minimum eval for the first level, then maximum for the next, and so on
-- Note that color here should be the color that made the first move in the tree it was given
getMoveEval :: Color -> Tree Move -> Int
getMoveEval playerColor moveTree
        | null $ branches moveTree = getPlayerEval playerColor (newChessPos $ node moveTree)
        | (getMoveColor $ node moveTree) == playerColor = minimum $ map (getMoveEval playerColor) (branches moveTree)
        | (getMoveColor $ node moveTree) == (getOtherColor playerColor) = maximum $ map (getMoveEval playerColor) (branches moveTree)

-- Gets the best move by looking through the move evals and fincing the best one
getBestMove :: Tree Move -> Move
getBestMove moveTree = case index of
    Just i -> node (branches moveTree !! i)
    where
        moveColor = getOtherColor $ getMoveColor $ node moveTree
        moveEvals = map (getMoveEval moveColor) (branches moveTree)
        index = findIndex (== maximum moveEvals) moveEvals