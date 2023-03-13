module Chess_engine where

import Types
import Constants
import Data.List

-- Returns true if a piece is owned by a player otherwise returns false
isOwnedBy :: Color -> Piece -> Bool
isOwnedBy playerColor piece = playerColor == pieceColor piece

-- This function returns the total number of points a white has
getTotalPoints :: ChessPosition -> Int
getTotalPoints allPieces = totalWhitePoints - totalBlackPoints
    where
        totalWhitePoints = sum $ map (getPieceValue) $ filter (isOwnedBy White) allPieces 
        totalBlackPoints = sum $ map (getPieceValue) $ filter (isOwnedBy Black) allPieces

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
changePiecePosition vector (Piece pieceType color position) = Piece pieceType color (addPositionVector position vector)

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

getAllPawnMoves :: Piece -> Pieces -> Moves
getAllPawnMoves piece allOtherPieces = case pieceColor piece of
    White -> getAllWhitePawnMoves piece allOtherPieces
    Black -> getAllBlackPawnMoves piece allOtherPieces

getAllWhitePawnMoves :: Piece -> Pieces -> Moves
getAllWhitePawnMoves piece allOtherPieces = map (\newPiece -> Move piece newPiece (newPiece : deleteWhere (\piece -> position piece == position newPiece) allOtherPieces)) (regularMove ++ captureMove1 ++ captureMove2)
    where
        regularNewPiece = changePiecePosition (PositionVector 0 1) piece
        captureNewPiece1 = changePiecePosition (PositionVector (-1) 1) piece
        captureNewPiece2 = changePiecePosition (PositionVector 1 1) piece
        regularMove = if (checkAnyInvalidCollision regularNewPiece allOtherPieces) || (checkAnyValidCollision regularNewPiece allOtherPieces) then [] else [regularNewPiece]
        captureMove1 = if (checkAnyValidCollision captureNewPiece1 allOtherPieces) then [captureNewPiece1] else []
        captureMove2 = if (checkAnyValidCollision captureNewPiece2 allOtherPieces) then [captureNewPiece2] else []


getAllBlackPawnMoves :: Piece -> Pieces -> Moves
getAllBlackPawnMoves piece allOtherPieces = map (\newPiece -> Move piece newPiece (newPiece : deleteWhere (\piece -> position piece == position newPiece) allOtherPieces)) (regularMove ++ captureMove1 ++ captureMove2)
    where
        regularNewPiece = changePiecePosition (PositionVector 0 (-1)) piece
        captureNewPiece1 = changePiecePosition (PositionVector (-1) (-1)) piece
        captureNewPiece2 = changePiecePosition (PositionVector 1 (-1)) piece
        regularMove = if (checkAnyInvalidCollision regularNewPiece allOtherPieces) || (checkAnyValidCollision regularNewPiece allOtherPieces) then [] else [regularNewPiece]
        captureMove1 = if (checkAnyValidCollision captureNewPiece1 allOtherPieces) then [captureNewPiece1] else []
        captureMove2 = if (checkAnyValidCollision captureNewPiece2 allOtherPieces) then [captureNewPiece2] else []

-- Gets all the moves for a particular piece
getAllMovesForPiece :: Piece -> Pieces -> Moves
getAllMovesForPiece piece allOtherPieces = case pieceType piece of
    Pawn -> getAllPawnMoves piece allOtherPieces
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
getMoveEval :: Tree Move -> Int
getMoveEval moveTree
        | null $ branches moveTree = getTotalPoints $ newChessPos $ node moveTree
        | (getMoveColor $ node moveTree) == White = minimum $ map getMoveEval $ branches moveTree
        | (getMoveColor $ node moveTree) == Black = maximum $ map getMoveEval $ branches moveTree

-- Gets the best move by looking through the move evals and fincing the best one
getBestMove :: Tree Move -> Move
getBestMove moveTree = case index of
    Just i -> node (branches moveTree !! i)
    where
        moveEvals = map getMoveEval (branches moveTree)
        index = findIndex (== getMoveEval moveTree) moveEvals