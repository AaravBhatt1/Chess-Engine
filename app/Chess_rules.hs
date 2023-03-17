module Chess_rules where

import Constants
import Types
import Data.List

-- Deletes all the items where a function is true in a list
deleteWhere :: (a -> Bool) -> [a] -> [a]
deleteWhere func [] = []
deleteWhere func (x : xn)
    | func x = deleteWhere func xn
    | otherwise = x : deleteWhere func xn

-- Checks if a collision is invalid, meaning that a piece is colliding with a piece of its own color
checkInvalidCollision :: Piece -> Piece -> Bool
checkInvalidCollision newPiece piece = (pieceColor newPiece == pieceColor piece) && (piecePosition newPiece == piecePosition piece)

-- Checks if a collision is valid, meaning that a piece is capturing a piece of the other color
checkValidCollision :: Piece -> Piece -> Bool
checkValidCollision newPiece piece = (pieceColor newPiece /= pieceColor piece) && (piecePosition newPiece == piecePosition piece)

-- Checks for any invalid collisions for a piece with a list of pieces
checkAnyInvalidCollision :: Piece -> Pieces -> Bool
checkAnyInvalidCollision newPiece allOtherPieces = any (checkInvalidCollision newPiece) allOtherPieces

-- Checks for any valid collisions for a piece with a list of pieces
checkAnyValidCollision :: Piece -> Pieces -> Bool
checkAnyValidCollision newPiece allOtherPieces = any (checkValidCollision newPiece) allOtherPieces

-- Checks if the piece is on the board or not, using the position vector
checkPieceOnBoard :: Piece -> Bool
checkPieceOnBoard (Piece pieceType pieceColor (Vector x y)) = (x >= 0) && (x <= 7) && (y >= 0) && (y <= 7)

-- Carries out a movement, by returning the new piece after it has moved
changePiecePosition :: Vector -> Piece -> Piece
changePiecePosition vector (Piece pieceType pieceColor piecePosition) = Piece pieceType pieceColor (addPositionVector piecePosition vector)

-- This is a standard move, that a knight or king could make
standardMove :: Piece -> Pieces -> Vector -> Moves
standardMove piece allOtherPieces vector
    | (checkAnyInvalidCollision newPiece allOtherPieces) || (not $ checkPieceOnBoard newPiece) = []
    | otherwise = [Move piece newPiece (newPiece : deleteWhere (checkValidCollision newPiece) allOtherPieces)]
    where
        newPiece = changePiecePosition vector piece

-- Gets all the Knight moves
getAllKnightMoves :: Piece -> Pieces -> Moves
getAllKnightMoves piece allOtherPieces = concat $ map (standardMove piece allOtherPieces) (getAllRotations knightMove1 ++ getAllRotations knightMove2)

-- Gets all the King moves
getAllKingMoves :: Piece -> Pieces -> Moves
getAllKingMoves piece allOtherPieces = concat $ map (standardMove piece allOtherPieces) (getAllRotations singleRightMove ++ getAllRotations singleDiagonalMove)

-- This repeats a move until it collides with a piece or is off the board, think of how a rook or bishop moves
repeatMove :: Piece -> Pieces -> Vector -> Moves
repeatMove piece allOtherPieces vector = map (\newPiece -> Move piece newPiece (newPiece : deleteWhere (checkValidCollision newPiece) allOtherPieces)) possiblePiecePos
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

-- Gets all the pawn moves
getAllPawnMoves :: Piece -> Pieces -> Moves
getAllPawnMoves piece allOtherPieces = case pieceColor piece of
    White -> getAllWhitePawnMoves piece allOtherPieces
    Black -> getAllBlackPawnMoves piece allOtherPieces

-- Gets all the white pawn moves
getAllWhitePawnMoves :: Piece -> Pieces -> Moves
getAllWhitePawnMoves piece allOtherPieces = map (\newPiece -> Move piece newPiece (newPiece : deleteWhere (checkValidCollision newPiece) allOtherPieces)) (regularMove ++ captureMove1 ++ captureMove2)
    where
        regularNewPiece = if (yPos $ piecePosition piece) == 6 then Piece Queen White (Vector (xPos $ piecePosition piece) 7) else changePiecePosition (Vector 0 1) piece
        captureNewPiece1 = changePiecePosition (Vector (-1) 1) piece
        captureNewPiece2 = changePiecePosition (Vector 1 1) piece
        regularMove = if (checkAnyInvalidCollision regularNewPiece allOtherPieces) || (checkAnyValidCollision regularNewPiece allOtherPieces) then [] else [regularNewPiece]
        captureMove1 = if (checkAnyValidCollision captureNewPiece1 allOtherPieces) then [captureNewPiece1] else []
        captureMove2 = if (checkAnyValidCollision captureNewPiece2 allOtherPieces) then [captureNewPiece2] else []

-- Gets all the black pawn moves
getAllBlackPawnMoves :: Piece -> Pieces -> Moves
getAllBlackPawnMoves piece allOtherPieces = map (\newPiece -> Move piece newPiece (newPiece : deleteWhere (checkValidCollision newPiece) allOtherPieces)) (regularMove ++ captureMove1 ++ captureMove2)
    where
        regularNewPiece = if (yPos $ piecePosition piece) == 1 then Piece Queen Black (Vector (xPos $ piecePosition piece) 0) else changePiecePosition (Vector 0 (-1)) piece
        captureNewPiece1 = changePiecePosition (Vector (-1) (-1)) piece
        captureNewPiece2 = changePiecePosition (Vector 1 (-1)) piece
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
getAllMoves playerColor chessPosition = concat $ map (\piece -> if isOwnedBy playerColor piece then getAllMovesForPiece piece (delete piece chessPosition) else []) chessPosition

-- Checks if the game is over, by seeing if there are not 2 kings on the board
checkGameOver :: ChessPosition -> Bool
checkGameOver chessPosition = (length $ filter (\piece -> pieceType piece == King) chessPosition) /= 2