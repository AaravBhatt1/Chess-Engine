module Constants where

import Types

singleRightMove :: Vector
singleRightMove = Vector 1 0

singleDiagonalMove :: Vector
singleDiagonalMove = Vector 1 1

knightMove1 :: Vector
knightMove1 = Vector 2 1

knightMove2 :: Vector
knightMove2 = Vector 2 (-1)

startingBoard :: ChessPosition
startingBoard = [
    Piece Pawn White (Vector 0 1),
    Piece Pawn White (Vector 1 1),
    Piece Pawn White (Vector 2 1),
    Piece Pawn White (Vector 3 1),
    Piece Pawn White (Vector 4 1),
    Piece Pawn White (Vector 5 1),
    Piece Pawn White (Vector 6 1),
    Piece Pawn White (Vector 7 1),
    Piece Pawn Black (Vector 0 6),
    Piece Pawn Black (Vector 1 6),
    Piece Pawn Black (Vector 2 6),
    Piece Pawn Black (Vector 3 6),
    Piece Pawn Black (Vector 4 6),
    Piece Pawn Black (Vector 5 6),
    Piece Pawn Black (Vector 6 6),
    Piece Pawn Black (Vector 7 6),
    Piece Rook White (Vector 0 0),
    Piece Knight White (Vector 1 0),
    Piece Bishop White (Vector 2 0),
    Piece Queen White (Vector 3 0),
    Piece King White (Vector 4 0),
    Piece Bishop White (Vector 5 0),
    Piece Knight White (Vector 6 0),
    Piece Rook White (Vector 7 0),
    Piece Rook Black (Vector 0 7),
    Piece Knight Black (Vector 1 7),
    Piece Bishop Black (Vector 2 7),
    Piece Queen Black (Vector 3 7),
    Piece King Black (Vector 4 7),
    Piece Bishop Black (Vector 5 7),
    Piece Knight Black (Vector 6 7),
    Piece Rook Black (Vector 7 7)
    ]