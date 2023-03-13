module Constants where

import Types

singleRightMove :: PositionVector
singleRightMove = PositionVector 1 0

singleDiagonalMove :: PositionVector
singleDiagonalMove = PositionVector 1 1

knightMove1 :: PositionVector
knightMove1 = PositionVector 2 1

knightMove2 :: PositionVector
knightMove2 = PositionVector 2 (-1)

startingBoard :: ChessPosition
startingBoard = [
    Piece Pawn White (PositionVector 0 1),
    Piece Pawn White (PositionVector 1 1),
    Piece Pawn White (PositionVector 2 1),
    Piece Pawn White (PositionVector 3 1),
    Piece Pawn White (PositionVector 4 1),
    Piece Pawn White (PositionVector 5 1),
    Piece Pawn White (PositionVector 6 1),
    Piece Pawn White (PositionVector 7 1),
    Piece Pawn Black (PositionVector 0 6),
    Piece Pawn Black (PositionVector 1 6),
    Piece Pawn Black (PositionVector 2 6),
    Piece Pawn Black (PositionVector 3 6),
    Piece Pawn Black (PositionVector 4 6),
    Piece Pawn Black (PositionVector 5 6),
    Piece Pawn Black (PositionVector 6 6),
    Piece Pawn Black (PositionVector 7 6),
    Piece Rook White (PositionVector 0 0),
    Piece Knight White (PositionVector 1 0),
    Piece Bishop White (PositionVector 2 0),
    Piece Queen White (PositionVector 3 0),
    Piece King White (PositionVector 4 0),
    Piece Bishop White (PositionVector 5 0),
    Piece Knight White (PositionVector 6 0),
    Piece Rook White (PositionVector 7 0),
    Piece Rook Black (PositionVector 0 7),
    Piece Knight Black (PositionVector 1 7),
    Piece Bishop Black (PositionVector 2 7),
    Piece Queen Black (PositionVector 3 7),
    Piece King Black (PositionVector 4 7),
    Piece Bishop Black (PositionVector 5 7),
    Piece Knight Black (PositionVector 6 7),
    Piece Rook Black (PositionVector 7 7)
    ]