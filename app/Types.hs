module Types where

-- Each chess piece has a type, color, position on the board labelled with x and y coordinates, and the last turn it was moved
data Piece = Piece {
    pieceType :: PieceType,
    pieceColor :: PieceColor,
    pieceX :: Int,
    pieceY :: Int
} deriving (Show, Eq)

-- This is the type that a piece can be
data PieceType = Pawn Bool | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

-- Pieces are either white or black
data PieceColor = White | Black deriving (Show, Eq)

-- This is a movement vector that can be added to a piece's position to move it
data Movement = Movement {
    movementX :: Int,
    movementY :: Int
} 
