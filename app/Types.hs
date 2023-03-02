module Types where

-- Each chess piece has a type, for example a pawn
-- Some of these store whether or not they have been moved yet because of special moves like castling
data PieceType = Pawn Bool | Knight | Bishop | Rook | Queen | King deriving (Show)

-- There are 2 colors that pieces can be, white or black. This also represents the player.
data Color = White | Black deriving (Show, Eq)

-- Each position is stored as a position vector
data PositionVector = PositionVector {
    xPos :: Int,
    yPos :: Int
} deriving (Show, Eq)

-- Each piece has a type, color, and position
data Piece = Piece {
    pieceType :: PieceType,
    color :: Color,
    position :: PositionVector
} deriving (Show)

-- A move consists of a piece's old position, new position and the resultant position of the whole board to help display a move
data Move = Move {
    oldPiece :: Piece,
    newPiece :: Piece,
    newChessPos :: ChessPosition
} deriving (Show)

-- We can call a list of moves, well, moves, and do the same for pieces
type Moves = [Move]
type Pieces = [Piece]
type ChessPosition = [Piece]
type ChessPositions = [ChessPosition]

-- This is a tree, which is used to get all possble positions
data Tree a = Leaf a | Branches [a] deriving (Show)

