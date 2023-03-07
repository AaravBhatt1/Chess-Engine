module Types where

-- Each chess piece has a type, for example a pawn
-- Some of these store whether or not they have been moved yet because of special moves like castling
data PieceType = Pawn Bool | Knight | Bishop | Rook | Queen | King deriving (Eq)

instance Show PieceType where
    show pieceType = case pieceType of
        Pawn _ -> "Pawn"
        Knight -> "Knight"
        Bishop -> "Bishop"
        Rook -> "Rook"
        Queen -> "Queen"
        King -> "King"

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
    pieceColor :: Color,
    position :: PositionVector
} deriving (Show, Eq)

-- A move consists of a piece's old position, new position, the resultant position, and the color that carried out the move to help display a move
data Move = Move {
    oldPiece :: Piece,
    newPiece :: Piece,
    newChessPos :: ChessPosition,
    playerColor :: Color
} deriving (Show, Eq)

-- We can call a list of moves, well, moves, and do the same for pieces
type Moves = [Move]
type Pieces = [Piece]
type ChessPosition = [Piece]
type ChessPositions = [ChessPosition]

