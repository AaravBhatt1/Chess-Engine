module Types where

-- Each chess piece has a type, for example a pawn
-- Some of these store whether or not they have been moved yet because of special moves like castling
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

-- There are 2 colors that pieces can be, white or black. This also represents the player.
data Color = White | Black deriving (Show, Eq)

-- Returns the other color
getOtherColor :: Color -> Color
getOtherColor color = case color of
    White -> Black
    Black -> White

-- Each position is stored as a position vector
data Vector = Vector {
    xPos :: Int,
    yPos :: Int
} deriving (Show, Eq)

-- This adds 2 position vectors together
addPositionVector :: Vector -> Vector -> Vector
addPositionVector (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

-- This rotates a vector 90 degrees anticlockwise
rotateVector :: Vector -> Vector
rotateVector (Vector x y) = Vector y (-x)

-- This basically rotates a vector 90 degrees 4 times
getAllRotations :: Vector -> [Vector]
getAllRotations vector = take 4 $ iterate rotateVector vector

-- This reverses a vector so that we can get the previous move
inverseVector :: Vector -> Vector
inverseVector (Vector x y) = Vector (-x) (-y)

-- Each piece has a type, color, and position
data Piece = Piece {
    pieceType :: PieceType,
    pieceColor :: Color,
    piecePosition :: Vector
} deriving (Show, Eq)

-- This function returns the number of points a piece is worth
getPieceValue :: Piece -> Float
getPieceValue piece = case pieceType piece of
    Pawn -> 1
    Knight -> 3
    Bishop -> 3.5
    Rook -> 5
    Queen -> 9
    King -> 40

-- Returns true if a piece is owned by a player otherwise returns false
isOwnedBy :: Color -> Piece -> Bool
isOwnedBy playerColor piece = playerColor == pieceColor piece

-- A move consists of a piece's old position, new position, the resultant position, and the color that carried out the move to help display a move
data Move = Move {
    oldPiece :: Piece,
    newPiece :: Piece,
    newChessPos :: ChessPosition
} deriving (Show, Eq)

-- Gets the color that carried out a move
getMoveColor :: Move -> Color
getMoveColor (Move oldPiece newPiece newChessPos) = pieceColor oldPiece

-- We can call a list of moves, well, moves, and do the same for pieces
type Moves = [Move]
type Pieces = [Piece]
type ChessPosition = [Piece]
type ChessPositions = [ChessPosition]

-- Time to create a tree data type, a tree has a node and a list of branches that extend from it
data Tree a = Tree {
    node :: a,
    branches :: [Tree a]
} deriving (Show)

-- This cuts all branches of a tree after a tree after a set depth
-- This is so we can deal with the move tree being an infinite tree
cutTree :: Int -> Tree a -> Tree a
cutTree depth tree
    | depth == 0 = Tree move []
    | otherwise = Tree move cutBranches
    where
        move = node tree
        cutBranches = map (cutTree (depth - 1)) (branches tree)

