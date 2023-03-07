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