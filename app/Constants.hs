module Constants where

import Types

-- All single step vertical and horizontal moves
singleAxial :: [Movement]
singleAxial = [Movement 1 0, Movement (-1) 0, Movement 0 1, Movement 0 (-1)]

-- All single step diagonal moves
singleDiagonal :: [Movement]
singleDiagonal = [Movement 1 1, Movement 1 (-1), Movement (-1) 1, Movement (-1) (-1)]

-- All knight moves
knightMoves :: [Movement]
knightMoves = [Movement 2 1, Movement 2 (-1), Movement 1 2, Movement 1 (-1), Movement (-1) 2, Movement (-1) (-2), Movement (-2) 1, Movement (-2) (-1)]

-- The depth of the AI
-- If this is too high, the AI plays too safe
-- If it is too low, the AI is stupid
depth :: Int
depth = 3