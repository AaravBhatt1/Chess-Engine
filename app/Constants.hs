module Constants where

import Types

-- All single step vertical and horizontal moves
singleAxial :: [Movement]
singleAxial = [Movement 1 0, Movement (-1) 0, Movement 0 1, Movement 0 (-1)]

-- All single step diagonal moves
singleDiagonal :: [Movement]
singleDiagonal = [Movement 1 1, Movement 1 (-1), Movement (-1) 1, Movement (-1) (-1)]