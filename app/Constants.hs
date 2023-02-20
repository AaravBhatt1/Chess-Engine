module Constants where

import Types

-- Moves a piece 1 square right
moveRight :: Movement
moveRight = Movement 1 0

-- Moves a piece 1 square left
moveLeft :: Movement
moveLeft = Movement (-1) 0

-- Moves a piece 1 square up
moveUp :: Movement
moveUp = Movement 0 1

-- Moves a piece 1 square down
moveDown :: Movement
moveDown = Movement 0 (-1) 

-- Moves a piece 1 square diagonally up and right
moveUpRight :: Movement
moveUpRight = Movement 1 1

-- Moves a piece 1 square diagonally up and left
moveUpLeft :: Movement
moveUpLeft = Movement (1) 1

-- Moves a piece 1 square diagonally down and right
moveDownRight :: Movement
moveDownRight = Movement 1 (-1)

-- Moves a piece 1 square diagonally down and left
moveDownLeft :: Movement
moveDownLeft = Movement (-1) (-1)
