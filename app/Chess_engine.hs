module Chess_engine where

import Types
import Constants
import Chess_rules
import Data.List

-- Generates a tree of possible moves from a single move and the color that made that move
generateMoveTree :: Move -> Tree Move
generateMoveTree move
    | checkGameOver newPos = Tree move []
    | otherwise = Tree move newMoveTrees
    where
        newPos = newChessPos move
        otherColor = getOtherColor $ getMoveColor move
        newMoveTrees = map generateMoveTree (getAllMoves otherColor newPos)

-- Gets the evaluation for the scenario where each player makes the best possible move, working backwards
getMoveEval :: Tree Move -> Float
getMoveEval moveTree
        | null $ branches moveTree = getPositionEval $ newChessPos $ node moveTree
        | (getMoveColor $ node moveTree) == White = minimum $ map (\branch -> getMoveEval branch + 0.01) $ branches moveTree -- 0.01 is added so it favours the fastest checkmate
        | (getMoveColor $ node moveTree) == Black = maximum $ map (\branch -> getMoveEval branch + 0.01) $ branches moveTree

-- Gets the best move by looking through the move evaluations and fincing the best one
getBestMove :: Tree Move -> Move
getBestMove moveTree = case index of
    Just i -> node (branches moveTree !! i)
    where
        moveEvals = map getMoveEval (branches moveTree)
        index = findIndex (== getMoveEval moveTree - 0.01) moveEvals

-- Gets the engine evaluation of a position
-- It is mainly based on the number of points each player has
-- In the opening stage of the game, it favours positions where pieces are active near the centre of the board to avoid it moving the "a" pawn
-- In the endgame stage of the game, it favours pushing pawns to the end of the board and forcing the opponent's king to the corner
getPositionEval :: ChessPosition -> Float
getPositionEval chessPosition = totalPoints + openingWeightWhite - openingWeightBlack + endgameKingWeightWhite - endgameKingWeightBlack + endgamePawnWeightWhite - endgamePawnWeightBlack
    where
        totalPoints = getTotalPoints chessPosition
        openingWeightWhite = if isOpening White chessPosition then 0.1 * (sum $ map getDistanceFromMiddle $ filter (isOwnedBy Black) chessPosition) else 0
        openingWeightBlack = if isOpening Black chessPosition then 0.1 * (sum $ map getDistanceFromMiddle $ filter (isOwnedBy White) chessPosition) else 0
        endgameKingWeightWhite = if (isEndgame White chessPosition) && (totalPoints > 0) && (not $ checkGameOver chessPosition) then 0.1 * (getDistanceFromMiddle (filter (\piece -> isOwnedBy Black piece && pieceType piece == King) chessPosition !! 0)) else 0
        endgameKingWeightBlack = if (isEndgame Black chessPosition) && (totalPoints < 0) && (not $ checkGameOver chessPosition) then 0.1 * (getDistanceFromMiddle (filter (\piece -> isOwnedBy White piece && pieceType piece == King) chessPosition !! 0)) else 0
        endgamePawnWeightWhite = if isEndgame White chessPosition then 0.2 * (sum $ map (\pawn -> fromIntegral $ yPos $ piecePosition pawn) $ filter (\piece -> isOwnedBy White piece && pieceType piece == Pawn) chessPosition) else 0
        endgamePawnWeightBlack = if isEndgame White chessPosition then 0.2 * (sum $ map (\pawn -> 8 - (fromIntegral $ yPos $ piecePosition pawn)) $ filter (\piece -> isOwnedBy Black piece && pieceType piece == Pawn) chessPosition) else 0

-- This function returns the total number of points a white has, which can be negative if black is winning
-- This helps us to judge who is winning in a position
getTotalPoints :: ChessPosition -> Float
getTotalPoints allPieces = totalWhitePoints - totalBlackPoints
    where
        totalWhitePoints = sum $ map (getPieceValue) $ filter (isOwnedBy White) allPieces 
        totalBlackPoints = sum $ map (getPieceValue) $ filter (isOwnedBy Black) allPieces

-- This returns true if at least 11 pieces are on the board, which is what I am counting as the opening
isOpening :: Color -> ChessPosition -> Bool
isOpening playerColor chessPosition = (length $ filter (isOwnedBy playerColor) chessPosition) > 13

-- This returns true if there is less than 6 pieces on the board
isEndgame :: Color -> ChessPosition -> Bool
isEndgame playerColor chessPosition = (length $ filter (isOwnedBy (getOtherColor playerColor)) chessPosition) < 6

-- This gets the distance of a piece from the middle of the board
getDistanceFromMiddle :: Piece -> Float
getDistanceFromMiddle (Piece pieceType pieceColor (Vector x y)) = sqrt ((3.5 - fromIntegral x) ** 2 + (3.5 - fromIntegral y) ** 2)