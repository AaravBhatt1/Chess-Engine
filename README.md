# Chess Engine

## A chess AI built with haskell that users can play against.

This project is a chess engine that I have built using haskell, so that I could improve my functional programming skills. Users can play chess against the AI using a physical board or a board editor such as the [board editor on Lichess](https://lichess.org/editor), typing in their moves when prompted and moving pieces where the program tells it to for the AI.

Features include:

- Users can play chess using the app (however, some rules have been removed)
- Users can play against the chess engine, which plays accurately

## Demonstration

Demo coming soon

## How to use the chess program

To install and run the chess program, follow the instructions:

1. Clone this repository on your device
2. Run the `Chess.exe` file in `Chess-Engine/dist-newstyle/build/x86_64-windows/ghc-9.2.5/Chess-0.1.0.0/x/Chess/build/Chess/`

To then use the chess program, follow the instructions:

1. Set up a chess board or use a board editor such as the [board editor on Lichess](https://lichess.org/editor)
2. Run the `Chess.exe` file using the instructions above
3. When prompted to enter your move, move your piece on your chess board, then type your move in the format: `Move the pawn from e4 to e5`
4. Then, when the AI describes its move, make the move it describes on your chess board
5. Make sure to check the [limitations](#limitations-and-bugs) because this version of chess does not follow rules such as castling

## How this chess program works

### Decomposition of the Problem

This chess program is split into multiple parts:
 - The rules of chess
 - The AI that plays chess
 - The user IO management
 
### How I made the Rules of Chess in Haskell

In order to make the rules of chess, first, I made the basic data types needed to represent a chess piece. Each piece has a color which is either black or white, a type such as a bishop; and a position on the board. I used vectors to represent the position of the piece on the board because vectors are easy to manipulate. This allowed me to represent a chess board as a list of chess pieces that would be on the board. 

## Limitations and Bugs

There are some limitations to this chess AI, these include:

- The chess rules this program follows does not contain castling and en passant
- In order to win or lose, you need to capture the opponent's king with one of your pieces
- All pawns auto-promote to a queen
- The game currently doesn't detect draws

There are currently no bugs that I know of.

If you find a bug, please report it as an issue so I can add it to this section and possibly fix it.

If you want to fix a bug, feel free to request a merge.
