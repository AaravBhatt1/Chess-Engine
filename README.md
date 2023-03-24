# Chess Engine

## A chess AI built with haskell that users can play against.

This project is a chess engine that I have built using haskell, so that I could improve my functional programming skills. Users can play chess against the AI using a physical board or a board editor such as the [board editor on Lichess](https://lichess.org/editor), typing in their moves when prompted and moving pieces where the program tells it to for the AI.

Features include:

- Users can play chess using the app
- Users can play against the chess engine, which plays accurately
- Users can choose the difficulty of the chess engine

## Demonstration

![Haskell Chess Opening Demo](https://user-images.githubusercontent.com/64959071/227003166-c572bef6-9f13-42d5-9980-46bf81610104.gif)

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

In order to make the rules of chess, I first made the basic data types needed to represent a chess piece. Each piece has a color which is either black or white, a type such as a bishop or rook; and a position on the board. I used vectors to represent the position on the board because vectors are easy to manipulate. This allowed me to represent a chess board as a list of chess pieces that would be on the board. I created functions to check for collisions and whether a piece is on the board and then used these to find all the possible moves for each piece. I utilized patterns in the ways in which the pieces move, for example, bishops and rooks move in similiar ways.

### How I made the AI

To make the AI, I first created an evaluation function that judges which player is winning and by how much. This mainly used the number of points each player had, calculated using the number and types of pieces they own. But I additionally made the chess engine favour moving pieces closer to the centre of the board during the opening, and pushing pawns to the back rank and forcing the opponent's king to the corner at the endgame. Another important function for the AI was the function that created a tree of possible moves that could branch out from 1 move. Because haskell is lazy compiled, I made this an infinite tree which can be cut with a different function at a certain depth. This would then allow the user to choose the depth of the AI. From this tree of moves, we can then find the best move by working back from the deepest moves and finding the move that provides the best evaluation for each player at the end.

### How I managed user input and output

For this project, I broke down the input and output (IO) into different parts, such as inputting a move, but the most important part was the move loop which essentally acted as the game loop that inputted the user's move for a turn and then outputted the engine's move. Managing the user IO involved lots of recursion. For example, whenever text needed to be inputted, I had to create a recursive function that would ask them to input the text again until their input is valid. When the main function is called, the move loop is started after asking the user what difficulty they want to play against.

## Limitations and Bugs

There are some limitations to this chess AI, these include:

- The chess rules this program follows does not contain castling and en passant
- In order to win or lose, you need to capture the opponent's king with one of your pieces
- All pawns auto-promote to a queen
- The game currently doesn't detect draws

There are currently no bugs that I know of.

If you find a bug, please report it as an issue so I can add it to this section and possibly fix it.

If you want to fix a bug, feel free to request a merge.
