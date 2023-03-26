import Data.List (intercalate) -- Part of Q2
import Data.Maybe (isJust) -- part of Q4
import Control.Concurrent.Chan (Chan, readChan, writeChan) --part of Q5
import Text.Read (readMaybe) --part of Q5

--part of Q6
import Data.Time.Clock.System
import Data.Bits

--part of Q7
import Data.Maybe (fromJust)
import Data.List (transpose)
import Data.Char (intToDigit)

--part of Q8
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Monad (forever, when)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)


-- Q1 - Define appropriate types for a Board, a Move, and an Outcome. You can
-- use algebraic types or type synonyms — this is your choice.
data Player = X | O deriving (Eq, Show)
type Row = Int
type Col = Char
type Position = (Row, Col)
type Board = [(Position, Maybe Player)]
type Move = Position
data Outcome = Win Player | Draw | Continue deriving (Eq, Show)





-- Q2 Define a function printBoard :: Board -> IO() that prints the board
-- in a human-readable format.
printBoard :: Board -> IO ()
printBoard board = putStrLn $ intercalate "\n" rows
  where
    rows = map showRow [1..3]
    showRow row = intercalate " | " [showPlayer (row, col) | col <- ['A'..'C']]
    showPlayer pos = case lookup pos board of
                      Just (Just player) -> show player
                      _ -> " "

--This is required to work properly
--write 'printBoard board' to see the board in a human-readable format
--You can change the position of the X and Os with the board variable below
--If you want to change the symbols on the board...
--switch 'Nothing' with either 'Just X' or 'Just O'
board :: Board
board = [((1, 'A'), Nothing), ((1, 'B'), Nothing), ((1, 'C'), Nothing),
         ((2, 'A'), Nothing), ((2, 'B'), Nothing), ((2, 'C'), Nothing),
         ((3, 'A'), Nothing), ((3, 'B'), Nothing), ((3, 'C'), Nothing)]





-- Q3 - Define a function makeMove :: Board -> Player -> Move -> Maybe Board that
-- returns a new board with the given move applied if it is valid, or Nothing if
-- the move is invalid.
makeMove :: Board -> Player -> Move -> Maybe Board
makeMove board player move
  | isMoveValid board move = updateBoard board move player
  | otherwise = Nothing


-- Helper function to check if a move is valid
isMoveValid :: Board -> Move -> Bool
isMoveValid board move =
  case lookup move board of
    Just Nothing -> True
    _ -> False





-- Q4: Define a function updateBoard :: Board -> Move -> Player -> Maybe Board
-- that takes the current board, a proposed move and a player and returns the new
-- board if the move is valid, otherwise Nothing.
updateBoard :: Board -> Move -> Player -> Maybe Board
updateBoard board move player
  | move `notElem` positions = Nothing
  | isJust $ lookup move board = Nothing
  | otherwise = Just $ (move, Just player) : board
  where
    positions = [(r, c) | r <- [1..3], c <- ['A'..'C']]

--Q5: Define the human player. When a move is requested from the human
-- player, the board should be on display, and a move should be read from
-- the keyboard. If the move is illegal an appropriate error message should
-- be printed, and we try again. If the move is legal it should be send back
-- on the player’s output channel.

humanPlayer :: Player -> Chan Board -> Chan Move -> IO ()
humanPlayer player inputChan outputChan = do
  putStrLn $ "It's " ++ show player ++ "'s turn."
  -- Display the current board
  board <- readChan inputChan
  printBoard board
  -- Ask for a move
  putStrLn "Enter a move (e.g. 1A): "
  moveStr <- getLine
  case parseMove moveStr of
    Just move -> do
      -- Check if the move is valid
      case makeMove board player move of
        Just newBoard -> do
          -- Send the move to the game manager
          writeChan outputChan move
          -- Wait for the game manager to update the board
          _ <- readChan inputChan
          return ()
        Nothing -> do
          putStrLn "Invalid move!"
          humanPlayer player inputChan outputChan
    Nothing -> do
      putStrLn "Invalid input!"
      humanPlayer player inputChan outputChan

-- Helper function to parse move strings (e.g. "1A")
parseMove :: String -> Maybe Move
parseMove [r,c] = do
  row <- readMaybe [r]
  col <- if c `elem` ['A'..'C'] then Just c else Nothing
  return (row, col)
parseMove _ = Nothing

--Q6 Define a bot player. This player should make some legal move by just
-- investigating the board. There should be an element of randomness (use
-- the PickRandom module on moodle), unless there is only one “good” move.
-- The bot does not have to be clever, but it could be.
botPlayer :: Player -> Chan Board -> Chan Move -> IO ()
botPlayer player inputChan outputChan = do
  -- Wait for the current board
  board <- readChan inputChan
  -- Choose a random move
  let moves = [(r, c) | r <- [1..3], c <- ['A'..'C']]
      validMoves = filter (\pos -> isMoveValid board pos) moves
  case validMoves of
    [] -> error "No valid moves"
    [move] -> do
      -- Send the move to the game manager
      writeChan outputChan move
      -- Wait for the game manager to update the board
      _ <- readChan inputChan
      return ()
    _ -> do
      -- Choose a random valid move
      randomMove <- pickRandom validMoves
      -- Send the move to the game manager
      writeChan outputChan randomMove
      -- Wait for the game manager to update the board
      _ <- readChan inputChan
      return ()


pickRandom :: [a] -> IO a
pickRandom [] = undefined
pickRandom [x] = return x
pickRandom xs = do
   t <- getSystemTime
   let n = systemNanoseconds t
   let nn = fromIntegral(shiftR (n * 123456789) 16) -- some kind of multiplicative hashing
   let m = length xs
   return (xs!! (mod nn m))




--Q7 Define the game managing process behavior. This needs to keep track
-- of the board position, the players’ input channels and whose turn it is. It
-- should keep inviting the players in turn to make their moves, and announce
-- a winner (or a draw) when the situation occurs.
checkWin :: Board -> Maybe Outcome
checkWin board
  | isWin board X = Just $ Win X
  | isWin board O = Just $ Win O
  | isDraw board = Just Draw
  | otherwise = Nothing

-- Helper function to check if the board is in a winning state for a player
isWin :: Board -> Player -> Bool
isWin board player =
  any (\col -> all (\row -> isOccupiedByPlayer board player (toEnum (fromEnum 'A' + fromEnum row - fromEnum '1'), toEnum (fromEnum '1' + fromEnum col - fromEnum '1'))) ['A'..'C']) [1..3]
  || any (\row -> all (\col -> isOccupiedByPlayer board player (toEnum (fromEnum 'A' + fromEnum row - fromEnum '1'), toEnum (fromEnum '1' + fromEnum col - fromEnum '1'))) ['1'..'3']) ['A'..'C']
  || all (\idx -> isOccupiedByPlayer board player (toEnum (fromEnum 'A' + idx - 1), toEnum (fromEnum '1' + idx - 1))) [1..3]
  || all (\idx -> isOccupiedByPlayer board player (toEnum (fromEnum 'A' + idx - 1), toEnum (fromEnum '3' - idx + 1))) [1..3]


-- Helper function to check if a cell is occupied by the given player
isOccupiedByPlayer :: Board -> Player -> Position -> Bool
isOccupiedByPlayer board player pos =
  case lookup pos board of
    Just (Just p) -> p == player
    _ -> False

-- Helper function to check if the game is a draw
isDraw :: Board -> Bool
isDraw board = all isJust $ map snd board




--Q8 Define a game start. This is an IO action parameterized by two players.
-- This action should create the necessary channels, fork the player processes,
-- and then continue managing the game. The purpose of the two parameters
-- is to allow different match-ups (humans or bots vs. humans or bots).

