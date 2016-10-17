-- Mantas Neviera, PS1, 5 užduotis
-- to load / :l ParseAndMove TicTacToe.Messages.Json
-- to run  / move TicTacToe.Messages.Json.message

module ParseAndMove
where

import Data.Char

data Move = Move {
    x :: Int
  , y :: Int
  , v :: Char
} deriving Show

instance Eq Move where
  a == b = (x a == x b) && (y a == y b)

type Moves = [Move]

move :: String -> Maybe Move
move message
    | n == 0 = Just (Move 1 1 'o')
    | n == 9 = Nothing
    | otherwise = Just (head nextPossibleMoves)
    where
      moves = parseMessage message
      n = length moves
      nextPlayer = case v (last moves) of
        'x' -> 'o'
        'o' -> 'x'
      nextPossibleMoves = [move | x <- [0..2], y <- [0..2], v <- [nextPlayer], let move = (Move x y v), move `notElem` moves]

parseMessage :: String -> Moves
parseMessage ('[':restMessage) = reverse $ parseJson [] formatedMessage
  where
    messageWithoutWhitespaces = filter (\l -> l /= ' ') restMessage
    formatedMessage = map toLower messageWithoutWhitespaces
parseMessage _ = error "This is not valid list"

parseJson :: Moves -> String -> Moves
parseJson allMoves "]" = allMoves
parseJson allMoves restMessage = parseJson (currentMove:allMoves) nextToParse
  where
    (currentMove, rest) = parseEntry restMessage
    nextToParse = case rest of
      (',':message) -> message
      message -> message

parseEntry :: String -> (Move, String)
parseEntry ('{':restMessage) =
   let
    (x, afterX) = readDigit $ skipKey restMessage
    (y, afterY) = readDigit $ skipKey $ skipSeparator afterX
    (p, rest) = readPlayer $ skipKey $ skipSeparator afterY
  in
    case rest of
      ('}':nextEntry) -> ((Move x y p), nextEntry)
      _ -> error "Entry without closing bracket"
parseEntry _ = error "This is not valid entry"

skipSeparator :: String -> String
skipSeparator (',':restMessage) = restMessage
skipSeparator _ = error "This is not valid separator"

skipKey :: String -> String
skipKey ('\"':'x':'\"':':': restMessage) = restMessage
skipKey ('\"':'y':'\"':':': restMessage) = restMessage
skipKey ('\"':'v':'\"':':': restMessage) = restMessage
skipKey _ = error "This is not valid key"

readDigit :: String -> (Int, String)
readDigit ('0':restMessage) = (0, restMessage)
readDigit ('1':restMessage) = (1, restMessage) 
readDigit ('2':restMessage) = (2, restMessage) 
readDigit _ = error "This is not valid digit" 

readPlayer :: String -> (Char, String)
readPlayer ('\"':'x':'\"': restMessage) = ('x', restMessage)
readPlayer ('\"':'o':'\"': restMessage) = ('o', restMessage)
readPlayer _ = error "This is not valid player"