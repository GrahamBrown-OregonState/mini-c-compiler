module Scanner where

import Data.Char
import Data.List

-- token data type
data Token
  = TElse
  | TIf
  | TInt
  | TReturn
  | TVoid
  | TWhile
  | TPlus
  | TMinus
  | TMult
  | TDiv
  | TLessThanEq
  | TLessThan
  | TGreaterThanEq
  | TGreaterThan
  | TEqEq
  | TNotEq
  | TEq
  | TSemiCol
  | TComma
  | TLParen
  | TRParen
  | TLeftBrack
  | TRightBrack
  | TLeftCurly
  | TRightCurly
  | TId String
  | TNum Int
  | Error String
  deriving (Eq, Show)

-- scanner function
scan :: String -> [Token]
scan [] = []
-- scan for comments
scan ('/' : '*' : xs) = scan (dropComment xs)
-- scan multi-char operators
scan ('<' : '=' : xs) = TLessThanEq : scan xs
scan ('>' : '=' : xs) = TGreaterThanEq : scan xs
scan ('=' : '=' : xs) = TEqEq : scan xs
scan ('!' : '=' : xs) = TNotEq : scan xs
scan (x : xs)
  -- scan for blank space
  | isWhitespace x = scan xs
  -- scan for operator symbols
  | x == '+' = TPlus : scan xs
  | x == '-' = TMinus : scan xs
  | x == '*' = TMult : scan xs
  | x == '/' = TDiv : scan xs
  | x == '=' = TEq : scan xs
  | x == ';' = TSemiCol : scan xs
  | x == ',' = TComma : scan xs
  | x == '(' = TLParen : scan xs
  | x == ')' = TRParen : scan xs
  | x == '[' = TLeftBrack : scan xs
  | x == ']' = TRightBrack : scan xs
  | x == '{' = TLeftCurly : scan xs
  | x == '}' = TRightCurly : scan xs
  -- identifiers and keywords
  | isAlpha x =
      let (word, rest) = span isAlpha (x : xs)
       in classify word : scan rest
  -- numbers
  | isDigit x =
      let (digits, rest) = span isDigit (x : xs)
       in TNum (read digits) : scan rest
  -- handle non-regonized characters
  | otherwise = (Error ("Unexpected character: " ++ [x])) : scan xs

-- classify a word as keyword or identifier
classify :: String -> Token
classify "else" = TElse
classify "if" = TIf
classify "int" = TInt
classify "return" = TReturn
classify "void" = TVoid
classify "while" = TWhile
classify s = TId s

-- returns true/false based on if the character is whitespace
isWhitespace :: Char -> Bool
isWhitespace (x)
  | x == ' ' || x == '\t' || x == '\n' = True
  | otherwise = False

-- strip string of comments after triggered by scan
dropComment :: String -> String
dropComment [] = "Error: Unclosed comment"
dropComment ('*' : '/' : xs) = xs
dropComment (x : xs) = dropComment xs

-- strip string until next whitespace and return it
findId :: String -> String
findId [] = []
findId (x : xs)
  | isAlpha x = x : findId xs
  | otherwise = []

findNum :: String -> Int
findNum s =
  let (digits, _) = span isDigit s
   in read digits

removeFirstId :: String -> String -> String
removeFirstId id fullString
  | id `isPrefixOf` fullString = drop (length id) fullString
  | otherwise = fullString

removeFirstNum :: Int -> String -> String
removeFirstNum num fullString
  | show num `isPrefixOf` fullString = drop (length (show num)) fullString
  | otherwise = fullString