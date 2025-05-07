module Main where

import Text.Regex.TDFA
-- import Debug.Trace

-- fun x -> x
-- x
-- x x

{-
How do you parse a function application expression like this?

  fun y -> y fun x -> x 10

(The intended parse tree is:)

  (fun y -> y (fun x -> x 10))

The algorithm is as follows:

- Split the parser in two; one that parses function application (AP),
  and one that parses other terms (TP).
- AP calls TP on the input; if there are still tokens left, treat
  those as the RHS in a function application term, otherwise return
- Since the RHS might contain further applications, recurse on the
  remaining tokens using AP

Remember that since we're dealing with a "flat" sequence of
tokens, the first token(s) will *always* be an operand!
-}

data Term
  = Fun String Term
  | Var String
  | App Term Term
  deriving (Show)

data Token
  = Symbol String
  | Fn
  | Arrow
  | Whitespace
  deriving (Show)

parse :: [Token] -> Either String (Term, [Token])
parse (Symbol x : rest) =
  Right (Var x, rest)
parse (Fn : Symbol x : Arrow : rest) =
  do
    (body, rest') <- parse rest
    Right (Fun x body, rest')
parse tokens = Left $ "Syntax error; unexpected: " ++ show tokens

parseApp :: [Token] -> Either String (Term, [Token])
parseApp tokens =
  do
    (lhs, rest) <- parse tokens
    case rest of
      [] -> Right (lhs, [])
      _ ->
        do
          (rhs, rest') <- parseApp rest
          Right (App lhs rhs, rest')
    -- (rhs, rest') <- parse rest
    -- Right (App lhs rhs, rest')

stringToToken :: String -> Maybe Token
stringToToken "fun" = Just Fn
stringToToken "->" = Just Arrow
stringToToken str | str =~ "[a-zA-Z_]+" = Just $ Symbol str
stringToToken str | str =~ "[:space:]+" = Just Whitespace
stringToToken _ = Nothing

scan :: [String] -> Either String [Token]
scan [] = Right []
scan (lexeme : rest) =
  case stringToToken lexeme of
    Just Whitespace -> scan rest
    Just t -> (t :) <$> scan rest
    Nothing -> Left $ "Unexpected " ++ show lexeme

scanRegex :: String
scanRegex = "fun|->|[a-zA-Z_]+|[:space:]+"

compile :: String -> Either String Term
compile input =
  do
    tokens <- scan $ getAllTextMatches (input =~ scanRegex)
    parseApp tokens
    >>= \(t, rest) -> 
      case rest of
        [] -> Right t
        _ -> Left $ "Unexpected remaining input" ++ show rest

main :: IO ()
main = putStrLn "Hello, Haskell!"
