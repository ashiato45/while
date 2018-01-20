module Main where

import Lib
import Text.Parsec

test p = parse p ""

type TNumber = Int
numberParser:: Parsec String st TNumber
-- numberParser = read <$> (many $ oneOf "0123456789")
numberParser = do
  x <- many $ oneOf "0123456789"
  return $ read x

data TOperator = TAdd | TSub | TMult | TDiv
  deriving Show
operatorParser :: Parsec String st TOperator
operatorParser = do
  x <- oneOf "+-*/"
  return $ case x of
    '+' -> TAdd
    '-' -> TSub
    '*' -> TMult
    '/' -> TDiv

main :: IO ()
main = someFunc
