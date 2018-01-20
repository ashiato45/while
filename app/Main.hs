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

data TOperator = OAdd | OSub | OMult | ODiv
  deriving Show
operatorParser :: Parsec String st TOperator
operatorParser = do
  x <- oneOf "+-*/"
  return $ case x of
    '+' -> OAdd
    '-' -> OSub
    '*' -> OMult
    '/' -> ODiv

data TExpression = ENum TNumber | EBin TExpression TOperator TExpression
  deriving (Show)
expressionParser :: Parsec String st TExpression
expressionParser = 
  (do
      spaces
      char '('
      spaces
      a <- expressionParser
      spaces
      x <- operatorParser
      spaces
      b <- expressionParser
      spaces
      char ')'
      return $ EBin a x b)
  <|>
  (do
  x <- numberParser
  return $ ENum x)
  
-- expressionParser :: Parsec String st TExpression
-- expressionParser = do
--   x <- numberParser
  

main :: IO ()
main = someFunc
