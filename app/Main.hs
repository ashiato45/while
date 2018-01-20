module Main where

import Lib
import Text.Parsec
import Data.Ratio

test p = parse p ""

type TNumber = Integer
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
  
calcExpression :: TExpression -> Rational
calcExpression (ENum n) = n % 1
calcExpression (EBin a o b) =
  case o of
    OAdd -> (calcExpression a) + (calcExpression b)
    OSub -> (calcExpression a) - (calcExpression b)
    OMult -> (calcExpression a) * (calcExpression b)
    ODiv -> (calcExpression a) / (calcExpression b)

data TBoolOperator = BOAnd | BOOr
  deriving Show
data TCompareOperator = COEq | COLt | COLe | COGt | COGe | CONeq
  deriving Show
data TBool = BTrue
           | BFalse
           | BBin TBool TBoolOperator TBool
           | BNot TBool
           | BCompare TExpression TCompareOperator TExpression
  deriving Show
boolParser :: Parsec String st TBool
boolParser =
  try (do
          spaces
          char '!'
          spaces
          x <- boolParser
          return $ BNot x
      )
  <|>
  try (do
          spaces
          char '('
          spaces
          x <- boolParser
          spaces
          o <- oneOf "&|"
          spaces
          y <- boolParser
          spaces
          char ')'          
          let oo = case o of
                '&' -> BOAnd
                '|' -> BOOr
          return $ BBin x oo y
          )
  <|>
  try (do
          spaces
          char '('
          spaces
          x <- expressionParser
          spaces
          o <- try (string "==")
               <|> try (string "<=")
               <|> try (string "<")
               <|> try (string ">=")
               <|> try (string ">")
               <|> string "!="
          spaces
          y <- expressionParser
          spaces
          char ')'
          let oo = case o of
                "==" -> COEq
                "<=" -> COLe
                "<" -> COLt
                ">=" -> COGe
                ">" -> COGt
                "!=" -> CONeq
          return $ BCompare x oo y
  )
  <|>
  (do
      spaces
      x <- (string "TRUE") <|> (string "FALSE")
      return $ case x of
        "TRUE" -> BTrue
        "FALSE" -> BFalse
  )
           

main :: IO ()
main = someFunc
