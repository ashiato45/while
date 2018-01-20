module Main where

import Lib
import Text.Parsec
import Data.Ratio
-- import Data.Map
import Data.Map.Strict

test p = parse p ""


chars_variable = "abcdefghijklmnopqrstuvwxyz'_"

type TVar = String
type TNumber = Integer
type Env = Map TVar Rational

numberParser:: Parsec String st TNumber
-- numberParser = read <$> (many $ oneOf "0123456789")
numberParser = do
--  x <- many $ oneOf "0123456789"
  x <- many1 $ digit  
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

data TExpression = ENum TNumber | EVar TVar | EBin TExpression TOperator TExpression
  deriving (Show)
expressionParser :: Parsec String st TExpression
expressionParser =
  try
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
  try (do
  x <- numberParser
  return $ ENum x)
  <|>
  (do
      spaces
      x <- many1 $ oneOf chars_variable
      return $ EVar x
  )
  
calcExpression :: Env -> TExpression -> Rational
calcExpression _ (ENum n) = n % 1
calcExpression e (EVar x) = e ! x
calcExpression e (EBin a o b) =
  case o of
    OAdd -> (calcExpression e a) + (calcExpression e b)
    OSub -> (calcExpression e a) - (calcExpression e b)
    OMult -> (calcExpression e a) * (calcExpression e b)
    ODiv -> (calcExpression e a) / (calcExpression e b)

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


calcBool :: Env -> TBool -> Bool
calcBool _ BTrue = True
calcBool _ BFalse = False
calcBool e (BNot b) = not $ calcBool e b
calcBool e (BBin a o b) = (oo o) (calcBool e a) (calcBool e b)
  where oo BOAnd = (&&)
        oo BOOr = (||)
calcBool e (BCompare a o b) = (oo o) (calcExpression e a) (calcExpression e b)
  where oo COEq = (==)
        oo COLt = (<)
        oo COLe = (<=)
        oo COGt = (>)
        oo COGe = (>=)
        oo CONeq = (/=)


data TProgram = PExit
  | PError
  | PAssign TVar TExpression
  | PNext TProgram TProgram
  | PWhile TBool TProgram
  | PIf TBool TProgram
  | PIfElse TBool TProgram TProgram
  deriving Show

programParserHelp :: Parsec String st TProgram
programParserHelp =
  try (do
          spaces
          string "WHILE"
          spaces
          char '['
          spaces
          b <- boolParser
          spaces
          char ']'
          spaces
          char '{'
          spaces
          x <- programParser
          spaces
          char '}'
          return $ PWhile b x
          )
  <|>
  try (do
          spaces
          string "IF"
          spaces
          char '['
          spaces
          b <- boolParser
          spaces
          char ']'
          spaces
          char '{'
          spaces
          x <- programParser
          spaces
          char '}'
          spaces
          string "ELSE"
          spaces
          char '{'
          spaces
          y <- programParser
          spaces
          char '}'
          return $ PIfElse b x y
          )
  <|>
  try (do
          spaces
          string "IF"
          spaces
          char '['
          spaces
          b <- boolParser
          spaces
          char ']'
          spaces
          char '{'
          spaces
          x <- programParser
          spaces
          char '}'
          return $ PIf b x
          )
  <|>
  try (do
          spaces
          x <- many1 $ oneOf chars_variable
          spaces
          char '='
          spaces
          e <- expressionParser
          return $ PAssign x e
          )
  <|>
  try (do
          spaces
          x <- try (string "EXIT") <|> (string "ERROR")
          return $ case x of
            "EXIT" -> PExit
            "ERROR" -> PError
          )


programParser :: Parsec String st TProgram
programParser =
    try (do
          spaces
          x <- programParserHelp
          spaces
          char ';'
          spaces
          y <- programParser
          return $ PNext x y
          )
  <|>
  programParserHelp


type TSafe = Bool
type THalt = Bool
calcProgramHelp :: Env -> TProgram -> (TSafe, THalt, Env)
calcProgramHelp e PExit = (True, True, e)
calcProgramHelp e PError = (False, True, e)
calcProgramHelp e (PNext a b) =
  let (s', h', e') = calcProgramHelp e a
  in case h' of
    False -> calcProgramHelp e' b
    True -> (s', True, e')
calcProgramHelp e (PAssign v exp) = (False, False, insert v (calcExpression e exp) e)
calcProgramHelp e (PIf b p) = case (calcBool e b) of
  True -> calcProgramHelp e p
  False -> (False, False, e)
calcProgramHelp e (PIfElse b p1 p2) = case (calcBool e b) of
  True -> calcProgramHelp e p1
  False -> calcProgramHelp e p2
calcProgramHelp e (PWhile b p) = 
  let p' = PIf b (PNext p (PWhile b p))
  in calcProgramHelp e p'


calcProgram :: Env -> TProgram -> (TSafe, Env)
calcProgram e p = let (s', _, e') = calcProgramHelp e p
              in (s', e')

    

main :: IO ()
main = someFunc
