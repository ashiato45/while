module MyParser
    ( programParser
    ) where
import Lib
import Text.Parsec
import Data.Ratio
-- import Data.Map
import Data.Map.Strict


chars_variable = "abcdefghijklmnopqrstuvwxyz'_"

numberParser:: Parsec String st TNumber
-- numberParser = read <$> (many $ oneOf "0123456789")
numberParser = do
--  x <- many $ oneOf "0123456789"
  x <- many1 $ digit  
  return $ read x

operatorParser :: Parsec String st TOperator
operatorParser = do
  x <- oneOf "+-*/"
  return $ case x of
    '+' -> OAdd
    '-' -> OSub
    '*' -> OMult
    '/' -> ODiv

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

