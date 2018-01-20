module Main where

import Lib
import Text.Parsec

type TNumber = Int
numberParser:: Parsec String st TNumber
-- numberParser = read <$> (many $ oneOf "0123456789")
numberParser = do
  x <- many $ oneOf "0123456789"
  return $ read x

main :: IO ()
main = someFunc
