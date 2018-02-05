module Lib
    ( someFunc,
      TVar,
      TNumber,
      Env,
      TOperator(..),
      TExpression(..),
      TBoolOperator(..),
      TCompareOperator(..),
      TBool(..),
      TProgram(..),
      TSafe,
      THalt
    ) where

import Data.Map.Strict

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type TVar = String
type TNumber = Integer
type Env = Map TVar Rational

data TOperator = OAdd | OSub | OMult | ODiv
  deriving Show

data TExpression = ENum TNumber | EVar TVar | EBin TExpression TOperator TExpression
  deriving (Show)

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

data TProgram = PExit
  | PError
  | PAssign TVar TExpression
  | PNext TProgram TProgram
  | PWhile TBool TProgram
  | PIf TBool TProgram
  | PIfElse TBool TProgram TProgram
  deriving Show

type TSafe = Bool
type THalt = Bool
