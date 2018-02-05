module Main where

import Lib
import MyParser
import Text.Parsec
import Data.Ratio
-- import Data.Map
import Data.Map.Strict

test p = parse p ""

  
calcExpression :: Env -> TExpression -> Rational
calcExpression _ (ENum n) = n % 1
calcExpression e (EVar x) = e ! x
calcExpression e (EBin a o b) =
  case o of
    OAdd -> (calcExpression e a) + (calcExpression e b)
    OSub -> (calcExpression e a) - (calcExpression e b)
    OMult -> (calcExpression e a) * (calcExpression e b)
    ODiv -> (calcExpression e a) / (calcExpression e b)


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
