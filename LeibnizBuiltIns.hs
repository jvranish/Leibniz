
module LeibnizBuiltIns (builtIns, opTable, Closure) where

import Data.Map hiding (map)
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Expr
import Text.Parsec.String
import Control.Monad.Identity

import LeibnizLexer
import LeibnizExpr

operators :: [[((String, ParsedExpr), Operator String () Identity ParsedExpr)]]
operators = [ [defInfix AssocLeft "+" add, defInfix AssocLeft "-" sub]
            , [defInfix AssocLeft "*" mul]
            ]

functions :: [(String, ParsedExpr)]
functions = [ -- defBuiltIn2 "fname" f
            ]

add :: Expr ParsedExpr -> Expr ParsedExpr -> Expr ParsedExpr
add (Literal (Number a)) (Literal (Number b)) = Literal $ Number $ a + b
add a b = evalTypeError2 "+" a b

sub :: Expr ParsedExpr -> Expr ParsedExpr -> Expr ParsedExpr
sub (Literal (Number a)) (Literal (Number b)) = Literal $ Number $ a - b
sub a b = evalTypeError2 "+" a b

mul :: Expr ParsedExpr -> Expr ParsedExpr -> Expr ParsedExpr
mul (Literal (Number a)) (Literal (Number b)) = Literal $ Number $ a * b
mul a b = evalTypeError2 "*" a b

evalTypeError2 :: (Show a, Show b) => String -> a -> b -> Expr c
evalTypeError2 name a b = Undefined $ "Type error with " ++ name ++ ": a = " ++ show a ++ ", b = " ++ show b ++ "\n"

opTable :: [[Operator String () Identity ParsedExpr]]
opTable = map (map snd) operators

type Closure = Map String ParsedExpr

builtIns :: Closure
builtIns = fromList $ functions ++ map fst (concat operators)

defInfix :: Assoc -> String -> (Expr ParsedExpr -> Expr ParsedExpr -> Expr ParsedExpr) -> ( (String, ParsedExpr), Operator String () Identity ParsedExpr )
defInfix assoc name f = (defBuiltIn2 name f, Infix (parseOperator name) assoc)

applyInfix :: String -> SourcePosition -> ParsedExpr -> ParsedExpr -> ParsedExpr
applyInfix name pos = mkApply . mkApply (mkParsedExpr pos (Id name))

parseOperator :: String -> Parser (ParsedExpr -> ParsedExpr -> ParsedExpr)
parseOperator name = do
  pos <- liftM fromSourcePos getPosition
  reservedOp name
  return $ applyInfix name pos

defBuiltIn2 :: String -> (Expr ParsedExpr -> Expr ParsedExpr -> Expr ParsedExpr) -> (String, ParsedExpr)
defBuiltIn2 name f = (name, mkBuiltIn2 name (wrapExprFunc2 f))

mkBuiltIn :: String -> (ParsedExpr -> ParsedExpr) -> ParsedExpr
mkBuiltIn name = mkParsedExpr (fromSourcePos $ initialPos "Built in function") . Literal . BuiltIn name
mkBuiltIn2 :: String -> (ParsedExpr -> ParsedExpr -> ParsedExpr) -> ParsedExpr
mkBuiltIn2 name f = mkBuiltIn name (\a -> mkBuiltIn name (f a))

--wrapExprFunc :: (Expr ParsedExpr -> SourceNode (Expr ParsedExpr)) -> ParsedExpr -> ParsedExpr
--wrapExprFunc f a = ParsedExpr $ f (getExpr a)

wrapExprFunc2 :: (Expr ParsedExpr -> Expr ParsedExpr -> Expr ParsedExpr) -> ParsedExpr -> ParsedExpr -> ParsedExpr
wrapExprFunc2 f a b = subsExpr a (f (getExpr a) (getExpr b))





                      