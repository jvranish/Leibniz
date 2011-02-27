
module LeibnizParser ( parseSource ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Control.Monad.Error
import Data.Map 

import LeibnizLexer
import LeibnizExpr
import LeibnizBuiltIns

parseNode :: Parser a -> Parser (SourceNode a)
parseNode parser = do 
             pos <- liftM fromSourcePos getPosition
             expr <- parser
             return $ SourceNode pos expr 0
             
parseExprNode :: Parser (Expr ParsedExpr) -> Parser ParsedExpr
parseExprNode parser = (return . ParsedExpr) =<< parseNode parser

parseFactor :: Parser ParsedExpr
parseFactor = liftM (foldl1 mkApply) (parens $ many1 parseExpr)
          <|> parseLiteral
          <|> parseLambda
          <?> "simple expression"
          
parseLiteral :: Parser ParsedExpr
parseLiteral = parseExprNode $ 
                  liftM (Constructor []) typeName
             <|>  liftM Id identifier
             <|>  liftM (Literal . Number . either toRational toRational) naturalOrFloat
             <|>  liftM (Literal . String) stringLiteral

parseLambda :: Parser ParsedExpr
parseLambda = parseExprNode $ do
  symbol "\\"
  equation <- parseEquation
  otherEquations <- many (symbol "|" >> parseEquation)
  return $ FunctionDef equation otherEquations empty

parseEquation :: Parser (Equation ParsedExpr)
parseEquation = do
  pattern <- many parseExpr
  comma
  expr <- parseExpr
  return $ Equation pattern expr
  
parseExpr :: Parser ParsedExpr
parseExpr = buildExpressionParser opTable parseFactor <?> "expression"

parseDefinition :: Parser (ParsedDefinition ParsedExpr)
parseDefinition = parseNode $ do
  name <-  identifier
  reservedOp "="
  expr <- parseExpr
  return $ Definition name expr

parseModule :: Parser ParsedModule
parseModule = parseNode $ do
  symbol "module"
  name <- typeName
  symbol "where"
  defs <- many parseDefinition
  return $ Module name defs
  
parseLex :: Parser a -> Parser a
parseLex p = do 
  whiteSpace
  x <- p
  eof
  return x
  
parseSource :: Parser ParsedModule
parseSource = parseLex parseModule
