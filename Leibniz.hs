module Main where

import System( getArgs )
import Prelude hiding (lookup)
import Data.Maybe
import Data.Map hiding (update, map, null)
import Text.Parsec.String
import Control.Monad.Error

-- http://en.wikipedia.org/wiki/Gottfried_Leibniz#Information_technology
-- http://notvincenz.blogspot.com/2008/01/simple-type-inference-in-haskell.html

import LeibnizExpr
import LeibnizParser
import LeibnizBuiltIns

errorMsg :: ParsedExpr -> String -> String
errorMsg parsedExpr msg = "Error: " ++ msg ++ ", at " ++ (show $ getPos parsedExpr) ++ "\n"

evaluateExpr :: Closure -> [ParsedExpr] -> ParsedExpr -> ParsedExpr
evaluateExpr closure stack parsedExpr = evaluateExpr' $ getExpr parsedExpr 
  where
    evaluateExpr' (Apply a b)                         = evaluateExpr closure (evaluateExpr closure [] b : stack) a
    evaluateExpr' (Constructor stuff name)            = newExpr (Constructor (stuff ++ stack) name) 
    evaluateExpr' (Id name)                           = maybe (evalError ("Id '" ++ name ++ "' not found in context: " ++ show closure ++ "\n")) (evaluateExpr closure stack) $ lookup name closure 
    evaluateExpr' (Undefined _)                       = parsedExpr -- to avoid problems if stack is not null
    evaluateExpr' (Literal (BuiltIn _ f)) 
                                   | not $ null stack = applyBuiltin f $ getExpr topOfStack 
    evaluateExpr' _                | null stack       = parsedExpr
    evaluateExpr' (FunctionDef x@(Equation patterns _) xs newClosure) 
                   | length stack >= length patterns  = fromMaybe (evalError "pattern match failure") $ firstJust $ map (matchEquation (union newClosure closure) stack) (x:xs)
    evaluateExpr' (FunctionDef _ _ _)                 = evalError "Not enough parameters to evaluate function"
    evaluateExpr' x                                   = evalError $ "Attempt to apply: " ++ (show . getExpr . head $ stack) ++ ", to nonfunction: " ++ show x ++ "\n"
    evalError = newExpr . Undefined . errorMsg parsedExpr
    topOfStack = evaluateExpr closure [] (head stack)
    applyBuiltin _ (Undefined _) = topOfStack
    applyBuiltin f _ = evaluateExpr closure (tail stack) (f topOfStack)
    newExpr = subsExpr parsedExpr

matchEquation :: Closure -> [ParsedExpr] -> Equation ParsedExpr -> Maybe ParsedExpr
matchEquation closure stack (Equation patterns expr)  = let evalRightSide newClosure = evaluateExpr (union newClosure closure) (drop (length patterns) stack) expr in 
   either Just (Just . evalRightSide . fromList =<<) (collectMatches $ zipWith (match []) patterns stack)
   
collectMatches :: (Error a) => [Either a (Maybe [b])] -> Either a (Maybe [b])
collectMatches = foldr (liftM2 $ liftM2 mplus) (Right $ Just [])

match :: [ParsedExpr] -> ParsedExpr -> ParsedExpr -> Either ParsedExpr (Maybe [(String, ParsedExpr)]) 
match stuff parsedPattern parsedExpr = match' (getExpr parsedPattern) (getExpr parsedExpr)
  where
    match' (Id name) _  = Right $ Just [(name, parsedExpr)]
    match' (Apply a b) _ = match (b:stuff) a parsedExpr 
    match' (Constructor _ a) (Constructor otherStuff b) | a == b = collectMatches $ zipWith (match []) stuff otherStuff
    match' _ (Undefined _) = Left parsedExpr
    match' pattern expr | pattern == expr = Right $ Just []
    match' _ _ = Right Nothing --pattern match fail

firstJust :: [Maybe a] -> Maybe a
firstJust (Nothing:xs) = firstJust xs
firstJust (x:_) = x
firstJust [] = Nothing

moduleToClosure :: ParsedModule -> Closure
moduleToClosure parsedModule = fromList [(name, value) | Definition name value <- map nodeExpr $ moduleDefs $ nodeExpr parsedModule]

evaluateMain :: ParsedModule -> Either String ParsedExpr
evaluateMain parsedModule = let topLevel = union (moduleToClosure parsedModule) builtIns in 
  (maybeToError "main function is not defined in module.\n" $ lookup "main" topLevel) >>= (Right . evaluateExpr topLevel [])

maybeToError :: e -> Maybe a -> Either e a 
maybeToError _ (Just a) = Right a
maybeToError e Nothing  = Left e

parseArg :: (Monad m) => [a] -> m a
parseArg [] = fail "Please pass in a file"
parseArg (x:_) = return x

main :: IO ()
main = do
   args <- getArgs
   fileName <- parseArg args
   parsedModuleOrError <- parseFromFile parseSource fileName
   either putStrLn print $ either (Left . show) Right parsedModuleOrError >>= evaluateMain

testParse :: IO ()
testParse = parseFromFile parseSource "test.v" >>= either print print
              
testEval :: IO ()
testEval = parseFromFile parseSource "test.v" >>= either print (print . evaluateMain)
          
