
module LeibnizExpr ( Constant (..)
                    , Equation (..)
                    , Expr (..)
                    , Definition (..)
                    , Module (..)
                    , ParsedDefinition
                    , ParsedModule
                    , ParsedExpr (..)
                    , SourceNode (..)
                    , SourcePosition (..)
                    , fromSourcePos
                    , toSourcePos
                    , getPos
                    , getExpr
                    , subsExpr
                    , mkParsedExpr
                    , mkApply
                    ) where
                    
import Text.Parsec.Pos
import Control.Monad.Error
import Data.Map

data Constant = Number Rational 
              | String String -- perhaps replace with some sort of built in list?
              | BuiltIn String (ParsedExpr -> ParsedExpr) -- (name, f)

instance Eq Constant where
  (==) (Number a) (Number b) = a == b
  (==) (String a) (String b) = a == b
  (==) (BuiltIn a _) (BuiltIn b _)= a == b
  (==) _ _ = False

instance Show Constant where
  show (Number x) = show x
  show (String x) = show x
  show (BuiltIn name _) = "BuiltIn " ++ name

data Equation a = Equation [a] a deriving (Show, Eq)
data Definition a = Definition String a deriving (Show, Eq)
data Module a = Module 
              { moduleName :: String
              , moduleDefs :: [ParsedDefinition a]
              } deriving (Show, Eq)

-- data ExprType = AnyType
--               | ExprType deriving (Show, Eq)

type ExprType = Int

-- because parsec doesn't export constructors so I can't make data and typeable instances
data SourcePosition  = SourcePosition String !Int !Int
    deriving ( Eq, Ord )
    
instance Show SourcePosition where
  show x = show $ toSourcePos x
    
fromSourcePos x = SourcePosition (sourceName x) (sourceLine x) (sourceColumn x)
toSourcePos (SourcePosition name line column) = newPos name line column

data SourceNode a = SourceNode 
                  { nodePos  :: SourcePosition
                  , nodeExpr :: a 
                  , nodeType :: ExprType
                  } deriving Eq
instance (Show a) => Show (SourceNode a) where
  show = show . nodeExpr

type ParsedDefinition a = SourceNode (Definition a)
type ParsedModule = SourceNode (Module ParsedExpr)

newtype ParsedExpr = ParsedExpr (SourceNode (Expr ParsedExpr)) deriving (Eq)
instance Show ParsedExpr where
  show (ParsedExpr a) = show a
-- A haskell wart, need to be a member of Error class to use (Either ParsedExpr) as a monad
--  even though I wont be using these methods,.. ever...
instance Error ParsedExpr where
  noMsg    = undefined
  strMsg _ = undefined

getPos :: ParsedExpr -> SourcePosition
getPos (ParsedExpr node) = nodePos node
getExpr :: ParsedExpr -> Expr ParsedExpr
getExpr (ParsedExpr node) = nodeExpr node
subsExpr :: ParsedExpr -> Expr ParsedExpr -> ParsedExpr
subsExpr (ParsedExpr node) expr = ParsedExpr $ node { nodeExpr = expr }

mkParsedExpr :: SourcePosition -> (Expr ParsedExpr) -> ParsedExpr
mkParsedExpr pos expr = ParsedExpr $ SourceNode pos expr 0

mkApply :: ParsedExpr -> ParsedExpr -> ParsedExpr 
mkApply a = subsExpr a . Apply a

data Expr a = FunctionDef (Equation a) [Equation a] (Map String a)
            | Apply a a
            | Constructor [a] String
            | Id String 
            | Literal Constant
            | Undefined String
          deriving (Eq)
          
instance (Show a) => Show (Expr a) where
  show (FunctionDef x xs closure) = "FunctionDef " ++ "(" ++ show x ++ ") " ++ "(" ++ show xs ++ ") " ++ "(" ++ show closure ++ ")"
  show (Apply a b) = "Apply " ++ "(" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Constructor stuff name) = "(" ++ name ++ concatMap ((' ':) . show) stuff ++ ")"
  show (Id a) = "Id " ++ show a
  show (Literal a) = show a
  show (Undefined a) = a