
module LeibnizLexer ( symbol
                    , parens
                    , naturalOrFloat
                    , stringLiteral
                    , identifier
                    , typeName
                    , reservedOp
                    , whiteSpace
                    , comma
                    ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

--whitespaceChars :: String
--whitespaceChars = " \n\r\t"
operatorChars :: String
operatorChars = "`~!@$%^&*()-+=[]{};:<>,./?\\|" 
--identChars :: String
--identChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

lexerStyle :: LanguageDef st
lexerStyle = emptyDef
                { Token.commentStart   = "{-"
                , Token.commentEnd     = "-}"
                , Token.commentLine    = "--"
                , Token.nestedComments = True
                , Token.identStart     = lower
                , Token.identLetter    = alphaNum <|> oneOf "_'"
                , Token.opStart        = Token.opLetter lexerStyle
                , Token.opLetter       = oneOf operatorChars
                , Token.reservedOpNames= []
                , Token.reservedNames  = []
                , Token.caseSensitive  = True
                }
                
lexer :: Token.TokenParser st
lexer = Token.makeTokenParser lexerStyle

symbol :: String -> Parser String
symbol = Token.symbol lexer
parens :: Parser a -> Parser a
parens = Token.parens lexer
naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = Token.naturalOrFloat lexer
stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer
identifier :: Parser String
identifier = Token.identifier lexer
typeName :: Parser String
typeName = Token.identifier $ Token.makeTokenParser lexerStyle { Token.identStart = upper }
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
comma :: Parser String
comma = Token.comma lexer
