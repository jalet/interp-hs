module Lexer.Token where

import           Protolude

data Token where
  Illegal :: Token
  EOF :: Token
  Ident :: Text -> Token
  IntLiteral :: Integer -> Token
  Assign :: Token
  Plus :: Token
  Comma :: Token
  SemiColon :: Token
  LeftParen :: Token
  RightParen :: Token
  LeftBrace :: Token
  RightBrace :: Token
  Function :: Token
  Let :: Token
  deriving (Show, Eq)
