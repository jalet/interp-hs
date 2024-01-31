module Lexer where

import           Common.ParserT
import           Lexer.Token
import           Lexer.Types
import           Prelude        (read)
import           Protolude      hiding (many, many1, one)

lexToken :: Lexer Token
lexToken =
  choose
    [ lexInteger,
      lexIllegal
    ]

digit :: Lexer Char
digit = predicate isDigit

lexInteger :: Lexer Token
lexInteger = IntLiteral . read <$> many1 digit

lexIllegal :: Lexer Token
lexIllegal = consume $> Illegal

skipWhitespaces :: Lexer ()
skipWhitespaces = many (predicate $ flip elem [' ', '\t', '\n', '\r']) >> return ()

lex :: Text -> Either ParserError [Token]
lex = execLexer fn
  where
    fn :: Lexer [Token]
    fn = do
      skipWhitespaces
      c <- preview
      case c of
        Nothing -> return [EOF]
        Just t  -> (:) <$> lexToken <*> fn
