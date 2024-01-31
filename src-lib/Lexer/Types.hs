module Lexer.Types where

import           Common.ParserT
import           Common.Stream
import qualified Data.Text      as T
import           Protolude

type Lexer = ParserT Text Identity

instance Stream Text Char where
  read = T.uncons

execLexer :: Lexer a -> Text -> Either ParserError a
execLexer = (runIdentity .) . execParserT
