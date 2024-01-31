module Main (main) where

import qualified LexerSpec  as Lexer
import           Protolude
import           Test.Hspec

main :: IO ()
main = hspec $ Lexer.spec
