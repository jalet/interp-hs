module LexerSpec where

import           Lexer
import           Lexer.Token
import           Protolude
import           Test.Hspec

spec :: Spec
spec = do
  describe "lexer" $ do
    it "base chars" $ do
      lex "=+(){},;"
        `shouldBe` Right
          [ Assign,
            Plus,
            LeftParen,
            RightParen,
            LeftBrace,
            RightBrace,
            Comma,
            SemiColon
          ]
