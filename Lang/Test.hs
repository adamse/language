-- |
module Lang.Test where

import           Data.Text (Text)

import qualified Lang.Lexer as L
import qualified Lang.Parser as P

testPipeline :: Text -> P.ParserTerm
testPipeline = L.testParser P.termP . L.testLexer L.lex
