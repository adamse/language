{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
-- |
module Lang.Lexer where

-- megaparsec
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-- text
import           Data.Text (Text)
import qualified Data.Text as Text

-- base
import           Control.Applicative ((<|>), empty)
import           Data.Void (Void)
import           Prelude hiding (lex)
import           Data.Proxy (Proxy(..))
import qualified Data.List as List

-- language
import           Lang.Annot

data PositionedToken = PToken
  { ptSpan :: Span
  , ptToken :: Token
  } deriving (Show, Eq, Ord)

instance Annot PositionedToken where
  type Ann PositionedToken = Span
  getAnn = ptSpan

prettyPToken :: PositionedToken -> Text
prettyPToken = prettyToken . ptToken

prettyPTokens :: [PositionedToken] -> Text
prettyPTokens = Text.unwords . map prettyPToken

-- borrowing from purescript here
data Token
  = LParen
  | RParen
  | LArrow
  | Equals
  | Backslash
  | Colon
  | HoleLit Text
  | StringLit Text
  | LName Text
  | IntLit Integer
  | FloatLit Double
  deriving (Show, Eq, Ord)

prettyToken :: Token -> Text
prettyToken t = case t of
  LParen -> "("
  RParen -> ")"
  LArrow -> "->"
  Equals -> "="
  Backslash -> "\\"
  Colon -> ":"
  HoleLit l -> Text.cons '?' l
  StringLit l -> Text.pack $ show l
  LName n -> n
  IntLit i -> Text.pack $ show i
  FloatLit i -> Text.pack $ show i

type Lexer = P.Parsec Void Text
type Parser = P.Parsec Void [PositionedToken]

instance P.Stream [PositionedToken] where
  type Token [PositionedToken] = PositionedToken
  type Tokens [PositionedToken] = [PositionedToken]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy _ _ = spEnd . getSpan
  advanceN Proxy _ = List.foldl' (const (spEnd . getSpan))
  take1_ = List.uncons
  takeN_ n s
    | n <= 0 = Just ([], s)
    | List.null s = Nothing
    | otherwise = Just (List.splitAt n s)
  takeWhile_ p = List.break (not . p)

lex :: Lexer [PositionedToken]
lex = P.some lexToken

testLexer :: Lexer a -> Text -> a
testLexer lexer i =
  either (error . P.parseErrorPretty) id (P.runParser lexer "" i)

spaceL :: Lexer ()
spaceL = L.space P.space1 empty empty

lexToken :: Lexer PositionedToken
lexToken = L.lexeme spaceL posTokenL

posTokenL :: Lexer PositionedToken
posTokenL = do
  p1 <- P.getPosition
  tok <- tokenL
  p2 <- P.getPosition
  -- tokens are terminated by whitespace
  P.lookAhead $ (P.spaceChar *> pure ()) <|> P.eof
  pure $ PToken (Span p1 p2) tok

tokenL :: Lexer Token
tokenL = P.choice
  [ P.try $ P.string "(" *> pure LParen
  , P.try $ P.string ")" *> pure RParen
  , P.try $ P.string "->" *> pure LArrow
  , P.try $ P.string "=" *> pure Equals
  , P.try $ P.string "\\" *> pure Backslash
  , P.try $ P.string ":" *> pure Colon
  , HoleLit <$> P.try holeLit
  , LName <$> lnameP
  , StringLit <$> P.try stringLit
  , FloatLit <$> P.try floatLit
  , IntLit <$> P.try intLit
  ]
  where
    -- todo: extend to support all symbols here.
    identLetter = P.alphaNumChar <|> P.oneOf ("_'" :: [Char])
    identStart = P.alphaNumChar

    holeLit = P.char '?' *> (Text.pack <$> P.some identLetter)
    lnameP = Text.cons <$> identStart <*> (Text.pack <$> P.many identLetter)
    stringLit = P.char '"' *> (Text.pack <$> P.manyTill L.charLiteral (P.char '"'))
    intLit = L.decimal
    floatLit = L.float
