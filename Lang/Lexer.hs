{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
module Lang.Lexer
  ( PositionedToken(..)
  , prettyPToken
  , prettyPTokens
  , Token(..)
  , prettyToken
  , Lexer
  , testLexer
  , lex
  , lexToken
  , Parser
  , testParser
  , anyToken
  , lparenP
  , rparenP
  , parenP
  , larrowP
  , equalsP
  , backslashP
  , colonP
  , dotP
  , commaP
  , numP
  , stringP
  , holeP
  , lnameP
  , lnameP'
  , identP
  , ridentP
  ) where

import           Control.Applicative ((<|>), empty)
import           Control.Monad (when)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import           Data.Proxy (Proxy(..))
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import           Prelude hiding (lex)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-- language
import           Lang.Annot

data PositionedToken = PToken
  { ptSpan :: Span
  , ptToken :: Token
  } deriving (Show, Eq, Ord)

instance Annot PositionedToken where
  type Ann PositionedToken = Span
  getAnn = ptSpan

instance P.ShowToken PositionedToken where
  showTokens = Text.unpack . prettyPTokens . NE.toList

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
  | Dot
  | Comma
  | HoleLit Text
  | StringLit Text
  | LName Text
  | NumLit (Either Integer Double)
  deriving (Show, Eq, Ord)

prettyToken :: Token -> Text
prettyToken t = case t of
  LParen -> "("
  RParen -> ")"
  LArrow -> "->"
  Equals -> "="
  Backslash -> "\\"
  Colon -> ":"
  Dot -> "."
  Comma -> ","
  HoleLit l -> Text.cons '?' l
  StringLit l -> Text.pack $ show l
  LName n -> n
  NumLit n ->  Text.pack $ either show show n

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
  either (error . P.parseErrorPretty' i) id (P.runParser lexer "" i)

testParser :: Parser a -> [PositionedToken] -> a
testParser lexer i =
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
  , P.try $ P.string "." *> pure Dot
  , P.try $ P.string "," *> pure Comma
  , HoleLit <$> P.try holeLit
  , NumLit <$> P.try numP
  , LName <$> lnameP
  , StringLit <$> P.try stringLit
  ]
  where
    -- todo: extend to support all symbols here.
    identLetter = P.alphaNumChar <|> P.oneOf ("_'" :: [Char])
    identStart = P.alphaNumChar

    holeLit = P.char '?' *> (Text.pack <$> P.some identLetter)

    lnameP = Text.cons <$> identStart <*> (Text.pack <$> P.many identLetter)

    stringLit = P.char '"' *> (Text.pack <$> P.manyTill L.charLiteral (P.char '"'))

    numP = ((Right <$> P.try L.float) <|>
            (Left <$> P.try L.decimal))
           P.<?>
           "number"

anyToken :: Parser PositionedToken
anyToken = P.anyChar

token' :: (PositionedToken -> Maybe a) -> Parser a
token' f = P.token g Nothing
  where
    g pt = case f pt of
      Just a -> pure a
      Nothing -> Left (pure (P.Tokens (pt NE.:| [])), Set.empty)

-- token :: (Token -> Maybe a) -> Parser a
-- token f = token' (f . ptToken)

match :: Token -> Parser PositionedToken
match t = P.satisfy ((== t) . ptToken)

lparenP :: Parser PositionedToken
lparenP = match LParen

rparenP :: Parser PositionedToken
rparenP = match RParen

parenP :: Parser a -> Parser (PositionedToken, PositionedToken, a)
parenP p = do
  open <- lparenP
  a <- p
  close <- rparenP
  pure (open, close, a)

larrowP :: Parser PositionedToken
larrowP = match LArrow

equalsP :: Parser PositionedToken
equalsP = match Equals

backslashP :: Parser PositionedToken
backslashP = match Backslash

colonP :: Parser PositionedToken
colonP = match Colon

dotP :: Parser PositionedToken
dotP = match Dot

commaP :: Parser PositionedToken
commaP = match Comma

numP :: Parser (Span, Either Integer Double)
numP = token' $ \pt -> case ptToken pt of
  NumLit l -> Just (ptSpan pt, l)
  _ -> Nothing

stringP :: Parser (Span, Text)
stringP = token' $ \pt -> case ptToken pt of
  StringLit l -> Just (ptSpan pt, l)
  _ -> Nothing

holeP :: Parser (Span, Text)
holeP = token' $ \pt -> case ptToken pt of
  HoleLit t -> Just (ptSpan pt, t)
  _ -> Nothing

lnameP :: Parser (Span, Text)
lnameP = token' $ \pt -> case ptToken pt of
  LName t -> Just (ptSpan pt, t)
  _ -> Nothing

lnameP' :: Text -> Parser PositionedToken
lnameP' t1 = token' $ \pt -> case ptToken pt of
  LName t2
    | t1 == t2 -> Just pt
  _ -> Nothing

reserved :: [Text]
reserved =
  [ "let"
  , "in"
  ]

identP :: Parser (Span, Text)
identP = do
  n <- lnameP
  when (snd n `elem` reserved) $
    fail $ Text.unpack (snd n) <> " cannot be an identifier"
  pure n

ridentP :: Text -> Parser PositionedToken
ridentP = lnameP'
