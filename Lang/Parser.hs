{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
module Lang.Parser where

import           Control.Applicative ((<|>))
import           Data.Semigroup (Semigroup, (<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Text.Megaparsec as P

import           Lang.Annot
import           Lang.Lexer
import           Lang.Syntax

type ParserName = (Span, Text)

instance PrettyName (Span, Text) where
  prettyName = snd

data ExtraAnn
  = ParenAnn { openParen :: PositionedToken, closeParen :: PositionedToken }
  | AbsAnn { lambdaAnn :: PositionedToken, arrowAnn :: PositionedToken }
  | LetAnn { letAnn :: PositionedToken, eqAnn :: PositionedToken, inAnn :: PositionedToken }
  deriving (Show, Eq, Ord)

data ParserAnn =
  ParserAnn
  { paSpan :: Span
  , paMore :: Set ExtraAnn
  } deriving (Show, Eq)

addMore :: ExtraAnn -> ParserAnn -> ParserAnn
addMore e ann = ann { paMore = Set.insert e $ paMore ann }

instance Semigroup ParserAnn where
  p1 <> p2 =
    ParserAnn
    (paSpan p1 <> paSpan p2)
    Set.empty

instance AnnotSpan ParserAnn where
  getSpan' = paSpan

type ParserTerm = Term ParserName ParserAnn

parse :: Parser ParserTerm
parse = termP

{-# INLINEABLE termP #-}
termP :: Parser ParserTerm
termP = foldApp <$> P.some nonAppP
  where
    foldApp = foldl1 $ \t1 t2 ->
      App (getAnn t1 <> getAnn t2) t1 t2

{-# INLINE nonAppP #-}
nonAppP :: Parser ParserTerm
nonAppP =
  parented
  <|>
  term
  where
    parented = do
      (open, close, t) <- parenP termP
      pure $ fmap (addMore (ParenAnn open close)) t
    term = P.choice
      [ absp
      , letp
      , holep
      , numberp
      , identp
      ]
    absp = do
      lam <- backslashP
      name <- identP
      arr <- larrowP
      bod <- termP
      pure $ Abs
        (ParserAnn (getSpan lam <> getSpan bod) (Set.singleton (AbsAnn lam arr)))
        name
        bod
    letp = do
      lets <- ridentP "let"
      name <- identP
      eqs <- equalsP
      bound <- termP
      ins <- ridentP "in"
      bod <- termP
      pure $ Let
        (ParserAnn (getSpan lets <> getSpan bod) (Set.singleton (LetAnn lets eqs ins)))
        name
        bound
        bod
    identp = P.try $ do
      name <- identP
      pure $ Ident
        (ParserAnn (getSpan name) Set.empty)
        name
    holep = do
      hole <- holeP
      pure $ Hole
        (ParserAnn (getSpan hole) Set.empty)
        hole
    numberp = do
      (sp, n) <- numP
      pure $ Number
        (ParserAnn sp Set.empty)
        n
