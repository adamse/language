{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
-- |
module Lang.Annot where

import Data.Semigroup

import Text.Megaparsec.Pos

import Lang.Fix

data AnnotF ann f e =
  Annot ann (f e)
  deriving (Show, Functor)

class Annot n where
  type Ann n
  getAnn :: n -> Ann n

instance Annot (AnnotF ann f e) where
  type Ann (AnnotF ann f e) = ann
  getAnn (Annot ann _) = ann

instance (Annot (f (F f))) => Annot (F f) where
  type Ann (F f) = Ann (f (F f))
  getAnn (In ann) = getAnn ann

instance Annot (ann, a) where
  type Ann (ann, a) = ann
  getAnn = fst

data Span
  = Span { spStart :: SourcePos, spEnd :: SourcePos }
  deriving (Show, Eq, Ord)

initialSpan f = Span i i
  where
    i = initialPos f

instance Semigroup Span where
  Span s11 s12 <> Span s21 s22
    = Span (min s11 s21) (max s12 s22)

prettySpan :: Span -> String
prettySpan (Span (SourcePos n l1 c1) (SourcePos _ l2 c2))
  | null n = showLC
  | otherwise = n <> ":" <> showLC
  where
    showLC =
      show (unPos l1) <> ":" <>
      show (unPos c1) <> "-" <>
      show (unPos l2) <> ":" <>
      show (unPos c2)

class AnnotSpan sp where
  getSpan' :: sp -> Span

instance AnnotSpan Span where
  getSpan' = id

type HasSpan annot = (Annot annot, AnnotSpan (Ann annot))

getSpan :: (HasSpan annot) => annot -> Span
getSpan = getSpan' . getAnn

inSpan :: (HasSpan annot) => SourcePos -> annot -> Bool
inSpan pos annot = sp1 `ltePos` pos && pos `ltPos` sp2
  where
    Span sp1 sp2 = getSpan annot
    ltePos (SourcePos _ l1 c1) (SourcePos _ l2 c2) =
      l1 <= l2 || (l1 == l2 && c1 <= c2)
    ltPos p1@(SourcePos _ l1 c1) p2@(SourcePos _ l2 c2) =
      p1 `ltePos` p2 && (l1, c1) /= (l2, c2)
