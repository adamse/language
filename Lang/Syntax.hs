{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
-- |
module Lang.Syntax where

import Data.Text (Text)
import Data.Semigroup ((<>))

import Lang.Annot

data Term name ann
  = App ann (Term name ann) (Term name ann)
  | Abs ann name (Term name ann)
  | Let ann name (Term name ann) (Term name ann)
  | Ident ann name
  | Hole ann name
  deriving (Show, Eq, Functor)

instance Annot (Term name ann) where
  type Ann (Term name ann) = ann
  getAnn = \case
    App ann _ _ -> ann
    Abs ann _ _ -> ann
    Let ann _ _ _ -> ann
    Ident ann _ -> ann
    Hole ann _ -> ann

class PrettyName name where
  prettyName :: name -> Text

prettyTerm :: (PrettyName name) => Term name ann -> Text
prettyTerm = \case
  App _ t1 t2 ->
    prettyTerm t1 <> " " <> prettyTerm t2
  Abs _ n b ->
    "\\ " <> prettyName n <> " -> " <> prettyTerm b
  Let _ n e b ->
    "let " <> prettyName n <> " = " <> prettyTerm e <> " in " <> prettyTerm b
  Ident _ n -> prettyName n
  Hole _ n -> "?" <> prettyName n
