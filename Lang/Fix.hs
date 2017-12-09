{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Lang.Fix where

import           Data.Function (fix)
import           Data.Functor.Contravariant (Contravariant(..))

newtype F f = In (f (F f))

deriving instance (Show (f (F f))) => Show (F f)

out :: F f -> f (F f)
out (In f) = f

ffixF
  :: (Functor g)
  => (forall a. g a -> g (f a))
  -> g (F f)
ffixF = fix . fmap (fmap In)

cfixF
  :: (Contravariant g)
  => (forall a. g a -> g (f a))
  -> g (F f)
cfixF = fix . fmap (contramap out)

cata :: Functor f => (f a -> a) -> F f -> a
cata f = c where c = f . fmap c . out
