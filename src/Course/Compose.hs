{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))
  deriving (Show, Eq)

-- $setup
-- >>> import Course.Core
-- >>> import Course.List
-- >>> import Course.Optional

-- | Implement a Functor instance for Compose
--
-- >>> (+1) <$> Compose (Full 1 :. Full 2 :. Nil)
-- Compose [Full 2,Full 3]
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  f <$> (Compose a) = Compose $ (f <$>) <$> a

-- | Binds a function on Compose
--
-- >>> Compose (Full (+1) :. Full (+2) :. Nil) <*> Compose (Full 1 :. Full 2 :. Nil)
-- Compose [Full 2,Full 3,Full 3,Full 4]
instance (Apply f, Apply g) =>
  Apply (Compose f g) where
-- Implement the (<*>) function for an Apply instance for Compose
  (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

-- | Insert into a Compose
--
-- prop> pure a == Compose (Full a :. Nil)
instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure

instance (Bind f, Bind g) =>
  Bind (Compose f g) where
-- Implement the (=<<) function for a Bind instance for Compose
  (=<<) =
    error "impossible"
