module Orphan.ListyInstances where

import           Orphan.Listy

instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy l) (Listy l') = Listy $ mappend l l'
