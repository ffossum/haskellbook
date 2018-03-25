module MaybeSuccess where

import           Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing
