module FixerUpper where

-- Given the function and values provided, use (<$>) from Functor, (<*>) and pure from the Applicative typeclass to fill in missing bits of the broken code to make it work.
hello = const <$> Just "Hello" <*> Just "World"

quad = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
