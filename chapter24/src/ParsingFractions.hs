{-# LANGUAGE OverloadedStrings #-}

module ParsingFractions where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta

badFraction = "1/0"

alsoBad = "10"

shouldWork = "1/2"

shouldAlsoWork = "2/1"

fraction :: Parser Rational
fraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  let fraction' = parseString decimalOrFraction mempty
  print $ fraction' shouldWork
  print $ fraction' shouldAlsoWork
  print $ fraction' alsoBad
  print $ fraction' badFraction

decimalOrFraction :: Parser (Either Integer Rational)
decimalOrFraction = try decimal' <|> fraction'
  where
    decimal' = Left <$> decimal <* notFollowedBy (string "/")
    fraction' = Right <$> fraction
