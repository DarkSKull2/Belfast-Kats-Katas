module Lib where

type Observatory = {
  name :: String,
  country :: String,
  startYear :: Int,
  coveredArea :: Float,
  earthQuakes :: [EarthQuakes]
}