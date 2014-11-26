module Data.SolarPower where

type GeoJson = String

data SolarArray = SolarArray [Float]
	deriving (Show)

readSolarArrays :: GeoJson -> [SolarArray]
readSolarArrays geojson = [ SolarArray [] | array <- [1..(fromIntegral $ length geojson)] ]
