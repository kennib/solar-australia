module Data.SolarPower where

type GeoJson = String

data SolarArray = SolarArray [Float]

readSolarArrays :: GeoJson -> [SolarArray]
readSolarArrays geojson = [SolarArray []]
