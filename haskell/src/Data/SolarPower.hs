{-# LANGUAGE OverloadedStrings #-}
module Data.SolarPower where

import Data.Aeson (decode, FromJSON, parseJSON)
import Data.Aeson.Types
import Data.Geospatial
import Data.LinearRing

data SolarArray = SolarArray [[Double]] Int
	deriving (Show)

data Props = Props { panels :: Int }
	deriving (Show)

readSolarArrays :: GeoFeatureCollection Props -> [SolarArray]
readSolarArrays geo = zipWith SolarArray (coords geo) (panelCounts geo)
panelCounts = (map panels . map _properties . _geofeatures)
coords = (map fromLinearRing . map head . map _unGeoPolygon . map getPolygon . filter isPolygon . map _geometry . _geofeatures)
	where isPolygon (Polygon g) = True
	      isPolygon _ = False
	      getPolygon (Polygon g) = g

instance FromJSON Props where
	parseJSON (Object obj) = do
		panels <- obj .: "panels"
		return $ Props panels
	parseJSON _ = return $ Props 0
