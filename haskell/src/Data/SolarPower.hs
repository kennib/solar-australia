{-# LANGUAGE OverloadedStrings #-}
module Data.SolarPower where

import Data.Aeson (decode, FromJSON, parseJSON)
import Data.Aeson.Types
import Data.Geospatial

data SolarArray = SolarArray [Double] Int
	deriving (Show)

data Props = Props { panels :: Int }

readSolarArrays :: GeoFeatureCollection Props -> [SolarArray]
readSolarArrays geo = zipWith SolarArray coords panelCounts
	where coords = (map _unGeoPoint . map getPoint . filter isPoint . map _geometry . _geofeatures) geo :: [[Double]]
	      panelCounts = (map panels . map _properties . _geofeatures) geo :: [Int]
	      isPoint (Point g) = True
	      isPoint _ = False
	      getPoint (Point g) = g

instance FromJSON Props where
	parseJSON (Object obj) = do
		panels <- obj .: "panels"
		return $ Props panels
	parseJSON _ = return $ Props 0
