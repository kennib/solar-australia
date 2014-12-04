{-# LANGUAGE OverloadedStrings #-}
module Data.SolarPower where

import Data.HashMap.Lazy (HashMap(..), fromList)

import Data.Aeson (FromJSON)
import Data.Aeson.Types
import Data.Geospatial hiding (properties)

data SolarArray = SolarArray [Double]
	deriving (Show)

data GHI = GHI [Double] Float
	deriving (Show)

data PropsGHI = PropsGHI { averageGHI :: Float }
	deriving (Show)

data DontCare = DontCare

readSolarArrays :: GeoFeatureCollection DontCare -> [SolarArray]
readSolarArrays geo = map SolarArray (coords geo)

readGHIs :: GeoFeatureCollection PropsGHI -> HashMap [Double] GHI
readGHIs geo = fromList $ zip (coords geo) $ zipWith GHI (coords geo) (ghis geo)
ghis = map averageGHI . properties

properties = map _properties . _geofeatures
coords = map _unGeoPoint . map getPoint . filter isPoint . map _geometry . _geofeatures
	where isPoint (Point g) = True
	      isPoint _ = False
	      getPoint (Point g) = g

instance FromJSON DontCare where
	parseJSON _ = return DontCare

instance FromJSON PropsGHI where
	parseJSON (Object obj) = do
		ghi <- obj .: "avgGHI"
		return $ PropsGHI ghi
	parseJSON _ = return $ PropsGHI 0
