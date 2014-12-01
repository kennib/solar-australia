module Data.SolarPower where

import Data.Aeson (decode, FromJSON, parseJSON, obj)
import Data.Geospatial

data SolarArray = SolarArray [Double] Int
	deriving (Show)

data Props = Props { panels :: Int }

readSolarArrays :: GeoFeatureCollection -> [SolarArray]
readSolarArrays geo = SolarArray coords $ panels props
	where coords = (_geometry . _geofeatures) geo
	      panels = (_properties . _geofeatures) geo :: Props

instance FromJSON where
	parseJSON parseJSON (Object obj) = do
		panels <- obj .: "panels"
		Props panels
	parseJSON _ = Props 0
