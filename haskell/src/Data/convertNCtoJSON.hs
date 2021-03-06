{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (putStr)

import Control.Monad (forM, mapM)
import Data.List (transpose)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.ByteString.Lazy (putStr)

import Data.NetCDF
import Data.NetCDF.Vector
import Foreign.C
import qualified Data.Vector.Storable as SV

import Data.Aeson (encode, object, ToJSON, toJSON, (.=))
import Data.Geospatial
import Data.LinearRing
import GHC.Float (float2Double)

import System.Directory
import System.FilePath

import Data.SolarPower

type SVRet a = IO (Either NcError (SV.Vector a))

type Point a = (CDouble, CDouble)
type Polygon a = [Point a]

main = do
	let dir = "../data"
	files <- getDirectoryContents dir
	let netcdfs = map (combine dir) $ filter ((== ".nc") . takeExtension) files
	ghiData <- forM netcdfs extractGhi
	let ghiData' = mapM (\x -> case x of
		Right ghi -> Right $ getGhi ghi
		Left err -> Left err) ghiData
	latLngData <- extractLatLng $ head netcdfs

	case latLngData of
		Right (lats, lngs) -> (case ghiData' of
			Right ghis -> putStr . encode $ geojson [point lat lng | lat <- toDoubleVec lats, lng <- toDoubleVec lngs] ghis
			Left err -> print err)
		Left err -> print err

extractLatLng :: String -> IO (Either NcError (SV.Vector CDouble, SV.Vector CDouble))
extractLatLng fname = do
	enc <- openFile fname
	case enc of
		Right nc -> do
			let (Just var) = ncVar nc "latitude"
			lats <- get nc var :: SVRet CDouble

			let (Just var) = ncVar nc "longitude"
			lngs <- get nc var :: SVRet CDouble

			case (lats, lngs) of
				(Right lat, Right lng) -> return $ Right (lat, lng)
				(Left err, _)          -> return $ Left err
				(_, Left err)          -> return $ Left err

		Left err -> return $ Left err

extractGhi :: String -> IO (Either NcError (SV.Vector CDouble))
extractGhi fname = do
	enc <- openFile fname
	case enc of
		Right nc -> do
			let (Just var) = ncVar nc "solar_exposure_day"
			ghis <- get nc var :: SVRet CDouble

			case ghis of
				Right ghi -> return $ Right ghi
				Left err  -> return $ Left err

		Left err -> return $ Left err

toDoubleVec = map (float2Double . toFloat) . SV.toList
getGhi ghis = [SV.toList ghis]

point :: Latitude -> Longitude -> GeospatialGeometry
point lat lng = Point $ GeoPoint [lng, lat]

geojson :: [GeospatialGeometry] -> [[[CDouble]]] -> GeoFeatureCollection PropsGHI 
geojson polys ds = GeoFeatureCollection Nothing features 
	where features = zipWith (\p d -> GeoFeature Nothing p d Nothing) polys props
	      props = map prop $ transpose $ map (map toFloat) $ map (concat . transpose) ds 
	      prop = PropsGHI . average . filter (-999 /=)
	      average [] = 0
	      average x = sum x / (fromIntegral $ length x)

toFloat :: RealFloat a => a -> Float
toFloat = uncurry encodeFloat . decodeFloat

instance ToJSON PropsGHI where
	toJSON (PropsGHI ghi) = object ["avgGHI" .= ghi]
