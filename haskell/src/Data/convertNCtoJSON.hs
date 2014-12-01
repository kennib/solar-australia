{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM, mapM)
import Data.List (transpose)
import Data.ByteString.Lazy.Internal (ByteString)

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

type SVRet a = IO (Either NcError (SV.Vector a))

type Point a = (CDouble, CDouble)
type Polygon a = [Point a]
type GHI a = a

data Props = Props { avgGHI :: GHI Float }

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
		Right (lat, lng) -> (case ghiData' of
			Right ghis -> putStr . encode $ geojson (tilePolygons $ getTiles lat lng) ghis
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


getTiles :: SV.Vector CDouble -> SV.Vector CDouble -> [[Polygon CDouble]]
getTiles lat lng = squares lng' lat'
	where lat' = SV.toList lat
	      lng' = SV.toList lng

squares :: [a] -> [b] -> [[[(a, b)]]]
squares xs ys = map (map concat . adjacents) $ transpose $ map adjacents points
	where points = [ [ (x, y) | x <- xs ] | y <- ys ]

adjacents :: [a] -> [[a]]
adjacents (x:x':xs) = [x, x'] : adjacents (x':xs)
adjacents (x:[]) = []
adjacents [] = []

getGhi :: SV.Vector CDouble -> [[GHI CDouble]]
getGhi ghis = [SV.toList ghis]

tilePolygons :: [[Polygon CDouble]] -> [GeospatialGeometry]
tilePolygons tiles = polygons
	where polygons = map (Polygon . GeoPolygon) $ map (ring . map coords) $ concat tiles
	      coords (lat, lng) = [float2Double $ toFloat lat, float2Double $ toFloat lng]
	      ring (a:b:c:ds) = [makeLinearRing a b c ds]

geojson :: [GeospatialGeometry] -> [[[CDouble]]] -> GeoFeatureCollection Props 
geojson polys ds = GeoFeatureCollection Nothing features 
	where features = zipWith (\p d -> GeoFeature Nothing p d Nothing) polys props
	      props = map Props $ map average $ map (filter (-999 /=)) $ transpose $ map (map toFloat . concat) ds 
	      average [] = 0
	      average x = sum x / (fromIntegral $ length x)

toFloat :: RealFloat a => a -> Float
toFloat = uncurry encodeFloat . decodeFloat

instance ToJSON Props where
	toJSON (Props ghi) = object ["avgGHI" .= ghi]
