{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM, mapM)
import Data.List (transpose)
import Data.ByteString.Lazy.Internal (ByteString)

import Data.NetCDF
import Data.NetCDF.Vector
import Foreign.C
import qualified Data.Vector.Storable as SV

import Data.Aeson.Encode (encode)
import Data.Geospatial
import Data.LinearRing
import GHC.Float (float2Double)

import System.Directory
import System.FilePath

type SVRet a = IO (Either NcError (SV.Vector a))

type Point a = (CDouble, CDouble)
type Polygon a = [Point a]
type GHI a = a

main = do
	let dir = "../data"
	files <- getDirectoryContents dir
	let netcdfs = map (combine dir) $ filter ((== ".nc") . takeExtension) files
	ghiData <- forM netcdfs extractData
	let ghiData' = mapM (\x -> case x of
		Right (lat, lng, ghi) -> do
			let tiles = getTiles lat lng
			let ghis = getGhi ghi
			Right (tiles, ghis)
		Left err -> Left err) ghiData
	
	case ghiData' of
		Right ((tiles, ghi):gs) -> print . encode $ geojson tiles $ ghi:(map snd gs)
		Left err -> print err

extractData :: String -> IO (Either NcError (SV.Vector CDouble, SV.Vector CDouble, SV.Vector CDouble))
extractData fname = do
	enc <- openFile fname
	case enc of
		Right nc -> do
			let (Just var) = ncVar nc "latitude"
			lats <- get nc var :: SVRet CDouble

			let (Just var) = ncVar nc "longitude"
			lngs <- get nc var :: SVRet CDouble

			let (Just var) = ncVar nc "solar_exposure_day"
			ghis <- get nc var :: SVRet CDouble

			case (lats, lngs, ghis) of
				(Right lat, Right lng, Right ghi) -> return $ Right (lat, lng, ghi)
				(Left err, _, _)-> return $ Left err
				(_, Left err, _)-> return $ Left err
				(_, _, Left err)-> return $ Left err

		Left err -> return $ Left err

getTiles :: SV.Vector CDouble -> SV.Vector CDouble -> [[Polygon CDouble]]
getTiles lat lng = squares lat' lng'
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

geojson :: [[Polygon CDouble]] -> [[[CDouble]]] -> GeoFeatureCollection [Float]
geojson tiles ds = GeoFeatureCollection Nothing features 
	where features = zipWith (\p d -> GeoFeature Nothing p d Nothing) polygons props
	      props = transpose $ map (map toFloat . concat) ds 
	      polygons = map (Polygon . GeoPolygon) $ map (ring . map coords) $ concat tiles
	      coords (lat, lng) = [float2Double $ toFloat lat, float2Double $ toFloat lng]
	      ring (a:b:c:ds) = [makeLinearRing a b c ds]

toFloat :: RealFloat a => a -> Float
toFloat = uncurry encodeFloat . decodeFloat
