{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit (exitFailure)

import Data.Text.Lazy as Text
import Data.ByteString.Lazy.Char8 as BS
import Control.Monad.IO.Class
import Control.Applicative

import Data.Aeson (decode, eitherDecode)
import Data.Geospatial (GeoFeatureCollection (..))

import Web.Scotty
import Network.Wai.Parse

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Game.SolarPower
import Data.SolarPower

data Team = Team { teamname :: String }

instance FromRow Int where
   fromRow = field

main :: IO ()
main = do
	conn <- open "solarpower.db"
	initDB conn
	scoringData <- loadScoringData "../data/ghi.geojson"
	case scoringData of
		Left err -> print err >> exitFailure
		Right geojson -> do
			let ghis = readGHIs geojson
			scotty 3000 $ app conn ghis

app conn ghis = do
	get "/" $ file "../website/index.html"
	post "/submit" $ do
		team <- fmap Team $ param "team"
		fs <- files
		let fs' = [ fileContent fi | (fieldName, fi) <- fs ]
		let f = case fs' of
			[] -> ""
			f:_ -> f

		let geofeatures = eitherDecode f :: Either String (GeoFeatureCollection Props)

		let arrays = case geofeatures of
			Right gfs -> readSolarArrays gfs
			Left err -> []

		let tscore = score arrays ghis
		liftIO $ submit conn team f tscore
		rank <- liftIO $ rank conn team tscore

		text . Text.pack $ "Thanks team '" ++ teamname team ++
						   "'! You are ranked " ++ show rank ++
						   ". With a score of " ++ (show $ tscore)

loadScoringData :: String -> IO (Either String (GeoFeatureCollection PropsGHI))
loadScoringData fname = do
	f <- BS.readFile fname
	return $ eitherDecode f

initDB :: Connection -> IO ()
initDB conn = execute_ conn "create table if not exists submissions (submission INTEGER PRIMARY KEY AUTOINCREMENT, team TEXT, arrays TEXT, score FLOAT);"

submit :: Connection -> Team -> ByteString -> Float -> IO ()
submit conn (Team team) geojson tscore = execute conn "insert into submissions (team, arrays, score) values (?, ?, ?)" (team, geojson, tscore)

rank :: Connection -> Team -> Float -> IO Int
rank conn (Team team) tscore = do
	rows <- query conn "SELECT count(*)+1 AS rank FROM (SELECT team, max(score) AS score FROM submissions GROUP BY team) WHERE team != ? AND score > ?" (team, tscore) :: IO [Int]
	return $ case rows of
		r:_ -> r
		[]  -> 0
