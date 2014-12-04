{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit (exitFailure)

import Data.Monoid ((<>))
import qualified Data.Text.Lazy as Text
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad.IO.Class
import Control.Applicative
import Data.HashMap.Lazy (HashMap(..))

import Data.Aeson (eitherDecode)
import Data.Geospatial (GeoFeatureCollection (..))

import Web.Scotty
import Network.Wai.Parse
import Network.Wai.Middleware.Static
import Lucid

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

hshow :: (Monad m, Show a) => a -> HtmlT m ()
hshow = toHtml . show

app conn ghis = do
	middleware $ staticPolicy (noDots >-> addBase "../website")

	get "/" $ redirect "/index.html"

	get "/scoreboard" $ do
		sb <- liftIO $ scoreboard conn
		html . renderText $ do
			link_ [rel_ "stylesheet", href_ "main.css"]
			h1_ "Scoreboard"
			table_ $ do
				tr_ $ th_ "Rank" >> th_ "Team" >> th_ "Profit"
				case sb of
					[] -> sequence [tr_ (td_ "-" >> td_ "No scores yet" >> td_ "-") ]
					scores -> sequence [tr_ (td_ (hshow rank) >> td_ (toHtml team) >> td_ ("$" <> hshow score))
					                   | (rank, (team, score)) <- zip [1..] sb]

	get "/submit" $ html . renderText $ do
		link_ [rel_ "stylesheet", href_ "main.css"]
		h1_ "Submissions"
		form_ [action_ "/submit", method_ "post", enctype_ "multipart/form-data"] $ do
			p_ "Choose a .geojson file to enter. Maximum file size is 3MB."
			p_ $ do
				label_ "Team name: "
				input_ [type_ "text", name_ "team"] >> br_  []
			p_ $ do
				label_ "Submission: "
				input_ [type_ "file", name_ "submission"] >> br_  []
			p_ $ input_ [type_ "submit", value_ "Submit!", name_ "submit"]

	post "/submit" $ do
		team <- fmap Team $ param "team"
		fs <- files
		let fs' = [ fileContent fi | (fieldName, fi) <- fs ]
		let f = case fs' of
			[] -> ""
			f:_ -> f

		result <- submit conn ghis team f
		case result of
			Left err -> text $ case err of
				EmptySubmission -> "Your submission was empty!\n"
				InvalidJSON msg -> Text.pack ("Invalid GeoJSON format: " ++ msg ++ "\n")
				TooLarge -> "Your submission must be less than 3MB!\n"
				UnknownError -> "Unknown error occurred.\n"
			Right ((rank, score), succ) ->
				let warning = case succ of
					WarnSuccess msg -> " Warning: " ++ msg
					_ -> ""
				in text $ Text.pack $ "Thanks, team '" ++ teamname team ++
				                      "'! You are ranked " ++ show rank ++
				                      ". With a score of " ++ show score ++
				                      "." ++ warning ++ "\n"

type Submission = Either SubmissionError (SubmissionResult, SubmissionSuccess)

data SubmissionError
	= EmptySubmission
	| InvalidJSON String
	| TooLarge
	| UnknownError

data SubmissionSuccess
	= TotalSuccess
	| WarnSuccess String

type SubmissionResult = (Int, Float)

fileMaxMegabytes = 3.0
toBytes = (* 1024) . (* 1024)

submit :: Connection -> HashMap [Double] GHI -> Team -> ByteString -> ActionM Submission
submit conn ghis team f = if fromIntegral (BS.length f) > toBytes fileMaxMegabytes
	then return $ Left TooLarge
	else case (f, eitherDecode f) of
		("", _) -> return $ Left EmptySubmission
		(_, Left msg) -> return $ Left (InvalidJSON msg)
		(f, Right gfs) -> case readSolarArrays gfs of
			[] -> return $ Left EmptySubmission
			arrays -> do
				let tscore = score arrays ghis
				liftIO $ saveSubmission conn team f tscore
				rank <- liftIO $ rank conn team tscore
				return $ Right ((rank, tscore), TotalSuccess)

loadScoringData :: String -> IO (Either String (GeoFeatureCollection PropsGHI))
loadScoringData fname = do
	f <- BS.readFile fname
	return $ eitherDecode f

initDB :: Connection -> IO ()
initDB conn = execute_ conn "create table if not exists submissions (submission INTEGER PRIMARY KEY AUTOINCREMENT, team TEXT, arrays TEXT, score FLOAT);"

saveSubmission :: Connection -> Team -> ByteString -> Float -> IO ()
saveSubmission conn (Team team) geojson tscore = execute conn "insert into submissions (team, arrays, score) values (?, ?, ?)" (team, geojson, tscore)

rank :: Connection -> Team -> Float -> IO Int
rank conn (Team team) tscore = do
	rows <- query conn "SELECT count(*)+1 AS rank FROM (SELECT team, max(score) AS score FROM submissions GROUP BY team) WHERE team != ? AND score > ?" (team, tscore) :: IO [Int]
	return $ case rows of
		r:_ -> r
		[]  -> 0

scoreboard :: Connection -> IO [(String, Float)]
scoreboard conn = query_ conn "select team, max(score) as score from submissions group by team order by score desc;"
