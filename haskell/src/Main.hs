{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy as Text
import Data.ByteString.Lazy.Char8 as BS
import Control.Monad.IO.Class
import Control.Applicative

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

	scotty 3000 $ do
		post "/submit" $ do
			team <- fmap Team $ param "team"
			fs <- files
			let fs' = [ BS.unpack $ fileContent fi | (fieldName, fi) <- fs ]
			
			liftIO $ case fs' of
				[]   -> return ()
				f:_  -> submit conn team $ readSolarArrays f

			rank <- liftIO $ case fs' of
				[]  -> return 0
				f:_ -> rank conn team $ readSolarArrays f

			case fs' of
				[]   -> text . Text.pack $ "You forgot to send a file!"
				f:_  -> text . Text.pack $ "Thanks team '" ++ teamname team ++ "'! You are ranked " ++ show rank ++ ". With a score of " ++ (show $ score $ readSolarArrays f)


initDB :: Connection -> IO ()
initDB conn = execute_ conn "create table if not exists submissions (submission INTEGER PRIMARY KEY AUTOINCREMENT, team TEXT, arrays TEXT, score FLOAT);"

submit :: Connection -> Team -> [SolarArray] -> IO ()
submit conn (Team team) arrays = execute conn "insert into submissions (team, arrays, score) values (?, ?, ?)" (team, show arrays, score arrays)

rank :: Connection -> Team -> [SolarArray] -> IO Int
rank conn (Team team) arrays = do
	rows <- query conn "SELECT count(*)+1 AS rank FROM (SELECT team, max(score) AS score FROM submissions GROUP BY team) WHERE team != ? AND score > ?" (team, score arrays) :: IO [Int]
	return $ case rows of
		r:_ -> r
		[]  -> 0
