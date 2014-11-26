{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy as Text
import Data.ByteString.Lazy.Char8 as BS
import Control.Monad.IO.Class

import Web.Scotty
import Network.Wai.Parse

import Database.SQLite.Simple

import Game.SolarPower
import Data.SolarPower

data Team = Team { teamname :: String }

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

			case fs' of
				[]   -> text . Text.pack $ "You forgot to send a file!"
				f:_  -> text . Text.pack $ "Thanks team '" ++ teamname team ++ "'! You are ranked " ++ (show $ rank f)


initDB :: Connection -> IO ()
initDB conn = execute_ conn "create table if not exists submissions (submission INTEGER PRIMARY KEY AUTOINCREMENT, team TEXT, arrays TEXT);"

submit :: Connection -> Team -> [SolarArray] -> IO ()
submit conn (Team team) arrays = execute conn "insert into submissions (team, arrays) values (?, ?)" [team, show arrays]
