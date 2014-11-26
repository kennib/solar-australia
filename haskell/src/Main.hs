{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy as Text
import Data.ByteString.Lazy.Char8 as BS
import Control.Monad.IO.Class

import Web.Scotty
import Network.Wai.Parse

import Database.SQLite.Simple

import Game.SolarPower
import Data.SolarPower

main :: IO ()
main = do
	conn <- open "solarpower.db"
	initDB conn

	scotty 3000 $ do
		post "/submit" $ do
			fs <- files
			let fs' = [ BS.unpack $ fileContent fi | (fieldName, fi) <- fs ]
			
			liftIO $ case fs' of
				[]   -> return ()
				f:_  -> submit conn $ readSolarArrays f

			case fs' of
				[]   -> text . Text.pack $ "You forgot to send a file!"
				f:_  -> text . Text.pack $ "Thanks! You are ranked " ++ (show $ rank f)


initDB :: Connection -> IO ()
initDB conn = execute_ conn "create table if not exists submissions (arrays TEXT);"

submit :: Connection -> [SolarArray] -> IO ()
submit conn arrays = execute conn "insert into submissions (arrays) values (?)" [ show coords | SolarArray coords <- arrays ]
