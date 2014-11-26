{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy (pack)

import Web.Scotty
import Network.Wai.Parse

import Game.SolarPower

main :: IO ()
main = scotty 3000 $ do
	post "/submit" $ do
		fs <- files
		let fs' = [ fileContent fi | (fieldName, fi) <- fs ]
		case fs' of
			[]   -> text . pack $ "You forgot to send a file!" 
			f:_  -> text . pack $ "Thanks! You are ranked " ++ (show $ rank f)
