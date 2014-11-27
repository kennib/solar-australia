{-# LANGUAGE OverloadedStrings #-}

import Data.NetCDF
import Data.NetCDF.Vector
import Foreign.C
import qualified Data.Vector.Storable as SV

type SVRet a = IO (Either NcError (SV.Vector a))

file = "../data/20120101.nc"

test fname = do
	enc <- openFile fname
	case enc of
		Left err -> print err
		Right nc -> do
			let (Just var) = ncVar nc "solar_exposure_day"
			stuff <- get nc var :: SVRet CDouble
			case stuff of
				Left err -> print err
				Right vec -> print (SV.length vec)
