module Game.SolarPower where

import Data.SolarPower

score :: [SolarArray] -> [GHI] -> Float
score arrays ghis = sum tilesWithPanels
	where tilesWithPanels = [ghi * fromIntegral panels | GHI coords ghi <- ghis,
	                                                     SolarArray coords' panels <- arrays,
	                                                     coords==coords']
