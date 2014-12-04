module Game.SolarPower where

import Data.SolarPower

targetEnergy = 239.3e12 :: Float {- kWh -}
energyUnitPrice = 0.12 :: Float {- $ per kWh -}
farmEfficiency = 0.4 :: Float {- unitless -}
farmArea = 5000 * 5000 :: Float {- m^2 -}
farmCost = 2.5e9 :: Float {- $ -}

score :: [SolarArray] -> [GHI] -> Float
score arrays ghis = revenue (sum energy) - sum cost
	where (energy, cost) = unzip [(farmEnergy ghi, farmCost)
	                              | GHI coords ghi <- ghis,
	                                SolarArray coords' <- arrays,
	                                coords == coords']

revenue :: Float {- MJ per m^2 -} -> Float {- $ -}
revenue energy = min energy targetEnergy * energyUnitPrice

farmEnergy :: Float {- MJ per m^2 -} -> Float {- kWh -}
farmEnergy ghi = ghi * 365 * farmArea * farmEfficiency * 3.6
