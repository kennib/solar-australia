module Game.SolarPower where

import Data.SolarPower

maxPanels :: Int {- m^2 -}
maxPanels = 5000*5000

targetEnergy :: Float {- KWh -}
targetEnergy = 239.3e12

panelUnitCost :: Float {- $ per m^2 -}
panelUnitCost = 600

panelEfficiency :: Float {- -}
panelEfficiency = 0.4

energyUnitPrice :: Float {- $ per KWh -}
energyUnitPrice = 0.12

panelCost :: Int -> Float {- $ -}
panelCost panels = (fromIntegral panels * panelUnitCost)

ghiEnergy :: Float {- MJ per m^2 -} -> Float {- KWh -}
ghiEnergy ghi = ghi * panelEfficiency * 365 * 3.6

energyRevenue :: Float -> Float {- $ -}
energyRevenue = (* energyUnitPrice)

panelsEnergy :: Int -> Float -> Float
panelsEnergy panels' ghi = ghiEnergy $ fromIntegral panels * ghi
	where panels = min panels' maxPanels

panelsRevenue :: Int -> Float -> Float
panelsRevenue panels ghi = energyRevenue energy 
	where energy = min targetEnergy $ panelsEnergy panels ghi

profit :: Float -> Int -> Float
profit ghi panels = panelsRevenue panels ghi  - panelCost panels

score :: [SolarArray] -> [GHI] -> Float
score arrays ghis = sum tilesWithPanels
	where tilesWithPanels = [profit ghi panels | GHI coords ghi <- ghis,
	                                                     SolarArray coords' panels <- arrays,
	                                                     coords==coords', panels >= 0]
