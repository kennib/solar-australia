module Game.SolarPower where

import Data.SolarPower

score :: [SolarArray] -> Float
score = fromIntegral . length
