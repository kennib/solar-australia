module Game.SolarPower where

import Prelude hiding (lookup)

import GHC.Float (double2Float)
import Data.List (minimumBy)
import Data.HashMap.Lazy (HashMap(..), lookup)

import Data.SolarPower

data City = City { name :: String, location :: [Double] {- lng, lat -}, population :: Int }
	deriving (Eq, Ord)

energyUnitPrice = 0.12 :: Float {- $ per kWh -}
energyPerPersonPerYear = 10712.18 {- kwh per person per year -}
transmissionLossRate = 1e-7 :: Float {- m^-1 -}
farmEfficiency = 0.4 :: Float {- unitless -}
farmArea = 5000 * 5000 :: Float {- m^2 -}
farmCost = 2.5e9 :: Float {- $ -}

score :: [SolarArray] -> HashMap [Double] GHI -> Float
score arrays ghis = sum [ profits city $
                          unzip [(farmEnergy ghi * (1 - transmissionLoss coords city), farmCost)
					            | SolarArray coords <- arrays
                                , let Just (GHI coords' ghi) = lookup coords ghis
                                , city == closestCity coords
					            , coords == coords']
					    | city <- cities]
	where profits city (energies, costs) = revenue (sum energies) city - sum costs

revenue :: Float {- MJ per m^2 -} -> City -> Float {- $ -}
revenue energy city = min energy targetEnergy * energyUnitPrice
	where targetEnergy = (fromIntegral $ population city) * energyPerPersonPerYear

farmEnergy :: Float {- MJ per m^2 -} -> Float {- kWh -}
farmEnergy ghi = ghi * farmArea * farmEfficiency * 3.6 * 365

transmissionLoss :: [Double] {- lng, lat -} -> City -> Float
transmissionLoss coords city = cityDist coords city * transmissionLossRate

closestCity :: [Double] {- lng, lat -} -> City
closestCity coords = snd $ minimum [(cityDist coords city, city) | city <- cities]

cityDist :: [Double] {- lng, lat -} -> City -> Float {- m -}
cityDist coords city = earthDist (lat, lng) (lat', lng')
	where (lng:lat:[]) = map double2Float $ coords
	      (lng':lat':[]) = map double2Float $ location city

cities = [ City "Sydney"         [151.207,  (-33.868)]  4667283
         , City "Melbourne"      [144.963,  (-37.814)]  4246345
         , City "Brisbane" 	     [153.028,  (-27.468)]  2189878
         , City "Perth"          [115.861,  (-31.952)]  1897548
         , City "Adelaide"       [138.599,  (-34.929)]  1277174
         , City "Gold Coast"     [153.431,  (-28.000)]  590889
         , City "Newcastle"      [151.776,  (-32.927)]  418958
         , City "Canberra"       [149.128,  (-35.283)]  411609
         , City "Sunshine Coast" [153.0667, (-26.650)]  285169
         , City "Wollongong"     [150.893,  (-34.424)]  282099
         , City "Hobart"         [147.3250, (-42.8806)] 216959
         , City "Geelong"        [144.3500, (-38.1500)] 179042
         , City "Townsville"     [146.8183, (-19.2564)] 171971
         , City "Cairns"         [145.7753, (-16.9256)] 142528
         , City "Darwin"         [130.8333, (-12.4500)] 131678
         ]

--Taken from http://rosettacode.org/wiki/Haversine_formula#Haskell

-- The haversine of an angle.
hsin t = let u = sin (t/2) in u*u
 
-- The distance between two points, given by latitude and longtitude, on a
-- circle.  The points are specified in radians.
distRad radius (lat1, lng1) (lat2, lng2) =
  let hlat = hsin (lat2 - lat1)
      hlng = hsin (lng2 - lng1)
      root = sqrt (hlat + cos lat1 * cos lat2 * hlng)
  in 2 * radius * asin (min 1.0 root)
 
-- The distance between two points, given by latitude and longtitude, on a
-- circle.  The points are specified in degrees.
distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
  where deg2rad (t, u) = (d2r t, d2r u)
        d2r t = t * pi / 180
 
-- The approximate distance, in metres, between two points on Earth.  
-- The latitude and longtitude are assumed to be in degrees.
earthDist = distDeg 6372.8e3
