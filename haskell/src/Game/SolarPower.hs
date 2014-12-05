module Game.SolarPower where

import qualified Prelude
import Numeric.Units.Dimensional.Prelude hiding (lookup)
import Numeric.Units.Dimensional.NonSI (year)
import Numeric.Units.Dimensional.Currency
import Numeric.NumType (Zero, Pos1, Neg3)

import GHC.Float (double2Float)
import Data.List (minimumBy)
import Data.HashMap.Lazy (HashMap(..), lookup)

import Data.SolarPower

data City = City { name :: String, location :: [Double] {- lng, lat -}, population :: Int }
	deriving (Eq, Ord)

energyUnitPrice = 0.12 *~ (usdollar / kWh)
energyPerPersonPerYear = 10712.18 *~ (kWh / person / year)
transmissionLossRate = 1e-7 *~ per metre
farmEfficiency = 0.4 *~ one
farmArea = 25 *~ square (kilo metre)
farmCost = 2.5 *~ bn usdollar

person = one
per = (one /)
kWh = kilo (watt * hour)

score :: [SolarArray] -> HashMap [Double] GHI -> Float
score arrays ghis = profit /~ usdollar
	where profits city (energies, costs) =
	              revenue (sum energies) city - sum costs
	      profit = sum [ profits city $
	                     unzip [(farmEnergy ghi * (_1 - transmissionLoss coords city), farmCost)
	                           | SolarArray coords <- arrays
	                           , let Just (GHI coords' ghi') = lookup coords ghis
	                           , let ghi = ghi' *~ (mega joule / square meter / day) :: PowerPerArea Float
	                           , city == closestCity coords
	                           , coords == coords']
	                   | city <- cities]

type PowerPerArea = Quantity (Dim Zero Pos1 Neg3 Zero Zero Zero Zero)

revenue :: Energy Float -> City -> Currency Float
revenue energy city = energy' * energyUnitPrice
	where targetEnergy = people * energyPerPersonPerYear * (1 *~ year) :: Energy Float
	      people = (fromIntegral (population city) *~ one) :: Dimensionless Float
	      energy' = min energy targetEnergy :: Energy Float

farmEnergy :: PowerPerArea Float -> Energy Float
farmEnergy ghi = ghi * farmArea * farmEfficiency * (1 *~ year)

transmissionLoss :: [Double] {- lng, lat -} -> City -> Dimensionless Float
transmissionLoss coords city = cityDist coords city * transmissionLossRate

closestCity :: [Double] {- lng, lat -} -> City
closestCity coords = snd $ minimum [(cityDist coords city, city) | city <- cities]

cityDist :: [Double] {- lng, lat -} -> City -> Length Float
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
hsin t = let u = sin (t / _2) in u * u
 
-- The distance between two points, given by latitude and longtitude, on a
-- circle.  The points are specified in radians.
circleDist radius (lat1, lng1) (lat2, lng2) =
  let hlat = hsin (lat2 - lat1)
      hlng = hsin (lng2 - lng1)
      root = sqrt (hlat + cos lat1 * cos lat2 * hlng)
  in _2 * radius * asin (min _1 root)
 
-- The approximate distance, in metres, between two points on Earth.  
-- The latitude and longtitude are assumed to be in degrees.
earthDist p1' p2' = circleDist (6372.8 *~ kilo meter) p1 p2
 where (p1, p2) = tmap (tmap (*~ degree)) (p1', p2')
       tmap f (v1, v2) = (f v1, f v2)
