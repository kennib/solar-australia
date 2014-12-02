# Solar Powering Australia

The aim of the game is to power Australia completely with Solar Energy.
The team that can produce the most money for the least money spent on solar arrays will be victorious!

## The cost function
Australia needs a lot of energy per year. The amount you should be able to produce in KWh is
```
energy-needed = 239.3e12
```

The basic function for the amount of money that can be made from a set of solar panels is
```
profit = min(energy-produced, energy-needed) * energy-price - number-of-panels * panel-cost
```

The amount of energy produced is
```
energy-produced = energy-produced-per-tile for each tile
```

The amount of energy that can be produced by a solar panel on a particular tile in a year is
```
energy-produced-per-tile = min(number-of-panels, max-panel-area) * tile-ghi * (KWh/J) * panel-efficiency * 365
max-panel-area = 2500^2
panel-efficiency = 0.4
KWh/MJ = 3.6
```
Note that there is a maximum of 5km^2 worth of solar panels.

You can sell your energy for 12 cents per KWh
```
energy-price = 0.12
```

The cost per panel of 1m^2 is
```
panel-cost = 600
```

## How to submit your solar arrays

```bash
$ curl localhost:3000/submit --form "submission=@solararrays.geojson" --form "team=winners"
Thanks team 'winners'! You are ranked 1. With a score of 1337.5
```
