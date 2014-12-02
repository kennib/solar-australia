# Solar Powering Australia

The aim of the game is to power Australia completely with Solar Energy.
The team that can produce the most money for the least money spent on solar arrays will be victorious!

## The cost function
Australia needs a lot of energy per year. The amount you should be able to produce in KWh is
```
energy-needed = 213e9
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
energy-produced-per-tile = min(number-of-panels, 5000*5000) * tile-ghi * (KWh/J) * 365
KWh/J = 0.2778
```

You can sell your energy for 12 cents per KWh
```
energy-price = 0.12
```

## How to submit your solar arrays

```bash
$ curl localhost:3000/submit --form "submission=@solararrays.geojson" --form "team=winners"
Thanks team 'winners'! You are ranked 1. With a score of 1337.5
```
