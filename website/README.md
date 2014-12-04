# Solar Powering Australia

Your aim is to power Australia completely with Solar Energy.
The team that can power australia while making the most money will be victorious!

## The details
### The aim
Your aim is to place solar farms in such a way that you can minimize costs and maximize revenues.
The best areas to place solar farms will be those that are given the most energy from the Sun. 

### Input data
The input data gives us the GHI (Global Horizontal Irradiance) for the entire area of Australia.

<iframe style="width: 720px; height: 500px; border: none;" src="http://aremi.nicta.com.au?vis_str=%7B%22layers%22%3A%22%5B%7B%5C%22name%5C%22%3A%5C%22GHI%20mean%20July%5C%22%2C%5C%22type%5C%22%3A%5C%22WMS%5C%22%2C%5C%22url%5C%22%3A%5C%22http%3A%2F%2Fwww.ga.gov.au%2Fgisimg%2Fservices%2Fenergy%2FSolar_Energy_GHI_Mean%2FMapServer%2FWMSServer%3Fservice%3Dwms%26request%3DGetMap%26layers%3DGHI%2520mean%2520July%5C%22%2C%5C%22extent%5C%22%3A%7B%5C%22west%5C%22%3A1.7453292519943295%2C%5C%22south%5C%22%3A-0.767944870877505%2C%5C%22east%5C%22%3A2.775073510670984%2C%5C%22north%5C%22%3A-0.06981317007977318%7D%7D%5D%22%2C%22version%22%3A%220.0.02%22%2C%22camera%22%3A%22%7B%5C%22west%5C%22%3A1.9832302403277307%2C%5C%22south%5C%22%3A-0.7525781642655125%2C%5C%22east%5C%22%3A2.5826797903530965%2C%5C%22north%5C%22%3A-0.1531286142401468%7D%22%7D" allowFullScreen mozAllowFullScreen webkitAllowFullScreen></iframe>

We've prepare the data as [GeoJSON file](http://geojson.org/geojson-spec.html).
The dataset consists of 839 by 769 tiles 5km high and 5km wide.
For each tile there is an `avgGHI` property.
The average GHI indicates how many MegaJoules of energy that tile recieves per day from the Sun.

You can [download the data as GeoJSON here](/data/ghis.geojson.zip).

### Output data
Your output should be a subset of the tiles in the GHI GeoJSON.
Each tile in the GeoJSON should have the property `panels` which indicates the number of panels in the solar farm.
Don't include panels which do not have solar farms on them.

### Submission

You can submit your file from the command line via curl.
Here's an example of a submission of `solarfarms.geojson`:

```bash
$ curl localhost:3000/submit --form "submission=@solarfarms.geojson" --form "team=winners"
Thanks team 'winners'! You are ranked 1. With a score of 1.337e8
```

## The cost function
Australia needs a lot of energy per year. The amount you should be able to produce in KWh is
```
energy-needed = 239.3e12
```

The basic function for the amount of money that can be made from a set of solar panels is
```
profit = min(energy-produced, energy-needed) * energy-price - solar-farm-cost
```

The amount of energy produced is
```
energy-produced = energy-produced-per-tile for each tile
```

The amount of energy that can be produced by a solar panel on a particular tile in a year is
```
energy-produced-per-tile = min(number-of-panels, max-panel-area) * tile-ghi * (KWh/MJ) * panel-efficiency * 365
max-panel-area = 2500^2
panel-efficiency = 0.4
KWh/MJ = 3.6
```
Note that each panel is 1m^2 and there is a maximum of 2500m^2 worth of solar panels.

You can sell your energy for 12 cents per KWh
```
energy-price = 0.12
```

The cost of a solar farm is
```
solar-farm-cost = standalone-cost + number-of-panels * panel-cost
standalone-cost = 55e6 (if not next to a tile containing another solar farm)
panel-cost = 600
```

