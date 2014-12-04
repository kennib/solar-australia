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
The dataset consists of 839 by 679 points at the centre of areas 5km high and 5km wide.
For each tile there is an `avgGHI` property.
The average GHI indicates how many MegaJoules per square metre that area recieves per day from the Sun.

You can [download the data as GeoJSON here](/data/ghis.geojson.zip).

### Output data
Your output should be a subset of the points given in the GHI GeoJSON.

You can [download an example of GeoJSON with three solar farms here](/data/solar-farms.geojson).

### Submission

You can submit your file from the command line via curl.
Here's an example of a submission of `solarfarms.geojson`:

```bash
$ curl localhost:3000/submit --form "submission=@solarfarms.geojson" --form "team=winners"
Thanks team 'winners'! You are ranked 1. With a score of 1.337e8
```

## The cost function
Australia needs a lot of energy per year. The amount you should be able to produce is
```
energy-needed = 239.3e12 KWh
```

---

The amount of money that can be made from the solar farms is
```
profit = (min(energy-produced, energy-needed) * energy-price) - (solar-farm-cost * number-of-solar-farms)
where (energy-price) = $0.12 per KWh
```
Note that you can only sell energy if it will be used.

---

The total amount of energy produced by a set of solar panels is
```
energy-produced = solar-farm-energy * number-of-solar-farms
```

---

Each solar farm is based off the [Topaz Solar Farm](http://en.wikipedia.org/wiki/Topaz_Solar_Farm) (the largest solar farm in the world).
So each farm is 5km by 5km and costs $2.5 billion dollars.
```
solar-farm-cost = $2.5e9
solar-farm-energy = (ghi MJ/m^2/day) * (5000 m * 5000 m) * (MJ/KWh) * (365 days) * (1 - transmission-loss)
transmission-loss = (distance-to-city metres) * (1e-7 per metre)
where (MJ/KWh) = 3.6
```
Note that the ghi will vary per solar farm. It's your job to optimise this number.



