Nation Hockey League
========================================================
## Prediction of Goals per Game without Overtime

## author: Vladimir Poliakov
## date:   May 2016

Introduction
========================================================

The aim of the study is to predict the Total Goals per Game in National Hockey League (NHL) in the Season 2015 - 2016 with the information from the previous games by team (home and visitors). The study is interesting for all NHL fans and could be used as information at the betting.

- Datasource: http://eishockey.wettpoint.com/liga/nhl-usa.html
- 10 Pedictors

Datasource
========================================================

Manuelly Collecting (http://eishockey.wettpoint.com/liga/nhl-usa.html) 
and prepared with Analytical Functions from Oracle Express Database


```r
observation_file <- read.csv("./data/V_NHL_15_16_DATA_MINING.csv", fileEncoding="UTF-8")
summary(observation_file)
```

Histogram of Goals per Game
========================================================

![plot of chunk unnamed-chunk-2](NHL_GpG-figure/unnamed-chunk-2-1.png) 

Are you interested?
========================================================

- visit the application on shinyapps.io (https://vladimir.shinyapps.io/Project_NHL_GpG/)
- visit the application source on github (https://github.com/VladiPol/Developing_Data_Products) 
- enroll on [Coursera Data Science Specialization](https://www.coursera.org/specialization/jhudatascience/1) and learn how to build your own
