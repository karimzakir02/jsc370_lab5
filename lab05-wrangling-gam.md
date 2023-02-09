Lab 05 - Data Wrangling
================

# Learning goals

-   Use the `merge()` function to join two datasets.
-   Deal with missings and impute data.
-   Identify relevant observations using `quantile()`.
-   Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 4.1.3

``` r
library(dtplyr)
```

    ## Warning: package 'dtplyr' was built under R version 4.1.3

``` r
library(leaflet)
```

    ## Warning: package 'leaflet' was built under R version 4.1.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.1.3

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-36. For overview type 'help("mgcv-package")'.

``` r
library(purrr)
```

    ## Warning: package 'purrr' was built under R version 4.1.3

    ## 
    ## Attaching package: 'purrr'

    ## The following object is masked from 'package:data.table':
    ## 
    ##     transpose

``` r
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
```

``` r
# Download the data
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

``` r
met <- fread("met_all.gz")
head(met)
```

    ##    USAFID  WBAN year month day hour min  lat      lon elev wind.dir wind.dir.qc
    ## 1: 690150 93121 2019     8   1    0  56 34.3 -116.166  696      220           5
    ## 2: 690150 93121 2019     8   1    1  56 34.3 -116.166  696      230           5
    ## 3: 690150 93121 2019     8   1    2  56 34.3 -116.166  696      230           5
    ## 4: 690150 93121 2019     8   1    3  56 34.3 -116.166  696      210           5
    ## 5: 690150 93121 2019     8   1    4  56 34.3 -116.166  696      120           5
    ## 6: 690150 93121 2019     8   1    5  56 34.3 -116.166  696       NA           9
    ##    wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc ceiling.ht.method
    ## 1:              N     5.7          5      22000             5                 9
    ## 2:              N     8.2          5      22000             5                 9
    ## 3:              N     6.7          5      22000             5                 9
    ## 4:              N     5.1          5      22000             5                 9
    ## 5:              N     2.1          5      22000             5                 9
    ## 6:              C     0.0          5      22000             5                 9
    ##    sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp temp.qc dew.point
    ## 1:        N    16093           5       N          5 37.2       5      10.6
    ## 2:        N    16093           5       N          5 35.6       5      10.6
    ## 3:        N    16093           5       N          5 34.4       5       7.2
    ## 4:        N    16093           5       N          5 33.3       5       5.0
    ## 5:        N    16093           5       N          5 32.8       5       5.0
    ## 6:        N    16093           5       N          5 31.1       5       5.6
    ##    dew.point.qc atm.press atm.press.qc       rh
    ## 1:            5    1009.9            5 19.88127
    ## 2:            5    1010.3            5 21.76098
    ## 3:            5    1010.6            5 18.48212
    ## 4:            5    1011.6            5 16.88862
    ## 5:            5    1012.7            5 17.38410
    ## 6:            5    1012.7            5 20.01540

3.  Merge the data as we did during the lecture.

``` r
data <- merge(
  # Data
  x = met,
  y = stations,
  # List of variables to match
  by.x = "USAFID",
  by.y = "USAF",
  # Which obs to keep?
  all.x = TRUE,
  all.y = FALSE
  )
```

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
met_avg_lz <- data %>% 
  group_by(USAFID) %>% 
  summarize(
    # can also use across and pass c(_colnames_) and function
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE)
  )
```

``` r
met_med_lz <- met_avg_lz %>% 
  summarize(
    temp = median(temp, na.rm = TRUE),
    wind.sp = median(wind.sp, na.rm = TRUE),
    atm.press = median(atm.press, na.rm = TRUE)
  )
```

``` r
median_temp_station <- met_avg_lz %>% 
  mutate(
    temp_diff = abs(temp - met_med_lz %>% pull(temp))
  ) %>% 
  arrange(temp_diff) %>% 
  slice(1) %>% pull(USAFID)

median_wind_station <- met_avg_lz %>% 
  mutate(
    wind_diff = abs(wind.sp - met_med_lz %>% pull(wind.sp))
  ) %>% 
  arrange(wind_diff) %>% 
  slice(1) %>% pull(USAFID)

median_press_station <- met_avg_lz %>% 
  mutate(
    press_diff = abs(atm.press - met_med_lz %>% pull(atm.press))
  ) %>% 
  arrange(press_diff) %>% 
  slice(1) %>% pull(USAFID)
```

``` r
median_stations <- data %>% 
  select(USAFID, lon, lat) %>% 
  distinct_all() %>% 
  filter(
    USAFID %in% c(median_temp_station, median_wind_station, median_press_station)
) %>% 
  slice(c(1, 2, 3))
```

``` r
median_stations
```

    ## Source: local data table [3 x 3]
    ## Call:   unique(`_DT2`[, .(USAFID, lon, lat)])[USAFID %in% c(median_temp_station, 
    ##     median_wind_station, median_press_station)][c(1, 2, 3)[between(c(1, 
    ##     2, 3), -.N, .N)]]
    ## 
    ##   USAFID   lon   lat
    ##    <int> <dbl> <dbl>
    ## 1 720458 -82.6  37.8
    ## 2 720929 -92.0  45.5
    ## 3 722238 -85.7  31.4
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
met_avg_lz <- data %>% 
  group_by(STATE, USAFID) %>% 
  summarize(
    # can also use across and pass c(_colnames_) and function
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE)
  )
```

    ## `summarise()` has grouped output by 'STATE'. You can override using the
    ## `.groups` argument.

``` r
met_med_lz <- met_avg_lz %>% 
  group_by(STATE) %>% 
  summarize(
    temp = median(temp, na.rm = TRUE),
    wind.sp = median(wind.sp, na.rm = TRUE),
    atm.press = median(atm.press, na.rm = TRUE)
  )
```

``` r
euclidean <- function(a, b) sqrt(sum((a - b)^2))

euclid_distance <- function(data) {
  state <- data$STATE
  temp <- data$temp
  wind.sp <- data$wind.sp
  atm.press <- data$atm.press

  if (is.na(temp) | is.na(wind.sp) | is.na(atm.press)) {
    return(NA)
  }

  state_values <- met_med_lz %>% 
    filter(STATE == state) %>% 
    select(c(temp, wind.sp, atm.press)) %>%
    as.data.frame()
  
  
  values <- c(temp, wind.sp, atm.press)
  tryCatch(euclidean(values, state_values), error = function(e) print(state_values))
  return(euclidean(values, state_values))
}
```

``` r
df <- met_avg_lz %>% 
  select(c(STATE, temp, wind.sp, atm.press)) %>%
  as.data.frame()

distances <- df %>% 
  split(1:nrow(df)) %>% 
  map(euclid_distance) %>% 
  bind_rows()
```

``` r
df <- met_avg_lz %>% 
  select(c(STATE, USAFID, temp, wind.sp, atm.press)) %>%
  as.data.frame()

df$diff <- t(distances)
```

``` r
q2_df <- df %>%
    group_by(STATE) %>%
    filter(diff == min(diff, na.rm=TRUE))
```

    ## Warning in min(diff, na.rm = TRUE): no non-missing arguments to min; returning
    ## Inf

    ## Warning in min(diff, na.rm = TRUE): no non-missing arguments to min; returning
    ## Inf

``` r
q2_df
```

    ## # A tibble: 46 x 6
    ## # Groups:   STATE [46]
    ##    STATE USAFID  temp wind.sp atm.press diff[,1]
    ##    <chr>  <int> <dbl>   <dbl>     <dbl>    <dbl>
    ##  1 AL    722286  26.4    1.68     1015.   0.0561
    ##  2 AR    723407  25.9    2.21     1015.   0.461 
    ##  3 AZ    722745  30.3    3.31     1010.   0.233 
    ##  4 CA    722970  22.8    2.33     1013.   0.300 
    ##  5 CO    724767  22.0    2.78     1014.   0.944 
    ##  6 CT    725087  22.6    2.13     1015.   0.346 
    ##  7 DE    724180  24.6    2.75     1015.   0     
    ##  8 FL    722106  27.5    2.71     1015.   0.0477
    ##  9 GA    723160  26.6    1.68     1015.   0.312 
    ## 10 IA    725480  21.4    2.76     1015.   0.199 
    ## # ... with 36 more rows

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

``` r
met_avg_coord <- data %>% 
  group_by(STATE) %>% 
  summarize(
    # can also use across and pass c(_colnames_) and function
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE),
  )
```

``` r
euclidean <- function(a, b) sqrt(sum((a - b)^2))

euclid_distance <- function(data) {
  state <- data$STATE
  lat <- data$lat
  lon <- data$lon

  if (is.na(lat) | is.na(lon)) {
    return(NA)
  }

  state_values <- met_avg_coord %>% 
    filter(STATE == state) %>% 
    select(lat, lon) %>%
    as.data.frame()
  
  
  values <- c(lat, lon)
  return(euclidean(values, state_values))
}
```

``` r
df <- data %>% 
  group_by(STATE, USAFID) %>% 
  summarize(
    lat = median(lat, na.rm = TRUE),
    lon = median(lon, na.rm=TRUE)
  ) %>% 
  as.data.frame() 
```

    ## `summarise()` has grouped output by 'STATE'. You can override using the
    ## `.groups` argument.

``` r
distances <- df[, c(1, 3, 4)] %>% 
  split(1:nrow(df)) %>% 
  map(euclid_distance) %>% 
  bind_rows()
```

``` r
q2_points <- merge(q2_df, df, by.x="USAFID", by.y="USAFID", all.x=TRUE, all.y=FALSE) %>% select(lat, lon)
q2_points$color = "RED"
```

``` r
df$diff <- t(distances)
```

``` r
df <- df %>%
    group_by(STATE) %>%
    filter(diff == min(diff, na.rm=TRUE))
```

``` r
df <- df[, c(3, 4)]
df$color = "BLUE"
```

``` r
to_map <- rbind(df, q2_points)
```

``` r
leaflet(to_map) %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, color = ~color)
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<div class="leaflet html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-0aced4fd17f050d753f8" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-0aced4fd17f050d753f8">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"https://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addCircles","args":[[33.178,35.258,34.257,36.78,39.05,41.51,39.133,28.474,32.633,41.691,44.889,40.483,40.711,38.065,37.578,30.718,41.876,38.981,44.533,43.322,45.147,38.096,32.321,45.807,35.582,48.39,40.961,43.567,40.033,35.003,38.051,42.207,40.28,35.417,42.6,40.85,41.597,33.967,44.381,36.009,31.106,40.219,37.358,44.204,47.104,44.359,39,43.064,26.585,33.212,31.183,29.709,32.516,34.383,32.167,33.812,31.536,36.047,34.498,35.593,35.831,37.152,36.162,38.137,39.472,40.033,39.643,39.674,37.9,40.708,39.551,37.307,41.91,41.533,41.736,41.334,42.643,41.453,42.267,41.465,42.554,41.986,41.117,40.068,42.542,42.147,43.205,44.45,43.344,44.359,45.543,45.443,44.339,45.698],[-86.782,-93.095,-111.339,-119.719,-105.51,-72.828,-75.467,-82.454,-83.6,-93.566,-116.101,-88.95,-86.375,-97.861,-84.77,-91.479,-71.021,-76.922,-69.667,-84.688,-94.507,-92.553,-90.078,-108.542,-79.101,-100.024,-98.314,-71.433,-74.35,-105.662,-117.09,-75.98,-83.115,-97.383,-123.364,-77.85,-71.412,-80.8,-100.285,-86.52,-98.196,-111.723,-78.438,-72.562,-122.287,-89.837,-80.274,-108.458,-81.861,-87.616,-90.471,-98.046,-92.041,-103.316,-110.883,-118.146,-82.507,-79.477,-82.71,-88.917,-90.646,-94.495,-97.089,-78.455,-76.17,-74.35,-79.916,-75.606,-85.967,-84.027,-97.651,-108.626,-70.729,-71.283,-72.651,-75.727,-77.056,-87.006,-84.467,-90.523,-92.401,-97.435,-111.966,-118.569,-113.766,-121.724,-71.503,-68.367,-72.518,-89.837,-94.051,-98.413,-105.541,-110.44],10,null,null,{"interactive":true,"className":"","stroke":true,"color":["BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED"],"weight":5,"opacity":0.5,"fill":true,"fillColor":["BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","BLUE","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED","RED"],"fillOpacity":0.2},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[26.585,48.39],"lng":[-123.364,-68.367]}},"evals":[],"jsHooks":[]}</script>

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

-   low: temp \< 20
-   Mid: temp >= 20 and temp \< 25
-   High: temp >= 25

``` r
met_avg_lz <- data %>% 
  group_by(STATE) %>% 
  summarize(
    records = n(),
    missing = sum(is.na(temp) | is.na(wind.sp) | is.na(atm.press)),
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE),
    stations = n_distinct(USAFID)
  )

met_avg_lz <- met_avg_lz %>% 
  mutate(clf = case_when(
    temp < 20 ~ "low",
    temp >= 25 ~ "high",
    TRUE ~ "mid"
  ))
```

Once you are done with that, you can compute the following:

-   Number of entries (records),
-   Number of NA entries,
-   Number of stations,
-   Number of states included, and
-   Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

``` r
met_avg_lz %>% 
  group_by(clf) %>% 
  summarize(
    amount = sum(records, na.rm = TRUE),
    missing = sum(missing, na.rm = TRUE),
    stations_num = sum(stations, na.rm = TRUE),
    states = n(),
    temp = mean(temp, na.rm=TRUE),
    wind.sp = mean(wind.sp, na.rm = TRUE),
    atm.press = mean(atm.press, na.rm = TRUE)
  )
```

    ## Source: local data table [3 x 8]
    ## Call:   `_DT6`[, .(records = .N, missing = sum(is.na(temp) | is.na(wind.sp) | 
    ##     is.na(atm.press)), temp = mean(temp, na.rm = TRUE), wind.sp = mean(wind.sp, 
    ##     na.rm = TRUE), atm.press = mean(atm.press, na.rm = TRUE), 
    ##     stations = uniqueN(USAFID)), keyby = .(STATE)][, `:=`(clf = fcase(temp < 
    ##     20, "low", temp >= 25, "high", rep(TRUE, .N), "mid"))][, 
    ##     .(amount = sum(records, na.rm = TRUE), missing = sum(missing, 
    ##         na.rm = TRUE), stations_num = sum(stations, na.rm = TRUE), 
    ##         states = .N, temp = mean(temp, na.rm = TRUE), wind.sp = mean(wind.sp, 
    ##             na.rm = TRUE), atm.press = mean(atm.press, na.rm = TRUE)), 
    ##     keyby = .(clf)]
    ## 
    ##   clf    amount missing stations_num states  temp wind.sp atm.press
    ##   <chr>   <int>   <int>        <int>  <int> <dbl>   <dbl>     <dbl>
    ## 1 high   811126  575696          555     12  27.0    2.43     1014.
    ## 2 low    430794  338632          259     11  18.7    2.55     1014.
    ## 3 mid   1135423  759000          781     25  22.6    2.39     1015.
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

-   using your data with the median values per station, examine the
    association between median temperature (y) and median wind speed
    (x). Create a scatterplot of the two variables using ggplot2. Add
    both a linear regression line and a smooth line.

-   fit both a linear model and a spline model (use `gam()` with a cubic
    regression spline on wind speed). Summarize and plot the results
    from the models and interpret which model is the best fit and why.

``` r
met_avg_lz <- data %>% 
  group_by(USAFID) %>% 
  summarize(
    temp = median(temp, na.rm = TRUE),
    wind.sp = median(wind.sp, na.rm = TRUE),
  )
```

``` r
as.data.frame(met_avg_lz) %>% 
  ggplot(mapping=aes(wind.sp, temp)) +
  geom_point() +
  geom_smooth(method='lm',col="red") +
  geom_smooth(col="blue")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 16 rows containing non-finite values (stat_smooth).

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 16 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 16 rows containing missing values (geom_point).

![](lab05-wrangling-gam_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
linear_model <- lm(temp ~ wind.sp, data=met_avg_lz)
summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = temp ~ wind.sp, data = met_avg_lz)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -17.7243  -2.6518  -0.2309   2.7691  14.5052 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 22.23088    0.21779  102.08  < 2e-16 ***
    ## wind.sp      0.48614    0.08212    5.92 3.94e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.849 on 1577 degrees of freedom
    ##   (16 observations deleted due to missingness)
    ## Multiple R-squared:  0.02174,    Adjusted R-squared:  0.02112 
    ## F-statistic: 35.05 on 1 and 1577 DF,  p-value: 3.941e-09

``` r
df <- as.data.frame(met_avg_lz)
gam_model <- gam(temp ~ s(wind.sp, bs="cr", k=4), data=df)
summary(gam_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## temp ~ s(wind.sp, bs = "cr", k = 4)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 23.38566    0.09548   244.9   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##              edf Ref.df    F p-value    
    ## s(wind.sp) 2.967  2.999 27.8  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.0489   Deviance explained = 5.07%
    ## GCV =  14.43  Scale est. = 14.393    n = 1579

The spline model is slightly better, since it explains 2% more variation
in the response variable when compared to the linear model
