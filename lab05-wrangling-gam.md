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

euclid_distance <- function(state) {
  print(state)
  if (is.na(temp) | is.na(wind.sp) | is.na(atm.press)) {
    return(NA)
  }
  state_values <- met_med_lz %>% 
    filter(STATE == state) %>% 
    select(c(temp, wind.sp, atm.press))
  
  values <- c(temp, wind.sp, atm.press)
  
  return(euclidean(values, state_values))
}
```

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

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
    ## Call:   `_DT4`[, .(records = .N, missing = sum(is.na(temp) | is.na(wind.sp) | 
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
