ST558 Project 1
================
Aries Zhou
10/3/2021

#### Listing the packages used in this prject.

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
```

#### Connects to the One Call API.

The One Call API provides the following weather data for any
geographical coordinates:  
\- Current weather  
\- Minute forecast for 1 hour  
\- Hourly forecast for 48 hours  
\- Daily forecast for 7 days  
\- National weather alerts  
\- Historical weather data for the previous 5 days

These data can be accessed by defining the following parameters in the
URL.

Parameters:

  - `lat` `(required)` - Latitude; geographical coordinates that have
    values between -180 and 180.  
  - `lon` `(required)` - Longitude; geographical coordinates that have
    values between -180 and 180.  
  - `appid` `(required)` - Your unique API key  
  - `exclude` `(optional)` - You can use it to exclude some parts from
    the data, available values are `current`, `minutely`, `hourly`,
    `daily`, `alerts`, should be a comma-delimited list (without
    spaces).  
  - `units` `(optional)` - Units of measurement. `standard` (Kelvin &
    meter/sec), `metric` (Celsius & meter/sec) and `imperial`
    (Fahrenheit & miles/hour) units are available. If you do not use the
    `units` parameter, `standard` units will be applied by default.  
  - `lang` `(optinal)` - You can use this parameter to get the output in
    your language.

This section of code is written to get access to daily forecast for 7
days and government weather alerts around the globe at latitude 35.78
(where the North Carolina State University is located). Multiple URLs
are returned with the `get.URL` function. Then the data accessed with
the URLS are parsed and saved into a list called `Data.F7`.

``` r
#Define values for the parameters
lat <- 35.78
lon <- seq(from = -180, to = 180, by = 4.3)
exclude <- "current,minutely,hourly"
units <- "metric"
appid <- "411f596c9a67840a793286c1f7f3d0c1" # API key

#Function to get a vector containing a list of URL to request datasets from API.
get.URL <- function(lat, lon, appid, exclude=NULL, units = NULL, lang = NULL, ...){
        base_url<- "https://api.openweathermap.org/data/2.5/onecall?"
        if (is.null(lat)){
        URL <- paste(base_url, "lat=", lat, sep = "")    
        }
        if (is.null(lon)){
        URL <- paste(base_url, "lat=", lat, "&lon=", lon, sep = "")    
        }
        if (is.null(appid)){
        URL <- paste(base_url, "lat=", lat, "&lon=", lon, "&appid=", appid, sep = "")
        }
        if (is.null(exclude)){
        URL <- paste(base_url, "lat=", lat, "&lon=", lon, "&exclude=", tolower(exclude), "&appid=", appid, sep = "")
        }
        if (is.null(units)){
        URL <- paste(base_url, "lat=", lat, "&lon=", lon, "&exclude=", tolower(exclude), "&units=", units, "&appid=", appid, sep = "")
        }
        if (is.null(lang)){
        URL <- paste(base_url, "lat=", lat, "&lon=", lon, "&exclude=", tolower(exclude), "&units=", units, "&appid=", appid, sep = "")
        }
        else{
        return("Error: Invalid input")
        }
        URL
}

#Call the function to get URLs
URL <- get.URL(lat=lat, lon=lon, appid=appid, exclude=exclude, units=units)        

#Use for loop to get a list of data parsed from API and save them into an object called Data.F7.
Data.raw <- list()
Data.F7 <- list()

for (i in 1:length(URL)) {
j <- i
Data.raw[[j]] <- httr::GET(URL[i])
Data.F7[[j]] <- Data.raw[[j]]$content %>% rawToChar() %>% fromJSON()
}
```

#### Data Manipulation

Create a `select.data` function to choose the parts I want from each
listed data from API.

``` r
select.data <- function(Data.F7, k){
        Data.a <- Data.F7[[k]][["daily"]] %>% 
            select(dt, pressure:dew_point, starts_with("wind"))
        Data.b <- data.frame(Data.F7[[k]][["daily"]]$temp[2], 
                      Data.F7[[k]][["daily"]]$temp[3], 
                      Data.F7[[k]][["daily"]]$feels_like[1], 
                      Data.F7[[k]][2])
        Data.ab <- cbind(Data.a, Data.b)
        return(Data.ab) #return a horizontally combined data form one URl
}
```

Call the `select.data` function inside the `get.full.data` function to
combine all samll datasets togeter.

``` r
get.full.data <- function(Data){
  Data.a <- data.frame()
  Data.b <- data.frame()
  Full.data <- data.frame()

  for (k in 1:length(Data)){
    New.data <- select.data(Data, k)
    Full.data <- bind_rows(New.data, Full.data, )
  }
  return(Full.data) #return a vertically combined full data form all URls 
}

#called the get.full.data function to get a dataframe containing all datasets together.
Full.data  <- get.full.data(Data = Data.F7)
```

Creating two categorical variables and some statistics.

``` r
weather <- Full.data %>% 
        mutate(area = if_else((lon>=-120.74)&(lon<=-75.59), "Land", 
                      if_else((lon>=-5.91)&(lon<=120.44), "Land", "Water")), # divide the data into land area and water/ocean area according to the longitude values. 
               wind.status = if_else(wind_speed>=8.9, "Windy", 
                      if_else(wind_speed>=6.7, "Breezy", "Clam")), # define the wind status.
               temp.day = (min+max)/2) %>% # calculate the average  daily temperature.
        as_tibble()  
```

``` r
#Take a look at the data, only showing the first 10 observations.
knitr::kable(head(weather, n = 10))
```

|         dt | pressure | humidity | dew\_point | wind\_speed | wind\_deg | wind\_gust |   min |   max |   day |   lon | area  | wind.status | temp.day |
| ---------: | -------: | -------: | ---------: | ----------: | --------: | ---------: | ----: | ----: | ----: | ----: | :---- | :---------- | -------: |
| 1633647600 |     1023 |       83 |      19.99 |        8.64 |       206 |      12.06 | 22.28 | 24.13 | 23.64 | 176.9 | Water | Breezy      |   23.205 |
| 1633734000 |     1026 |       82 |      18.92 |        9.79 |        56 |       9.44 | 22.02 | 23.09 | 22.55 | 176.9 | Water | Windy       |   22.555 |
| 1633820400 |     1023 |       85 |      20.89 |        4.50 |       122 |       4.98 | 23.04 | 23.84 | 24.22 | 176.9 | Water | Clam        |   23.440 |
| 1633906800 |     1026 |       78 |      17.56 |       12.50 |        43 |      12.44 | 20.98 | 23.03 | 21.90 | 176.9 | Water | Windy       |   22.005 |
| 1633993200 |     1031 |       76 |      17.00 |       10.58 |        57 |      10.40 | 20.93 | 22.57 | 21.65 | 176.9 | Water | Windy       |   21.750 |
| 1634079600 |     1026 |       88 |      21.02 |        7.16 |       100 |       8.08 | 22.85 | 23.35 | 23.93 | 176.9 | Water | Breezy      |   23.100 |
| 1634166000 |     1023 |       89 |      21.46 |        6.18 |       124 |       8.52 | 23.29 | 23.59 | 24.19 | 176.9 | Water | Clam        |   23.440 |
| 1634252400 |     1021 |       90 |      22.00 |        8.60 |       137 |      12.86 | 23.39 | 23.75 | 24.41 | 176.9 | Water | Breezy      |   23.570 |
| 1633651200 |     1024 |       73 |      17.85 |        9.48 |        42 |       9.22 | 22.39 | 23.16 | 23.21 | 172.6 | Water | Windy       |   22.775 |
| 1633737600 |     1026 |       69 |      16.54 |        8.41 |        72 |       8.03 | 22.28 | 22.88 | 22.45 | 172.6 | Water | Breezy      |   22.580 |

``` r
#Create a contingency table for area and wind status.
knitr::kable(table(weather$area, weather$wind.status))
```

|       | Breezy | Clam | Windy |
| :---- | -----: | ---: | ----: |
| Land  |     42 |  206 |    72 |
| Water |    129 |  100 |   123 |

#### Graphical Summaries

Below is a histogram that shows the distribution of the average
forecasting temperature of future 7 days for the land and water/ocean
area around the globe at latitude of 35.78. From the graph, I observed
that the distributions of average temperature are highly skewed for both
land and water area. The water area tend to have higher daily
temperature in the future 7 days.

``` r
#Histogram for Temperature
his.temp <- ggplot(data = weather, aes(x = temp.day))
his.temp + geom_histogram(aes(fill = as.factor(area))) +
  labs(x="Average Daily Temperature", title = "Histogram for Temperature") + 
  scale_fill_discrete(name = "Area")
```

<img src="./docs/histogram-1.png" style="display: block; margin: auto;" />

Below is a boxplot that shows the distribution of the average daily
temperature for the land and water/ocean area around the globe at
latitude of 35.78. It confirms the trends displayed in the histogram
that the distributions of average temperature are highly skewed for both
areas. The water area tend to have higher average daily temperature in
the future 7 days.

``` r
#Boxplot for Temperature
box.temp <- ggplot(data = weather, aes(x = temp.day))
box.temp + geom_boxplot(aes(y = as.factor(area))) + 
  geom_jitter(aes(x = temp.day, y = as.factor(area), color = as.factor(area))) + 
labs(x ="Temperature", y ="Area", title = "Boxplot for Temperature") +
scale_color_discrete(name = "Area") + 
coord_flip() 
```

<img src="./docs/boxplot_temp-1.png" style="display: block; margin: auto;" />
From the barchart below, I found that the wind status on the land area
is more calm while it is more windy over the water area across the globe
at latitude 35.78 for the future 7 days.

``` r
#barchart for Wind Status
bar.wind <- ggplot(data = weather, aes(x = as.character(wind.status)))
bar.wind + geom_bar(aes(fill = as.factor(area)), position = "dodge") + 
  labs(x="Wind Status", title = "Bar Chart for Wind Status") + 
  scale_fill_discrete(name = "Area")
```

<img src="./docs/bar_chart-1.png" style="display: block; margin: auto;" />
This boxplot indicates that the humidity over the water areas is higher
in general than that on the land areas and the humidity varies more on
the land areas.

``` r
#boxplot for humidity
box.hum <- ggplot(data = weather, aes(x = humidity))
box.hum + geom_boxplot(aes(y = as.factor(area))) + 
  geom_jitter(aes(x = humidity, y = as.factor(area), color = as.factor(area))) + 
labs(x ="Humidity", y ="Area", title = "Boxplot for Humidity") +
scale_color_discrete(name = "Area") + 
coord_flip()
```

<img src="./docs/boxplot_hum-1.png" style="display: block; margin: auto;" />

This scatter plot indicates that there is a slight positive correlation
relationship between the temperature and humidity. Warm air tend to hold
more vapor than cool air.

``` r
correlation <- cor(weather$temp.day, weather$humidity)
knitr::kable(correlation)
```

|         x |
| --------: |
| 0.4642526 |

``` r
#Scatterplot for Wind Status v.s. Humidity.
scatter <- ggplot(data = weather, aes(x = temp.day, y = humidity))
scatter + geom_point(aes(color = as.factor(wind.status), shape = as.factor(area))) + 
geom_smooth(method = lm) + 
labs(title = "Avery Daily Temperature vs Humidity", x = "Avery Daily Temperature", y = "Humidity") + 
scale_color_discrete(name = "Wind Status") +
scale_shape_discrete(name = "Area")
```

<img src="./docs/scatter-1.png" style="display: block; margin: auto;" />
