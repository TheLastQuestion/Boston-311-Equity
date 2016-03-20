### SETUP FILE: Imports data, cleans it for analysis
#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
# if running for first time, uncomment all commands
#===================================================

# ----- set up work directory
#setwd(dirname(file.choose())) # find a file in workdir

### libraries
#===================================================
library(lubridate)
library(dplyr)
library(xts)
library(ggplot2)
library(rgdal)
library(scales)
library(ggmap)
library(dplyr)
library(Cairo)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(streamgraph)
library(plotly)
library(leaflet)
library(acs)
library(stargazer)

#===================================================

### Downloading + saving data from boston's website
#===================================================
# ----- RUN ONCE
# ----- get most recent 311 data from boston
# ----- note this is big file, may take a while
url <- "https://data.cityofboston.gov/api/views/awu8-dc52/rows.csv?accessType=DOWNLOAD"
downloaddir <- getwd()
destname <- "bos311_full.csv"
download.file(url, destname)

### Subsetting data by ticket open timestamp
#===================================================
# ----- create dataframe from downloaded data
tickets <- read.csv("bos311_full.csv")
# ----- parse date to format easier to subset
tickets$date2 <- parse_date_time(tickets$OPEN_DT, 'mdY HMS')
tickets$open_yr <- year(tickets$date2)
tickets$open_mon <- month(tickets$date2)
tickets$open_day <- day(tickets$date2)
tickets$open_hour <- hour(tickets$date2)
tickets$open_dow <- weekdays(tickets$date2)
tickets$open_mon_str <- months(tickets$date2)

#  View(tickets[,c("date2","open_yr","open_mon","open_day"
#                  ,"open_hour","open_dow")])
yr10 <- subset(tickets,tickets$open_yr == 2010) #0 obs
yr11 <- subset(tickets,tickets$open_yr == 2011) #58,289 obs
yr12 <- subset(tickets,tickets$open_yr == 2012) #118,214 obs
yr13 <- subset(tickets,tickets$open_yr == 2013) #143,539 obs
yr14 <- subset(tickets,tickets$open_yr == 2014) #149,409 obs
yr15 <- subset(tickets,tickets$open_yr == 2015) #213,209 obs

