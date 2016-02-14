# install streamgraph
# devtools::install_github("hrbrmstr/streamgraph")
# install other packages, eg install.packages('stringi')

### libraries
library("lubridate")
library("dplyr")
library("xts")
library("ggplot2")
library("rgdal")
library("scales")
library("ggmap")
library("dplyr")
library("Cairo")
library("rgeos")
library(maptools)
library(sp)
library(raster)
library(streamgraph)





### setting work dir
workdir <- [insert path to work directory here]
## create subdirectories "raw311"
#################################################################

### reading in 311 data
### data downloaded from: https://data.cityofboston.gov/City-Services/311-Service-Tickets/awu8-dc52
### then saved in work directory subfolder /raw311

### create data frame
tickets<- read.csv(paste0(workdir, "raw311/311__Service_Requests.csv"))

### setting date range 
tickets$Date2 <- parse_date_time(tickets$OPEN_DT, 'mdY HMS')

tm10.1 <- "2009-12-31 23:59:59"
tm10.2 <- "2010-12-31 23:59:59"
tm11.1 <- "2010-12-31 23:59:59"
tm11.2 <- "2011-12-31 23:59:59"
tm12.1 <- "2011-12-31 23:59:59"
tm12.2 <- "2012-12-31 23:59:59"
tm13.1 <- "2012-12-31 23:59:59"
tm13.2 <- "2013-12-31 23:59:59"
tm14.1 <- "2013-12-31 23:59:59"
tm14.2 <- "2014-12-31 23:59:59"
tm15.1 <- "2014-12-31 23:59:59"
tm15.2 <- "2015-12-31 23:59:59"

### subsetting to date range
yr10 <- subset(tickets,as.Date(Date2) > as.Date(tm10.1))
yr10 <- subset(yr10,as.Date(Date2) < as.Date(tm10.2))
yr11 <- subset(tickets,as.Date(Date2) > as.Date(tm11.1))
yr11 <- subset(yr11,as.Date(Date2) < as.Date(tm11.2))
yr12 <- subset(tickets,as.Date(Date2) > as.Date(tm12.1))
yr12 <- subset(yr12,as.Date(Date2) < as.Date(tm12.2))
yr13 <- subset(tickets,as.Date(Date2) > as.Date(tm13.1))
yr13 <- subset(yr13,as.Date(Date2) < as.Date(tm13.2))
yr14 <- subset(tickets,as.Date(Date2) > as.Date(tm14.1))
yr14 <- subset(yr14,as.Date(Date2) < as.Date(tm14.2))
yr15 <- subset(tickets,as.Date(Date2) > as.Date(tm15.1))
yr15 <- subset(yr15,as.Date(Date2) < as.Date(tm15.2))

### Plotting tickets over time by year
yearByTickets <- data.frame(x = c(2010,2011,2012,2013,2014,2015),
      y = c(nrow(yr10),nrow(yr11),nrow(yr12),nrow(yr13),nrow(yr14),
            nrow(yr15)))
colnames(yearByTickets) <- c("Year","Tickets")

ggplot(yearByTickets, aes(x=Year,y=Tickets))+geom_line(aes(y=Tickets)) +
  ggtitle("311 Tickets by year, 2010-2015")+ xlab("Year")+ ylab("Tickets")


#################################################
#################################################
#######   Chapter 2.1.2 Geographic Variation  #############
#################################################
#################################################


#############################################
### Mapping 2015 ############################
#############################################

#### download census shapefiles here: http://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2010&layergroup=Block+Groups

####################################
#### choose year to look at ###########
yr15.chosentypes <- subset(yr15, TYPE==
  "Requests for Street Cleaning" | TYPE==
  "Equipment Repair" | TYPE==
  "Request for Snow Plowing" | TYPE==
  "Ground Maintenance" | TYPE==
  "Parking Enforcement"	| TYPE==
  "Abandoned Vehicles" | TYPE==
  "Graffiti Removal"	 | TYPE==
  "Needle Pickup" | TYPE==
  "Street Light Outages" | TYPE==	
  "PWD Graffiti" | TYPE==
  "Request for Pothole Repair" | TYPE==	
  "Rodent Activity" | TYPE==
  "Empty Litter Basket"	 | TYPE==
  "Abandoned Bicycle" | TYPE==
  "Sidewalk Repair (Make Safe)"	 | TYPE==
  "Parks Lighting/Electrical Issues" | TYPE==
  "Sign Repair"	 | TYPE==
  "Pest Infestation - Residential" | TYPE==
  "Tree Maintenance Requests"	 | TYPE==
  "Poor Conditions of Property" | TYPE==
  "Traffic Signal Inspection" | TYPE==	
  "Snow Removal" | TYPE==
  "Pick up Dead Animal"  | TYPE==
  "Unsatisfactory Living Conditions" | TYPE==
  "Missed Trash/Recycling/Yard Waste/Bulk Item"	| TYPE==
  "Unshoveled Sidewalk")

yr15.comments <- subset(yr15, TYPE== "General Comments For a Program or Policy")
                             
vc <- yr15
# .chosentypes
#######################################

#### loading shapefile
tract <- shapefile(paste0(workdir,"Shapefiles/tl_2010_25025_bg10/tl_2010_25025_bg10.shp"))
tract <- spTransform(x=tract, CRSobj=CRS("+proj=longlat +datum=WGS84"))
names(tract@data) <- tolower(names(tract@data))

#Convert 311 data to a spatial points object
vc <- SpatialPointsDataFrame(coords=vc[, c("LONGITUDE", "LATITUDE")],
                             data=vc[, c("OPEN_DT","CASE_STATUS", "SUBJECT", "REASON","TYPE","Source")],
                             proj4string=CRS("+proj=longlat +datum=WGS84"))

vc.constit <- subset(vc,Source == "Constituent Call")
vc.app <- subset(vc,Source == "Citizens Connect App")


#Each row entry represents one 311 request, so, add count column
vc@data$count <- 1
vc.constit@data$count <- 1
vc.app@data$count <- 1

#Spatial overlay to identify census polygon in which each 311 request point falls
#The Result `vc_tract` is a dataframe with the tract data for each point
vc_tract <- over(x=vc, y=tract)
vc_tract_constit <- over(x=vc.constit, y=tract)
vc_tract_app <- over(x=vc.app, y=tract)

#Add tract data to Points
vc@data <- data.frame(vc@data, vc_tract)
vc.constit@data <- data.frame(vc.constit@data, vc_tract_constit)
vc.app@data <- data.frame(vc.app@data, vc_tract_app)


#Aggregate 311 tickets by block 
req_block <- aggregate(formula=count~tractce10, data=vc@data, FUN=length)
req_block_constit <- aggregate(formula=count~tractce10, data=vc.constit@data, FUN=length)
req_block_app <- aggregate(formula=count~tractce10, data=vc.app@data, FUN=length)


ticketsByTract <- req_block
colnames(ticketsByTract) <- c("id","count")
#write.csv(ticketsByTract,paste0(workdir,"tables/TicketsByTract.csv"))
ticketsByTract.constit <- req_block_constit
colnames(ticketsByTract.constit) <- c("id","count")
ticketsByTract.app <- req_block_app
colnames(ticketsByTract.app) <- c("id","count")



#Add number of 311 records to tracts object
m <- match(x=tract@data$tractce10, table=req_block$tractce10)
tract@data$reqs <- req_block$count[m]

#View(tract@data)

## reading shapefile in and storing as SpatialPolygonsDataFrame object
bos.shape <- readOGR(dsn = paste0(workdir,"Shapefiles/tl_2010_25025_bg10"), layer = "tl_2010_25025_bg10")

## dropping tracts not covered by 311 from future maps
bos.shape <- bos.shape[!grepl("980101", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("990101", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("981300", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("180500", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("180400", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("180301", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("180200", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("180101", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170800", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170702", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170701", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170601", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170502", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170501", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170400", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170300", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170200", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("170100", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("160602", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("160601", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("160502", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("160501", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("160400", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("160300", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("160200", bos.shape$TRACTCE10),]
bos.shape <- bos.shape[!grepl("160101", bos.shape$TRACTCE10),]

## transform data into df that ggplot can understand
# use "names(bos.shape)" to find options for region
bos.df <- fortify(bos.shape, region="TRACTCE10")

colnames(tract@data) <- c("state","county","id","blkgrp",
        "geoid10","name","mtfcc","funcstat","aland","awater","lat","long","reqs")

## joining map + count data
plotData <- merge(bos.df, tract@data,by="id")

## subsetting to lower reqs
plotData$reqs_limit <- plotData$reqs
plotData$reqs_limit <- ifelse(plotData$reqs>2000,2000,plotData$reqs)

plotData$reqsink <- plotData$reqs/1000
plotData$reqsink_limit <- ifelse(plotData$reqsink>3,3,plotData$reqsink)


plotData$lnreqs <- log(plotData$reqs)
  
## mapping total use
m <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
            fill = reqsink_limit), color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", direction=1,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Total 311 Tickets in 2015 (in '000s)",
       fill = "")
m
ggsave(m, file = paste0(workdir,"roughDraftScripts/Plots/Total_Tickets_2015.png"), width = 6, height = 4.5, type = "cairo-png")

##################################
##################################################
### bringing in acs data ####
####################################################
##################################

## reading in ACS race data
acs.race<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_B02001_with_ann_tract.csv"),
                    stringsAsFactors = FALSE)
## reading in ACS poverty data
acs.pov<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_S1701_with_ann_tract.csv"),
                   stringsAsFactors = FALSE)
## reading in ACS mean inc data
acs.meaninc<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_S1902_with_ann_tract.csv"),
                       stringsAsFactors = FALSE)
## reading in ACS median data
acs.medinc<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_S1903_with_ann_tract.csv"),
                      stringsAsFactors = FALSE)
## reading in ACS employment data
acs.emp<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_S2301_with_ann_tract.csv"),
                   stringsAsFactors = FALSE)
## reading in ACS education data
acs.educ<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_S1501_with_ann_tract_educ.csv"),
                   stringsAsFactors = FALSE)
## reading in ACS hispanic and latino data
acs.hisp<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_B03001_with_ann_hisp.csv"),
                    stringsAsFactors = FALSE)
## reading in ACS population data
acs.pop<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_B01003_with_ann_pop.csv"),
                    stringsAsFactors = FALSE)

## reading in gazetier data
acs.land<- read.table(paste0(workdir,"ACS_csvs/2015_Gaz_tracts_national.txt"),
                   sep="\t", header=TRUE, stringsAsFactors = FALSE)
acs.land$geo <-substr(acs.land$GEOID,1,5)
acs.land <- acs.land[grepl("25025", acs.land$geo),]



#### preparing acs data ####
acs.race$id <-substr(acs.race$GEO.id2,6,11)
acs.pov$id <-substr(acs.pov$GEO.id2,6,11)
acs.meaninc$id <-substr(acs.meaninc$GEO.id2,6,11)
acs.medinc$id <-substr(acs.medinc$GEO.id2,6,11)
acs.emp$id <-substr(acs.emp$GEO.id2,6,11)
acs.educ$id <- substr(acs.educ$GEO.id2,6,11)
acs.hisp$id <- substr(acs.educ$GEO.id2,6,11)
acs.pop$id <- substr(acs.educ$GEO.id2,6,11)
acs.land$id <- substr(acs.land$GEOID,6,11)


##################################################
### choose what ace variables to carry forward ####
####################################################

## race
acs.race.2 <- acs.race[,c("id", "HD01_VD02","HD01_VD03")]
colnames(acs.race.2) <- c("id", "white","black")
acs.race.2$id <- as.character(acs.race.2$id)
## pov
acs.pov.2 <- acs.pov[,c("id", "HC03_EST_VC01","HC01_EST_VC03","HC01_EST_VC05","HC01_EST_VC06")]
colnames(acs.pov.2) <- c("id", "Pct_Below_Pov_lev","under_18","age18to64","age65plus")
acs.pov.2$id <- as.character(acs.pov.2$id)
## meaninc
acs.meaninc.2 <- acs.meaninc[,c("id", "HC02_EST_VC02")]
colnames(acs.meaninc.2) <- c("id", "Mean_Inc")
acs.meaninc.2$id <- as.character(acs.meaninc.2$id)
## medinc
acs.medinc.2 <- acs.medinc[,c("id", "HC02_EST_VC02")]
colnames(acs.medinc.2) <- c("id", "Med_Inc")
acs.medinc.2$id <- as.character(acs.medinc.2$id)
acs.medinc.2$Med_Inc <- as.numeric(as.character(acs.medinc.2$Med_Inc))
## emp
acs.emp.2 <- acs.emp[,c("id", "HC04_EST_VC01")]
colnames(acs.emp.2) <- c("id", "Unemp")
acs.emp.2$id <- as.character(acs.emp.2$id)
## educ
acs.educ.2 <- acs.educ[,c("id","HC01_EST_VC02","HC01_EST_VC03",
      "HC01_EST_VC04","HC01_EST_VC05")]
colnames(acs.educ.2) <- c("id","less_than_hs","hs_grad","some_college","bach_or_higher")
acs.educ.2$id <- as.character(acs.educ.2$id)
## hisp
acs.hisp.2 <- acs.hisp[,c("id", "HD01_VD03")]
colnames(acs.hisp.2) <- c("id", "Hisp_or_Latin")
acs.hisp.2$id <- as.character(acs.hisp.2$id)
## pop
acs.pop.2 <- acs.pop[,c("id", "HD01_VD01")]
colnames(acs.pop.2) <- c("id", "pop")
acs.pop.2$id <- as.character(acs.pop.2$id)
## land area
acs.land.2 <- acs.land[,c("id","ALAND_SQMI")]
colnames(acs.land.2) <- c("id","sqmi")
acs.land.2$id <- as.character(acs.land.2$id)

## joining ACS datasets
acs.full <- left_join(acs.race.2, acs.pov.2)
acs.full <- left_join(acs.full,acs.meaninc.2)
acs.full <- left_join(acs.full,acs.medinc.2)
acs.full <- left_join(acs.full,acs.emp.2)
acs.full <- left_join(acs.full,acs.educ.2)
acs.full <- left_join(acs.full,acs.hisp.2)
acs.full <- left_join(acs.full,acs.pop.2)
acs.full <- left_join(acs.full,acs.land.2)

## looking at population density
acs.full$popdens <- as.numeric(as.character(acs.full$sqmi)) / 
  as.numeric(as.character(acs.full$pop))

## looking at % white 
acs.full$pct_white <- as.numeric(as.character(acs.full$white)) / 
  as.numeric(as.character(acs.full$pop))
acs.full$pct_white <- acs.full$pct_white * 100

## looking at % black 
acs.full$pct_black <- as.numeric(as.character(acs.full$black)) / 
  as.numeric(as.character(acs.full$pop))
acs.full$pct_black <- acs.full$pct_black * 100

## looking at % hispanic or latino in origin 
acs.full$pct_hisp_latin <- as.numeric(as.character(acs.full$Hisp_or_Latin)) / 
  as.numeric(as.character(acs.full$pop))
acs.full$pct_hisp_latin <- acs.full$pct_hisp_latin * 100

## joining map + acs data
plotData <- merge(plotData, acs.full,by="id")


##################################################
### mapping acs data ####
####################################################
plotData$pop <- as.integer(plotData$pop)

m <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
        fill = pop), color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", direction=1,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Total Population",
       fill = "")
m
ggsave(m, file = paste0(workdir,"roughDraftScripts/Plots/Population.png"), width = 6, height = 4.5, type = "cairo-png")

#################################################
#################################################
#######   Chapter 2.1.3 Variance over time   #############
#################################################
#################################################

yr15$count <- 1
yr15$Month <-format(yr15$Date2, format='%m')
yr15$yday <- yday(yr15$Date2)
yr15$dow <- weekdays(yr15$Date2)

## choose year
yr15.1 <- yr15

## aggregated by day of week
df.dow <- aggregate(yr15.1$count, list(yr15.1$dow), sum)
colnames(df.dow) <- c("dow","Tickets")

df.dow$Tickets <- as.numeric(df.dow$Tickets)

df.dow$dow <- factor(df.dow$dow, levels= c("Sunday", "Monday", 
         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

df.dow[order(df.dow$dow), ]

## plotting variance over days of the week
ggplot(df.dow, aes(dow, Tickets, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "Day of week", y = "Tickets", 
       title = "311 Tickets")

ggsave(paste0(workdir,"roughDraftScripts/Plots/TicketsByDOW2015.png"), dpi=300, width=5, height=3)

##### plotting variance over months of the year #####
## aggregated by month of year
df.month <- aggregate(yr15.1$count, list(yr15.1$Month), sum)
colnames(df.month) <- c("month","Tickets")

df.month$Tickets <- as.numeric(df.month$Tickets)

## plotting variance over months
ggplot(df.month, aes(month, Tickets, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "Month", y = "Tickets", 
       title = "311 Tickets")

ggsave(paste0(workdir,"roughDraftScripts/Plots/TicketsByMonth2015.png"), dpi=300, width=5, height=3)

## take out snow
yr15.1.nosnow <- yr15[!grepl("Request for Snow Plowing", yr15$TYPE),]

## aggregated by month of year
df.month.nosnow <- aggregate(yr15.1.nosnow$count, list(yr15.1.nosnow$Month), sum)
colnames(df.month.nosnow) <- c("month","Tickets")

df.month.nosnow$Tickets <- as.numeric(df.month.nosnow$Tickets)

## plotting variance over months
ggplot(df.month.nosnow, aes(month, Tickets, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "Month", y = "Tickets", 
       title = "311 Tickets (excluding snow tickets)")

ggsave(paste0(workdir,"roughDraftScripts/Plots/TicketsByMonth2015_nosnow.png"), dpi=300, width=5, height=3)

## plotting variance over year by day
## aggregated by day of week
df.day <- aggregate(yr15.1.nosnow$count, list(yr15.1.nosnow$yday), sum)
colnames(df.day) <- c("yday","Tickets")

df.day$Tickets <- as.numeric(df.day$Tickets)

ggplot(df.day, aes(yday, Tickets, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "Tickets", 
       title = "311 Tickets (excluding snow tickets)")

ggsave(paste0(workdir,"roughDraftScripts/Plots/TicketsByDay.png"), dpi=300, width=5, height=3)


#################################################
#################################################
#######   Chapter 2.1.4 Variance by source   #############
#################################################
#################################################

################ 2015 BY SOURCE ################
## aggregated by source
df.source.nosnow <- aggregate(yr15.1.nosnow$count, list(yr15.1.nosnow$Source), sum)
colnames(df.source.nosnow) <- c("Source","Tickets")

df.source.nosnow <- df.source.nosnow[order(df.source.nosnow$Tickets),]

row.names(df.source.nosnow) = c("Citizens Connect App",
        "City Workers App","Constituent Call","Employee Generated",
        "Maximo Integration","Self Service","Twitter")


png(paste0(workdir,"roughDraftScripts/Plots/RequestBySource.png"))

barplot(df.source.nosnow$Tickets, main="Tickets by Source 2015", xlab="Source", ylab="Tickets", 
        ylim=c(0,100000),names.arg=c("Twitter","Self Service","Maximo",
        "Employee","Constituent Call","City Workers App","Citizens Connect"),
        border="red")



################ 2014 BY SOURCE ################
## take out snow
yr12.nosnow <- yr12[!grepl("Request for Snow Plowing", yr12$TYPE),]
yr12.nosnow$count <- 1

## aggregated by source
df.source.nosnow.12 <- aggregate(yr12.nosnow$count, list(yr12.nosnow$Source), sum)
colnames(df.source.nosnow.12) <- c("Source","Tickets")

df.source.nosnow.12 <- df.source.nosnow.12[order(df.source.nosnow.12$Tickets),]

png(paste0(workdir,"roughDraftScripts/Plots/RequestBySource12.png"))

barplot(df.source.nosnow.12$Tickets, main="Tickets by Source 2012", xlab="Source", ylab="Tickets", 
        ylim=c(0,100000),names.arg=c("Twitter","Self Service","Maximo",
                    "Employee","Constituent Call","City Workers App","Citizens Connect"),
        border="red")

#################################################
#################################################
#######   Chapter 2.1.4 Tables by source   #############
#################################################
#################################################

yr15.CitApp <- subset(yr15, Source=="Citizens Connect App")
yr15.WorkApp <- subset(yr15, Source=="City Worker App")
yr15.ConstitCall <- subset(yr15, Source=="Constituent Call")
yr15.Employ <- subset(yr15, Source=="Employee Generated")


dir1 <- "CitApp"
dir2 <- "WorkApp"
dir3 <- "ConstitCall"
dir4 <- "Employ"

### Department Table
subjectcount<- data.frame(table(yr15.CitApp$SUBJECT))
write.table(subjectcount, file = paste0(workdir, "tables/",dir1,"/departments.csv"),
            sep = ",", row.names = FALSE)

### Call Reasons
reasoncount <- data.frame(table(yr15.CitApp$REASON))
write.table(reasoncount, file = paste0(workdir, "tables/",dir1,"/reasons.csv"),
            sep = ",", row.names = FALSE)
### Call Type
typecount <- data.frame(table(yr15.CitApp$TYPE))
write.table(typecount, file = paste0(workdir, "tables/",dir1,"/types.csv"),
            sep = ",", row.names = FALSE)

### Source
sourcecount <- data.frame(table(yr15.CitApp$Source))
write.table(sourcecount, file = paste0(workdir, "tables/",dir1,"/source.csv"),
            sep = ",", row.names = FALSE)


############################
### Department Table
subjectcount<- data.frame(table(yr15.WorkApp$SUBJECT))
write.table(subjectcount, file = paste0(workdir, "tables/",dir2,"/departments.csv"),
            sep = ",", row.names = FALSE)

### Call Reasons
reasoncount <- data.frame(table(yr15.WorkApp$REASON))
write.table(reasoncount, file = paste0(workdir, "tables/",dir2,"/reasons.csv"),
            sep = ",", row.names = FALSE)
### Call Type
typecount <- data.frame(table(yr15.WorkApp$TYPE))
write.table(typecount, file = paste0(workdir, "tables/",dir2,"/types.csv"),
            sep = ",", row.names = FALSE)

### Source
sourcecount <- data.frame(table(yr15.WorkApp$Source))
write.table(sourcecount, file = paste0(workdir, "tables/",dir2,"/source.csv"),
            sep = ",", row.names = FALSE)

############################
### Department Table
subjectcount<- data.frame(table(yr15.ConstitCall$SUBJECT))
write.table(subjectcount, file = paste0(workdir, "tables/",dir3,"/departments.csv"),
            sep = ",", row.names = FALSE)

### Call Reasons
reasoncount <- data.frame(table(yr15.ConstitCall$REASON))
write.table(reasoncount, file = paste0(workdir, "tables/",dir3,"/reasons.csv"),
            sep = ",", row.names = FALSE)
### Call Type
typecount <- data.frame(table(yr15.ConstitCall$TYPE))
write.table(typecount, file = paste0(workdir, "tables/",dir3,"/types.csv"),
            sep = ",", row.names = FALSE)

### Source
sourcecount <- data.frame(table(yr15.ConstitCall$Source))
write.table(sourcecount, file = paste0(workdir, "tables/",dir3,"/source.csv"),
            sep = ",", row.names = FALSE)

############################
### Department Table
subjectcount<- data.frame(table(yr15.Employ$SUBJECT))
write.table(subjectcount, file = paste0(workdir, "tables/",dir4,"/departments.csv"),
            sep = ",", row.names = FALSE)

### Call Reasons
reasoncount <- data.frame(table(yr15.Employ$REASON))
write.table(reasoncount, file = paste0(workdir, "tables/",dir4,"/reasons.csv"),
            sep = ",", row.names = FALSE)
### Call Type
typecount <- data.frame(table(yr15.Employ$TYPE))
write.table(typecount, file = paste0(workdir, "tables/",dir4,"/types.csv"),
            sep = ",", row.names = FALSE)

### Source
sourcecount <- data.frame(table(yr15.Employ$Source))
write.table(sourcecount, file = paste0(workdir, "tables/",dir4,"/source.csv"),
            sep = ",", row.names = FALSE)

tab.source <- merge(read.csv(paste0(workdir, "tables/CitApp/departments.csv")),
     read.csv(paste0(workdir, "tables/WorkApp/departments.csv")),by="Var1")
     
tab.source <- merge(tab.source,read.csv(paste0(workdir, "tables/ConstitCall/departments.csv")),by="Var1")

tab.source <- merge(tab.source,read.csv(paste0(workdir, "tables/Employ/departments.csv")),by="Var1")

colnames(tab.source) <- c("Source","Citizens Connect","City Worker","Constituent Call","Employee")
                     
#write.csv(tab.source,paste0(workdir,"roughDraftScripts/TicketsBySource.csv"))


#################################################
#################################################
#######   Chapter 2.2 Equity analysis   #############
#################################################
#################################################

## mapping percent non-white
plotData$pct_nonwhite <- 100- as.numeric(as.character(plotData$pct_white))

m <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pct_nonwhite), color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Blues", direction=1,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Percent non-White",
       fill = "")
m
ggsave(m, file = paste0(workdir,"roughDraftScripts/Plots/Percent_nonWhite.png"), width = 6, height = 4.5, type = "cairo-png")

## mapping per capita use
plotData$pc_count <- as.numeric(as.character(plotData$reqs)) /
  as.numeric(as.character(plotData$pop))

plotData$pc_count_limit <- plotData$pc_count
plotData$pc_count_limit <- ifelse(plotData$pc_count>.8,.8,plotData$pc_count)

m <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pc_count_limit), color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", direction=1,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Per capita tickets by tract (2015)",
       fill = "")
m

## mapping per square mil use
plotData$per_sqmi <- as.numeric(as.character(plotData$reqs)) /
  as.numeric(as.character(plotData$sqmi))

plotData$per_sqmi_limit <- plotData$per_sqmi
plotData$per_sqmi_limit <- ifelse(plotData$per_sqmi>10000,10000,plotData$per_sqmi)

plotData$per_sqmi_log <-log(plotData$per_sqmi)
plotData$per_sqmi_log_limit <- ifelse(plotData$per_sqmi_log>10,NA,plotData$per_sqmi_log)

m <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = per_sqmi_log), color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", direction=1,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Per square mile tickets by tract (2015), log-scale",
       fill = "")
m

#### scatter plot ####
## PER CAPITA 311 use Vs. % non-white##
scatData <- left_join(ticketsByTract,acs.full)
scatData.constit <- left_join(ticketsByTract.constit,acs.full)
scatData.app <- left_join(ticketsByTract.app,acs.full)

## adding metrics to full scat data
scatData$pc_count <- as.numeric(as.character(scatData$count)) /
  as.numeric(as.character(scatData$pop))
scatData$pct_nonwhite <- 100-as.numeric(as.character(scatData$pct_white))
scatData$pct_nonwhite <- ifelse(is.nan(scatData$pct_nonwhite),NA,scatData$pct_nonwhite)
scatData$pct_nonwhite <- ifelse(is.infinite(scatData$pct_nonwhite),NA,scatData$pct_nonwhite)
scatData$per_sqmi <- as.numeric(as.character(scatData$count)) /
  as.numeric(as.character(scatData$sqmi))

# adding metrics to constituent call scat data
scatData.constit$pc_count <- as.numeric(as.character(scatData.constit$count)) /
  as.numeric(as.character(scatData.constit$pop))
scatData.constit$pct_nonwhite <- 100-as.numeric(as.character(scatData.constit$pct_white))
scatData.constit$pct_nonwhite <- ifelse(is.nan(scatData.constit$pct_nonwhite),NA,scatData.constit$pct_nonwhite)
scatData.constit$pct_nonwhite <- ifelse(is.infinite(scatData.constit$pct_nonwhite),NA,scatData.constit$pct_nonwhite)
scatData.constit$per_sqmi <- as.numeric(as.character(scatData.constit$count)) /
  as.numeric(as.character(scatData.constit$sqmi))

# adding metrics to app scat data
scatData.app$pc_count <- as.numeric(as.character(scatData.app$count)) /
  as.numeric(as.character(scatData.app$pop))
scatData.app$pct_nonwhite <- 100-as.numeric(as.character(scatData.app$pct_white))
scatData.app$pct_nonwhite <- ifelse(is.nan(scatData.app$pct_nonwhite),NA,scatData.app$pct_nonwhite)
scatData.app$pct_nonwhite <- ifelse(is.infinite(scatData.app$pct_nonwhite),NA,scatData.app$pct_nonwhite)
scatData.app$per_sqmi <- as.numeric(as.character(scatData.app$count)) /
  as.numeric(as.character(scatData.app$sqmi))



scatData.limit <- subset(scatData,scatData$pc_count<1)
  
reg.pct_nonwhite <- lm(pc_count~as.numeric(pct_nonwhite),scatData.limit)

plot(x = scatData.limit$pct_nonwhite, y = scatData.limit$pc_count,
     xlab="non-White Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and % non-White (outliers removed)")
abline(reg.pct_nonwhite, col="Blue")
summary.lm(reg.pct_nonwhite)

reg.pct_hisp_latin <- lm(pc_count~as.numeric(pct_hisp_latin),scatData.limit)
plot(x = scatData.limit$pct_hisp_latin, y = scatData.limit$pc_count,
     xlab="Hispanic or Latin in Origin Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Hispanic/Latin (outliers removed)")
abline(reg.pct_hisp_latin, col="Blue")
summary.lm(reg.pct_hisp_latin)


reg.races <- lm(pc_count~as.numeric(pct_hisp_latin) + as.numeric(pct_nonwhite),scatData.limit)
summary.lm(reg.races)

##############################
### mapping median income ###
##############################

plotData$Med_Inc_k <- as.numeric(as.character(plotData$Med_Inc)) /1000

m <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
          fill = Med_Inc_k), color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Reds", direction=1,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Average Median Income in '000 Dollars",
       fill = "")
m


#### scatter plot ####
## PER CAPITA 311 use Vs. median income##
scatData$Med_Inc_k <- scatData$Med_Inc / 1000
scatData.limit$Med_Inc_k <- scatData.limit$Med_Inc / 1000
scatData.constit$Med_Inc_k <- scatData$Med_Inc / 1000
scatData.app$Med_Inc_k <- scatData.app$Med_Inc /1000

reg.medinc <- lm(pc_count~as.numeric(Med_Inc_k),scatData.limit)
plot(x = as.numeric(scatData.limit$Med_Inc_k), y = scatData.limit$pc_count,
     xlab="Median Income (in '000)", ylab="Per Capita 311 Use (outliers removed)")
abline(reg.medinc, col="Red")
summary.lm(reg.medinc)

## PER CAPITA 311 use Vs. LOG median income##
reg.medinc.ln <- lm(pc_count~as.numeric(log(Med_Inc_k)),scatData.limit)

plot(x = as.numeric(log(scatData.limit$Med_Inc_k)), y = scatData.limit$pc_count,
     xlab="Log of Median Income", ylab="Per Capita 311 Use")
abline(reg.medinc.ln, col="Red")

summary.lm(reg.medinc.ln)

## PER CAPITA 311 use Vs. POVERTY RATE##
reg.povlev <- lm(pc_count~as.numeric(Pct_Below_Pov_lev),scatData.limit)

plot(x = as.numeric(scatData.limit$Pct_Below_Pov_lev), y = scatData.limit$pc_count,
     xlab="Percent of Tract Below Poverty Level", ylab="Per Capita 311 Use")
abline(reg.povlev, col="Red")

summary.lm(reg.povlev)

## PER CAPITA 311 use Vs. EDUCATION ##
reg.educ <- lm(pc_count~as.numeric(hs_grad) + 
                 as.numeric(some_college) + as.numeric(bach_or_higher), 
               scatData.limit)
summary.lm(reg.educ)
# less_than_hs hs_grad some_college bach_or_higher

####################
## streamgraph ##################
###################

yr15_sg <- yr15 %>%
  # pick top 20 case titles
  filter(CASE_TITLE %in% names(sort(table(CASE_TITLE), decreasing=TRUE))[1:15]) %>%
  # Group by year-month (daily is too messy)
  # Need to add '-01' to make it a valid date for streamgraph
  mutate(yearmonth = paste0(format(as.Date(Date2), format="%Y-%m"), "-01")) %>%
  group_by(yearmonth, CASE_TITLE) %>%
  summarise(value = n())

# # Create streamgraph
# sg3 <- streamgraph(data = yr15_sg, key = "CASE_TITLE", value = "value", date = "yearmonth",
#                   offset = "silhouette", interpolate = "cardinal",
#                   width = "700", height = "400") %>%
#   sg_fill_brewer("PuOr") %>%
#   sg_legend(TRUE, "Case Title: ") %>%
#   sg_axis_x(tick_interval = 3, tick_units = "month", tick_format = "%m")
# 
# sg3

####################
## multiple regressions ##################
###################

## per capita tickets
reg.mult.pcticket <- lm(pc_count~as.numeric(pct_white) + 
                          as.numeric(pct_black) +
                          as.numeric(pct_hisp_latin) + 
                          as.numeric(Med_Inc_k) + 
                          as.numeric(hs_grad) + 
                          as.numeric(some_college) + 
                          as.numeric(bach_or_higher) +
                          as.numeric(popdens) + 
                          as.numeric(age18to64) + 
                          as.numeric(age65plus), 
               scatData.limit)

summary.lm(reg.mult.pcticket)

## per square mile tickets
reg.mult.persqml <- lm(per_sqmi~as.numeric(pct_white) + 
                         as.numeric(pct_black) +
                         as.numeric(pct_hisp_latin) + 
                         as.numeric(Med_Inc_k) + 
                         as.numeric(hs_grad) + 
                         as.numeric(some_college) + 
                         as.numeric(bach_or_higher) +
                         as.numeric(popdens) +
                         as.numeric(age18to64) + 
                         as.numeric(age65plus), 
                       scatData.limit)

summary.lm(reg.mult.persqml)

####################
##  ##################
###################
####### regressions based on source ##########
####################
##  ##################
###################

## per capita tickets and CONSTITUENT CALLS
reg.mult.pcticket.constit <- lm(pc_count~as.numeric(pct_white) + 
                          as.numeric(pct_black) +
                          as.numeric(pct_hisp_latin) + 
                          as.numeric(Med_Inc_k) + 
                          as.numeric(hs_grad) + 
                          as.numeric(some_college) + 
                          as.numeric(bach_or_higher) +
                          as.numeric(popdens) + 
                          as.numeric(age18to64) + 
                          as.numeric(age65plus), 
                        scatData.constit)

summary.lm(reg.mult.pcticket.constit)


## per capita tickets and CITIZENS CONNECT APP
reg.mult.pcticket.app <- lm(pc_count~as.numeric(pct_white) + 
                                  as.numeric(pct_black) +
                                  as.numeric(pct_hisp_latin) + 
                                  as.numeric(Med_Inc_k) + 
                                  as.numeric(hs_grad) + 
                                  as.numeric(some_college) + 
                                  as.numeric(bach_or_higher) +
                                  as.numeric(popdens) + 
                                  as.numeric(age18to64) + 
                                  as.numeric(age65plus), 
                                scatData.app)

summary.lm(reg.mult.pcticket.app)





