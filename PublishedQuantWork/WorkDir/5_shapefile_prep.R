#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
### SHAPEFILE PREPARATION: Downloading shapefile

# ----- set up work directory
#setwd(dirname(file.choose())) # find a file in workdir

# ----- download massachusetts' tracts shapefile
url<-"http://www2.census.gov/geo/tiger/TIGER2015/TRACT/tl_2015_25_tract.zip"
downloaddir <- paste0(getwd(),"/shapefiles")
destname<-"tiger.zip"
download.file(url, destname)
unzip(destname, exdir=downloaddir, junkpaths=TRUE)

# ----- some cleaning
filename <- "tl_2015_25_tract.shp"
filename <- gsub(".shp", "", filename)

# ----- Read in shapefile (NAD83 coordinate system)
# ----- this is a fairly big shapefile and takes 1 minute to read
tract <- readOGR(downloaddir, filename) 
tract <- spTransform(x=tract, CRSobj=CRS("+proj=longlat +datum=WGS84"))

# ----- Create a subset of Suffolk county
tract <- subset(tract, tract$COUNTYFP == "025")

# ----- Lowering cases
names(tract@data) <- tolower(names(tract@data))

# ----- Convert 311 data to a spatial points object
vc <- yr15.analysis
vc <- SpatialPointsDataFrame(coords=vc[, c("LONGITUDE", "LATITUDE")],
                             data=vc[, c("OPEN_DT","CASE_STATUS", "SUBJECT", "REASON","TYPE","Source")],
                             proj4string=CRS("+proj=longlat +datum=WGS84"))

# ----- Distinguishing calls from app
vc.constit <- subset(vc,Source == "Constituent Call")
vc.app <- subset(vc,Source == "Citizens Connect App")

# ----- Each row entry represents one 311 request, so, add count column
vc@data$count <- 1
vc.constit@data$count <- 1
vc.app@data$count <- 1

# ----- Spatial overlay to identify census polygon in which each 311 request point falls
# ----- The Result `vc_tract` is a dataframe with the tract data for each point
vc_tract <- over(x=vc, y=tract)
vc_tract_constit <- over(x=vc.constit, y=tract)
vc_tract_app <- over(x=vc.app, y=tract)

# ----- Add tract data to Points
vc@data <- data.frame(vc@data, vc_tract)
vc.constit@data <- data.frame(vc.constit@data, vc_tract_constit)
vc.app@data <- data.frame(vc.app@data, vc_tract_app)

# ----- Aggregate 311 tickets by tract 
req_tract <- aggregate(formula=count~tractce, data=vc@data, FUN=length)
req_tract_constit <- aggregate(formula=count~tractce, data=vc.constit@data, FUN=length)
req_tract_app <- aggregate(formula=count~tractce, data=vc.app@data, FUN=length)

# ----- Looking at tickets by tract
ticketsByTract <- req_tract
colnames(ticketsByTract) <- c("id","count")

# ----- sort
ticketsByTract <- ticketsByTract[order(-ticketsByTract$count),]

# ----- Write to csv
#write.csv(ticketsByTract, file = "TicketsByTract.csv", row.names = FALSE)

ticketsByTract.constit <- req_tract_constit
colnames(ticketsByTract.constit) <- c("id","count")
ticketsByTract.app <- req_tract_app
colnames(ticketsByTract.app) <- c("id","count")

# ----- Add number of 311 records to tracts object
m <- match(x=tract@data$tractce, table=req_tract$tractce)
tract@data$reqs <- req_tract$count[m]

# ----- reading shapefile in and storing as SpatialPolygonsDataFrame object
bos.shape <- readOGR(dsn = paste0(getwd(),"/shapefiles"), layer = "tl_2015_25_tract")
# ----- Create a subset of Suffolk county
bos.shape <- subset(bos.shape, bos.shape$COUNTYFP == "025")

# ----- dropping tracts not covered by 311 from future maps
bos.shape <- bos.shape[!grepl("980101", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("990101", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("981300", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("180500", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("180400", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("180301", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("180200", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("180101", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170800", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170702", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170701", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170601", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170502", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170501", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170400", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170300", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170200", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("170100", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("160602", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("160601", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("160502", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("160501", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("160400", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("160300", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("160200", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("160101", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("981600", bos.shape$TRACTCE),]
bos.shape <- bos.shape[!grepl("981502", bos.shape$TRACTCE),]

# ----- transform data into df that ggplot can understand
# ----- use "names(bos.shape)" to find options for region
bos.df <- fortify(bos.shape, region="TRACTCE")
colnames(tract@data) <- c("state","county","id","blkgrp",
                          "geoid10","name","mtfcc","funcstat","aland","awater","lat","long","reqs")
# ----- joining map + count data
plotData <- merge(bos.df, tract@data,by="id")

# ----- fetching neighborhoods shapefile
url2<-"http://www.zillow.com/static/shp/ZillowNeighborhoods-MA.zip"
downloaddir2 <- paste0(getwd(),"/shapefiles/neighborhoods")
destname2<-"ZillowNeighborhoods-MA.zip"
download.file(url2, destname2)
unzip(destname2, exdir=downloaddir2, junkpaths=TRUE)


neighbs <- paste0(getwd(),"/shapefiles/neighborhoods/ZillowNeighborhoods-MA")
bos_neighbs_raw <- readOGR(dsn = neighbs, layer = "ZillowNeighborhoods-MA")
bos_neighbs <- subset(bos_neighbs_raw, bos_neighbs_raw$CITY == "Boston")
bos_neighbs <- fortify(bos_neighbs, region="NAME")

