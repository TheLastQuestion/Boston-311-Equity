#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
### SHAPEFILE PREPARATION: Downloading shapefile

# ----- refer to massachusetts' tracts shapefile
downloaddir <- paste0(workdir,"/shapefiles")
destname<-"tiger.zip"

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
                             data=vc[, c("OPEN_DT","CASE_STATUS", "SUBJECT", "REASON","TYPE","Source","overdue","diffs")],
                             proj4string=CRS("+proj=longlat +datum=WGS84"))
vc14 <- yr14.analysis
vc14 <- SpatialPointsDataFrame(coords=vc14[, c("LONGITUDE", "LATITUDE")],
                             data=vc14[, c("OPEN_DT","CASE_STATUS", "SUBJECT", "REASON","TYPE","Source")],
                             proj4string=CRS("+proj=longlat +datum=WGS84"))
vc13 <- yr13.analysis
vc13 <- SpatialPointsDataFrame(coords=vc13[, c("LONGITUDE", "LATITUDE")],
                             data=vc13[, c("OPEN_DT","CASE_STATUS", "SUBJECT", "REASON","TYPE","Source")],
                             proj4string=CRS("+proj=longlat +datum=WGS84"))
vc12 <- yr12.analysis
vc12 <- SpatialPointsDataFrame(coords=vc12[, c("LONGITUDE", "LATITUDE")],
                             data=vc12[, c("OPEN_DT","CASE_STATUS", "SUBJECT", "REASON","TYPE","Source")],
                             proj4string=CRS("+proj=longlat +datum=WGS84"))
vc11 <- yr11.analysis
vc11 <- SpatialPointsDataFrame(coords=vc11[, c("LONGITUDE", "LATITUDE")],
                             data=vc11[, c("OPEN_DT","CASE_STATUS", "SUBJECT", "REASON","TYPE","Source")],
                             proj4string=CRS("+proj=longlat +datum=WGS84"))

# ----- Distinguishing calls from app
vc.constit <- subset(vc,Source == "Constituent Call")
vc.app <- subset(vc,Source == "Citizens Connect App")

# ----- Each row entry represents one 311 request, so, add count column
vc@data$count <- 1
vc.constit@data$count <- 1
vc.app@data$count <- 1

vc14@data$count <- 1
vc13@data$count <- 1
vc12@data$count <- 1
vc11@data$count <- 1

# ----- Spatial overlay to identify census polygon in which each 311 request point falls
# ----- The Result `vc_tract` is a dataframe with the tract data for each point
vc_tract <- over(x=vc, y=tract)
vc_tract_constit <- over(x=vc.constit, y=tract)
vc_tract_app <- over(x=vc.app, y=tract)

vc14_tract <- over(x=vc14, y=tract)
vc13_tract <- over(x=vc13, y=tract)
vc12_tract <- over(x=vc12, y=tract)
vc11_tract <- over(x=vc11, y=tract)


# ----- Add tract data to Points
vc@data <- data.frame(vc@data, vc_tract)
vc.constit@data <- data.frame(vc.constit@data, vc_tract_constit)
vc.app@data <- data.frame(vc.app@data, vc_tract_app)

vc14@data <- data.frame(vc14@data, vc14_tract)
vc13@data <- data.frame(vc13@data, vc13_tract)
vc12@data <- data.frame(vc12@data, vc12_tract)
vc11@data <- data.frame(vc11@data, vc11_tract)

# ----- Aggregate 311 tickets by tract 
req_tract <- aggregate(formula=count~tractce, data=vc@data, FUN=length)
req_tract_constit <- aggregate(formula=count~tractce, data=vc.constit@data, FUN=length)
req_tract_app <- aggregate(formula=count~tractce, data=vc.app@data, FUN=length)

req_tract14 <- aggregate(formula=count~tractce, data=vc14@data, FUN=length)
req_tract13 <- aggregate(formula=count~tractce, data=vc13@data, FUN=length)
req_tract12 <- aggregate(formula=count~tractce, data=vc12@data, FUN=length)
req_tract11 <- aggregate(formula=count~tractce, data=vc11@data, FUN=length)

# ----- agg overdue by tract
overdue_tract <- as.data.frame.matrix(table(vc@data$tractce, 
                                            vc@data$overdue == 1))
overdue_tract$id <- row.names(overdue_tract)
colnames(overdue_tract) <- c("on_time","overdue","id")
overdue_tract$rowsum <- overdue_tract$on_time + overdue_tract$overdue
overdue_tract$pct_overdue <- overdue_tract$overdue / overdue_tract$rowsum

# ----- Looking at tickets by tract
ticketsByTract <- req_tract
colnames(ticketsByTract) <- c("id","count")
ticketsByTract14 <- req_tract14
colnames(ticketsByTract14) <- c("id","count14")
ticketsByTract13 <- req_tract13
colnames(ticketsByTract13) <- c("id","count13")
ticketsByTract12 <- req_tract12
colnames(ticketsByTract12) <- c("id","count12")
ticketsByTract11 <- req_tract11
colnames(ticketsByTract11) <- c("id","count11")


# ----- sort
ticketsByTract <- ticketsByTract[order(-ticketsByTract$count),]

# ----- Write to csv
#write.csv(ticketsByTract, file = "TicketsByTract.csv", row.names = FALSE)

ticketsByTract.constit <- req_tract_constit
colnames(ticketsByTract.constit) <- c("id","count_constit")
ticketsByTract.app <- req_tract_app
colnames(ticketsByTract.app) <- c("id","count_app")

# ----- Add number of 311 records to tracts object
m <- match(x=tract@data$tractce, table=req_tract$tractce)
tract@data$reqs <- req_tract$count[m]

m14 <- match(x=tract@data$tractce, table=req_tract14$tractce)
tract@data$reqs14 <- req_tract14$count[m14]

m13 <- match(x=tract@data$tractce, table=req_tract13$tractce)
tract@data$reqs13 <- req_tract13$count[m13]

m12 <- match(x=tract@data$tractce, table=req_tract12$tractce)
tract@data$reqs12 <- req_tract12$count[m12]

m11 <- match(x=tract@data$tractce, table=req_tract11$tractce)
tract@data$reqs11 <- req_tract11$count[m11]

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
# ----- joining map + count data + overdue data
plotData <- merge(bos.df, tract@data,by="id")
plotData <- merge(plotData, overdue_tract, by="id")

# ----- refer to neighborhoods shapefile
# neighbs <- paste0(getwd(),"/shapefiles/neighborhoods/Bos_neighborhoods_new")
# bos_neighbs <- readOGR(dsn = neighbs, layer = "Bos_neighborhoods_new")
# bos_neighbs <- fortify(bos_neighbs, region="Name")

neighbs <- paste0(getwd(),"/shapefiles/neighborhoods/ZillowNeighborhoods-MA")
bos_neighbs_raw <- readOGR(dsn = neighbs, layer = "ZillowNeighborhoods-MA")
bos_neighbs <- subset(bos_neighbs_raw, bos_neighbs_raw$CITY == "Boston")
bos_neighbs <- fortify(bos_neighbs, region="NAME")


