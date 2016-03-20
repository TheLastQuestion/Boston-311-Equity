### ACS DATA PREP: downloading, cleaning, merging acs data
#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================

# ----- set up work directory
#setwd(dirname(file.choose())) # find a file in workdir


### setting work dir
bryant <- "C:/Dropbox/On Device/!HKS_Spring_16/PAE2/QuantWork/"
nick <- "C:/Users/Nick Ryan/Dropbox/PAE2/QuantWork/"

#################################################################
workdir <- bryant
#################################################################

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


## reading in ACS student data
acs.stud<- read.csv(paste0(workdir,"ACS_csvs/ACS_13_5YR_B14001_with_ann_stud.csv"),
                    stringsAsFactors = FALSE)

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
acs.stud$id <- substr(acs.stud$GEO.id2,6,11)


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
## students
acs.stud.2 <- acs.stud[,c("id","HD01_VD08","HD01_VD09")]
colnames(acs.stud.2) <- c("id", "enrld_undergrad","enrld_grad")
acs.pop.2$id <- as.character(acs.pop.2$id)


## joining ACS datasets
acs.full <- left_join(acs.race.2, acs.pov.2)
acs.full <- left_join(acs.full,acs.meaninc.2)
acs.full <- left_join(acs.full,acs.medinc.2)
acs.full <- left_join(acs.full,acs.emp.2)
acs.full <- left_join(acs.full,acs.educ.2)
acs.full <- left_join(acs.full,acs.hisp.2)
acs.full <- left_join(acs.full,acs.pop.2)
acs.full <- left_join(acs.full,acs.land.2)
acs.full <- left_join(acs.full,acs.stud.2)

## looking at population density
acs.full$popdens <- as.numeric(as.character(acs.full$pop)) / 
  as.numeric(as.character(acs.full$sqmi))

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

## looking at % college/university undergrad enrolled
acs.full$pct_undergrad <- as.numeric(as.character(acs.full$enrld_undergrad)) / 
  as.numeric(as.character(acs.full$pop))
acs.full$pct_undergrad <- acs.full$pct_undergrad * 100

## looking at % college/university undergrad enrolled
acs.full$pct_grad <- as.numeric(as.character(acs.full$enrld_grad)) / 
  as.numeric(as.character(acs.full$pop))
acs.full$pct_grad <- acs.full$pct_grad * 100

## joining map + acs data
plotData <- merge(plotData, acs.full,by="id")
dataByTract <- merge(ticketsByTract, acs.full,by="id")


# ----- creating per capita 311 tickets variable in plot data
plotData$pc_count <- as.numeric(as.character(plotData$reqs)) /
  as.numeric(as.character(plotData$pop))

# ----- per capita 311 tickets regression data
dataByTract$pc_count <- as.numeric(dataByTract$count)/as.numeric(dataByTract$pop)

# ----- creating df with neighborhood labels & cleaning
label_points <- coordinates(bos_neighbs_raw)
neighb_labels <- merge(label_points,bos_neighbs_raw["NAME"],by="row.names",all.x=TRUE)

neighb_labels$V1 <- ifelse(neighb_labels$NAME == "South End",-71.075,neighb_labels$V1)
neighb_labels$V2 <- ifelse(neighb_labels$NAME == "South End",42.34,neighb_labels$V2)
neighb_labels$V2 <- ifelse(neighb_labels$NAME == "Central",neighb_labels$V2 + .004,neighb_labels$V2)
neighb_labels$V1 <- ifelse(neighb_labels$NAME == "Fenway-Kenmore",neighb_labels$V1-.006,neighb_labels$V1)

neighb_labels <- subset(neighb_labels, neighb_labels$NAME == "East Boston" |
                          neighb_labels$NAME == "South End" |
                          neighb_labels$NAME == "Central" |
                          neighb_labels$NAME == "Charlestown" |
                          neighb_labels$NAME == "Allston-Brighton" |
                          neighb_labels$NAME == "Back Bay-Beacon Hill" |
                          neighb_labels$NAME == "Fenway-Kenmore" |
                          neighb_labels$NAME == "South Boston" |
                          neighb_labels$NAME == "Jamaica Plain" |
                          neighb_labels$NAME == "Roxbury" |
                          neighb_labels$NAME == "North Dorchester" |
                          neighb_labels$NAME == "West Roxbury" |
                          neighb_labels$NAME == "Roslindale" |
                          neighb_labels$NAME == "Mattapan" |
                          neighb_labels$NAME == "South Dorchester" |
                          neighb_labels$NAME == "Hyde Park")
neighb_labels$NAME2 <- ifelse(neighb_labels$NAME == "Allston-Brighton",
                              "Allston- \n Brighton",
                              ifelse(neighb_labels$NAME == "South End",
                                     "S. End",
                                     ifelse(neighb_labels$NAME == "Back Bay-Beacon Hill",
                                            "Back Bay- \n Beacon Hill",
                                            ifelse(neighb_labels$NAME == "Fenway-Kenmore",
                                                   "Fenway- \n Kenmore",
                                                   ifelse(neighb_labels$NAME == "South Boston",
                                                          "South \n Boston",
                                                          ifelse(neighb_labels$NAME == "Jamaica Plain",
                                                                 "Jamaica \n Plain",
                                                                 ifelse(neighb_labels$NAME == "North Dorchester",
                                                                        "North \n Dorchester",
                                                                        ifelse(neighb_labels$NAME == "West Roxbury",
                                                                               "West \n Roxbury",
                                                                               ifelse(neighb_labels$NAME == "South Dorchester",
                                                                                      "South \n Dorchester",
                                                                                      ifelse(neighb_labels$NAME == "Hyde Park",
                                                                                             "Hyde \n Park",
                                                                                             as.character(neighb_labels$NAME)))))))))))

# ----- setting tracts with bad data to NA
plotData$pc_count <- ifelse(plotData$id=="981000" |
                              plotData$id=="981201" |
                              plotData$id=="981501" |
                              plotData$id=="981502" |
                              plotData$id=="981600" |
                              plotData$id=="981700" |
                              plotData$id=="980700" |
                              plotData$id=="981800" |
                              plotData$id=="980700" |
                              plotData$id=="980300" |
                              plotData$id=="981100" |
                              plotData$id=="981202",NA,plotData$pc_count)
dataByTract$pc_count <- ifelse(dataByTract$id=="981000" |
                                 dataByTract$id=="981201" |
                                 dataByTract$id=="981501" |
                                 dataByTract$id=="981502" |
                                 dataByTract$id=="981600" |
                                 dataByTract$id=="981700" |
                                 dataByTract$id=="980700" |
                                 dataByTract$id=="981800" |
                                 dataByTract$id=="980700" |
                                 dataByTract$id=="980300" |
                                 dataByTract$id=="981100" |
                                 dataByTract$id=="981202",NA,dataByTract$pc_count)

plotData$pct_white <- ifelse(plotData$id=="981000" |
                                 plotData$id=="981201" |
                                 plotData$id=="981501" |
                                 plotData$id=="981502" |
                                 plotData$id=="981600" |
                                 plotData$id=="981700" |
                                 plotData$id=="980700" |
                                 plotData$id=="981800" |
                                 plotData$id=="980700" |
                                 plotData$id=="980300" |
                               plotData$id=="981100" |
                                 plotData$id=="981202",NA,plotData$pct_white)
dataByTract$pct_white <- ifelse(dataByTract$id=="981000" |
                                  dataByTract$id=="981201" |
                                  dataByTract$id=="981501" |
                                  dataByTract$id=="981502" |
                                  dataByTract$id=="981600" |
                                  dataByTract$id=="981700" |
                                  dataByTract$id=="980700" |
                                  dataByTract$id=="981800" |
                                  dataByTract$id=="980700" |
                                  dataByTract$id=="980300" |
                                  dataByTract$id=="981100" |
                                  dataByTract$id=="981202",NA,dataByTract$pct_white)

plotData$Med_Inc <- ifelse(plotData$id=="981000" |
                               plotData$id=="981201" |
                               plotData$id=="981501" |
                               plotData$id=="981502" |
                               plotData$id=="981600" |
                               plotData$id=="981700" |
                               plotData$id=="980700" |
                               plotData$id=="981800" |
                               plotData$id=="980700" |
                               plotData$id=="980300" |
                             plotData$id=="981100" |
                               plotData$id=="981202",NA,plotData$Med_Inc)
dataByTract$Med_Inc <- ifelse(dataByTract$id=="981000" |
                                  dataByTract$id=="981201" |
                                  dataByTract$id=="981501" |
                                  dataByTract$id=="981502" |
                                  dataByTract$id=="981600" |
                                  dataByTract$id=="981700" |
                                  dataByTract$id=="980700" |
                                  dataByTract$id=="981800" |
                                  dataByTract$id=="980700" |
                                  dataByTract$id=="980300" |
                                dataByTract$id=="981100" |
                                  dataByTract$id=="981202",NA,dataByTract$Med_Inc)

plotData$pct_hisp_latin <- ifelse(plotData$id=="981000" |
                             plotData$id=="981201" |
                             plotData$id=="981501" |
                             plotData$id=="981502" |
                             plotData$id=="981600" |
                             plotData$id=="981700" |
                             plotData$id=="980700" |
                             plotData$id=="981800" |
                             plotData$id=="980700" |
                             plotData$id=="980300" |
                               plotData$id=="981100" |
                             plotData$id=="981202",NA,plotData$pct_hisp_latin)
dataByTract$pct_hisp_latin <- ifelse(dataByTract$id=="981000" |
                                dataByTract$id=="981201" |
                                dataByTract$id=="981501" |
                                dataByTract$id=="981502" |
                                dataByTract$id=="981600" |
                                dataByTract$id=="981700" |
                                dataByTract$id=="980700" |
                                dataByTract$id=="981800" |
                                dataByTract$id=="980700" |
                                dataByTract$id=="980300" |
                                  dataByTract$id=="981100" |
                                dataByTract$id=="981202",NA,dataByTract$pct_hisp_latin)

plotData$pct_black <- ifelse(plotData$id=="981000" |
                             plotData$id=="981201" |
                             plotData$id=="981501" |
                             plotData$id=="981502" |
                             plotData$id=="981600" |
                             plotData$id=="981700" |
                             plotData$id=="980700" |
                             plotData$id=="981800" |
                             plotData$id=="980700" |
                             plotData$id=="980300" |
                               plotData$id=="981100" |
                             plotData$id=="981202",NA,plotData$pct_black)
dataByTract$pct_black <- ifelse(dataByTract$id=="981000" |
                                dataByTract$id=="981201" |
                                dataByTract$id=="981501" |
                                dataByTract$id=="981502" |
                                dataByTract$id=="981600" |
                                dataByTract$id=="981700" |
                                dataByTract$id=="980700" |
                                dataByTract$id=="981800" |
                                dataByTract$id=="980700" |
                                dataByTract$id=="980300" |
                                  dataByTract$id=="981100" |
                                dataByTract$id=="981202",NA,dataByTract$pct_black)

# ----- combining enrolment variables and cleaning
plotData$pct_enrol <- plotData$pct_grad + plotData$pct_undergrad
dataByTract$pct_enrol <- dataByTract$pct_grad + dataByTract$pct_undergrad
plotData$pct_enrol <- ifelse(plotData$id=="981000" |
                               plotData$id=="981201" |
                               plotData$id=="981501" |
                               plotData$id=="981502" |
                               plotData$id=="981600" |
                               plotData$id=="981700" |
                               plotData$id=="980700" |
                               plotData$id=="981800" |
                               plotData$id=="980700" |
                               plotData$id=="980300" |
                               plotData$id=="981100" |
                               plotData$id=="981202",NA,plotData$pct_enrol)
dataByTract$pct_enrol <- ifelse(dataByTract$id=="981000" |
                                  dataByTract$id=="981201" |
                                  dataByTract$id=="981501" |
                                  dataByTract$id=="981502" |
                                  dataByTract$id=="981600" |
                                  dataByTract$id=="981700" |
                                  dataByTract$id=="980700" |
                                  dataByTract$id=="981800" |
                                  dataByTract$id=="980700" |
                                  dataByTract$id=="980300" |
                                  dataByTract$id=="981100" |
                                  dataByTract$id=="981202",NA,dataByTract$pct_enrol)
