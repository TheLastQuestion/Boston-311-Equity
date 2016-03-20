### ACS DATA PREP: downloading, cleaning, merging acs data
#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================

# ----- get an api key emailed to you
# http://www.census.gov/developers/tos/key_request.html
# ----- run the following once
# api.key.install(key="592bc14cnotarealkey686552b17fda3c89dd389")

# ----- set up work directory
#setwd(dirname(file.choose())) # find a file in workdir

# # ----- download ACS race
# race <- acs.fetch(geography = geo.make(state = "MA", county = "Suffolk", tract = "*"), 
#           table.number = "B02001")
# race <- data.frame(estimate(race))
# race$id <- row.names(race)
# 
# race.2 <- race[,c(11,2,3)]
# colnames(race.2) <- c("id", "white","black")
# race.2$id <- as.character(race.2$id)
# 
# # ----- download ACS poverty
# poverty <- acs.fetch(geography = geo.make(state = "MA", county = "Suffolk",
#                                           tract = "*"), table.number = "S1701")
# poverty <- data.frame(estimate(poverty))
# poverty$id <- row.names(poverty)
# 
# pov.2 <-  pov[,c(11, "HC03_EST_VC01","HC01_EST_VC03","HC01_EST_VC05","HC01_EST_VC06")]
# colnames( pov.2) <- c("id", "Pct_Below_Pov_lev","under_18","age18to64","age65plus")
# pov.2$id <- as.character( pov.2$id)
# 
# # ----- download Med Inc poverty
# medinc <- acs.fetch(geography = geo.make(state = "MA", county = "Suffolk", tract = "*"), 
#                   table.number = "S1903")
# medinc <- data.frame(estimate(medinc))
# medinc$id <- row.names(medinc)
# 
# # ----- download ACS employment
# emp <- acs.fetch(geography = geo.make(state = "MA", county = "Suffolk", tract = "*"), 
#                   table.number = "S2301")
# emp <- data.frame(estimate(emp))
# emp$id <- row.names(emp)
# 
# # ----- download ACS education
# educ <- acs.fetch(geography = geo.make(state = "MA", county = "Suffolk", tract = "*"), 
#                  table.number = "S1501")
# educ <- data.frame(estimate(educ))
# educ$id <- row.names(educ)
# 
# # ----- download ACS hispanic/latino
# hisp <- acs.fetch(geography = geo.make(state = "MA", county = "Suffolk", tract = "*"), 
#                   table.number = "B03001")
# hisp <- data.frame(estimate(hisp))
# hisp$id <- row.names(hisp)
# 
# # ----- download ACS population
# pop <- acs.fetch(geography = geo.make(state = "MA", county = "Suffolk", tract = "*"), 
#                   table.number = "B01003")
# pop <- data.frame(estimate(pop))
# pop$id <- row.names(pop)
# 
# # ----- download ACS enrollment
# enrol <- acs.fetch(geography = geo.make(state = "MA", county = "Suffolk", tract = "*"), 
#                  table.number = "B14001")
# enrol <- data.frame(estimate(enrol))
# enrol$id <- row.names(enrol)
# 
# 
# 
# 
# 
# ## meaninc
# acs.meaninc.2 <- acs.meaninc[,c("id", "HC02_EST_VC02")]
# colnames(acs.meaninc.2) <- c("id", "Mean_Inc")
# acs.meaninc.2$id <- as.character(acs.meaninc.2$id)
# ## medinc
# acs.medinc.2 <- acs.medinc[,c("id", "HC02_EST_VC02")]
# colnames(acs.medinc.2) <- c("id", "Med_Inc")
# acs.medinc.2$id <- as.character(acs.medinc.2$id)
# acs.medinc.2$Med_Inc <- as.numeric(as.character(acs.medinc.2$Med_Inc))
# ## emp
# acs.emp.2 <- acs.emp[,c("id", "HC04_EST_VC01")]
# colnames(acs.emp.2) <- c("id", "Unemp")
# acs.emp.2$id <- as.character(acs.emp.2$id)
# ## educ
# acs.educ.2 <- acs.educ[,c("id","HC01_EST_VC02","HC01_EST_VC03",
#                           "HC01_EST_VC04","HC01_EST_VC05")]
# colnames(acs.educ.2) <- c("id","less_than_hs","hs_grad","some_college","bach_or_higher")
# acs.educ.2$id <- as.character(acs.educ.2$id)
# ## hisp
# acs.hisp.2 <- acs.hisp[,c("id", "HD01_VD03")]
# colnames(acs.hisp.2) <- c("id", "Hisp_or_Latin")
# acs.hisp.2$id <- as.character(acs.hisp.2$id)
# ## pop
# acs.pop.2 <- acs.pop[,c("id", "HD01_VD01")]
# colnames(acs.pop.2) <- c("id", "pop")
# acs.pop.2$id <- as.character(acs.pop.2$id)
# ## land area
# acs.land.2 <- acs.land[,c("id","ALAND_SQMI")]
# colnames(acs.land.2) <- c("id","sqmi")
# acs.land.2$id <- as.character(acs.land.2$id)
# ## students
# acs.stud.2 <- acs.stud[,c("id","HD01_VD08","HD01_VD09")]
# colnames(acs.stud.2) <- c("id", "enrld_undergrad","enrld_grad")
# acs.pop.2$id <- as.character(acs.pop.2$id)
# 
# 
# ## joining ACS datasets
# acs.full <- left_join(acs.race.2, acs.pov.2)
# acs.full <- left_join(acs.full,acs.meaninc.2)
# acs.full <- left_join(acs.full,acs.medinc.2)
# acs.full <- left_join(acs.full,acs.emp.2)
# acs.full <- left_join(acs.full,acs.educ.2)
# acs.full <- left_join(acs.full,acs.hisp.2)
# acs.full <- left_join(acs.full,acs.pop.2)
# acs.full <- left_join(acs.full,acs.land.2)
# acs.full <- left_join(acs.full,acs.stud.2)
# 
# ## looking at population density
# acs.full$popdens <- as.numeric(as.character(acs.full$pop)) / 
#   as.numeric(as.character(acs.full$sqmi))
# 
# ## looking at % white 
# acs.full$pct_white <- as.numeric(as.character(acs.full$white)) / 
#   as.numeric(as.character(acs.full$pop))
# acs.full$pct_white <- acs.full$pct_white * 100
# 
# ## looking at % black 
# acs.full$pct_black <- as.numeric(as.character(acs.full$black)) / 
#   as.numeric(as.character(acs.full$pop))
# acs.full$pct_black <- acs.full$pct_black * 100
# 
# ## looking at % hispanic or latino in origin 
# acs.full$pct_hisp_latin <- as.numeric(as.character(acs.full$Hisp_or_Latin)) / 
#   as.numeric(as.character(acs.full$pop))
# acs.full$pct_hisp_latin <- acs.full$pct_hisp_latin * 100
# 
# ## looking at % college/university undergrad enrolled
# acs.full$pct_undergrad <- as.numeric(as.character(acs.full$enrld_undergrad)) / 
#   as.numeric(as.character(acs.full$pop))
# acs.full$pct_undergrad <- acs.full$pct_undergrad * 100
# 
# ## looking at % college/university undergrad enrolled
# acs.full$pct_grad <- as.numeric(as.character(acs.full$enrld_grad)) / 
#   as.numeric(as.character(acs.full$pop))
# acs.full$pct_grad <- acs.full$pct_grad * 100
# 
# ## joining map + acs data
# plotData <- merge(plotData, acs.full,by="id")
































##################################
##################################################
### bringing in acs data ####
####################################################
##################################

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