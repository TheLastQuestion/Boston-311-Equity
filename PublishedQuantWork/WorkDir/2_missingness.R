### MISSINGNESS: Analyzes missingness in 2015 data
#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================

# ----- how many records have 0,0 geocode
sum(yr15$Geocoded_Location == "(0, 0)") ## 18,866

# ----- confirming that these are artifically set to city hall geo
sum(yr15$Geocoded_Location == "(0, 0)" & 
      yr15$LATITUDE == 42.3594 & 
      yr15$LONGITUDE == -71.0587) ## 18,866

# ----- confirming that missing streetnames belong to this category
sum(yr15$LOCATION_STREET_NAME == "") ## 11,293
sum(yr15$LOCATION_STREET_NAME == "" & yr15$Geocoded_Location == "(0, 0)")  ## 11,293

# ----- if street name not missing, but geo is missing
sum(yr15$LOCATION_STREET_NAME != "") ## 201,916
sum(yr15$LOCATION_STREET_NAME != "" & yr15$Geocoded_Location == "(0, 0)") ## 7,573

# ----- do all records have open date? yes
nrow(yr15) == sum(yr15$OPEN_DT != "") 

# ----- missing for target and closed
sum(yr15$TARGET_DT == "") ## 40,252 records blank for target
sum(yr15$CLOSED_DT == "") ## 20,060 records blank for closed

# ----- overlapping missing target and closed
sum(yr15$CLOSED_DT == "" & yr15$TARGET_DT != "") ## 14,841 records
sum(yr15$CLOSED_DT != "" & yr15$TARGET_DT == "") ## 35,033 records
sum(yr15$CLOSED_DT == "" & yr15$TARGET_DT == "") ## 5,219
sum(yr15$CLOSED_DT != "" & yr15$TARGET_DT != "") ## 158,116

yr15$count <- 1

##########3
# for each ticket type, how many records are
# 1) missing both target and close, 2) just missing close
# 3) just missing target, 4) has both

yr15.missing.both <- subset(yr15,yr15$CLOSED_DT == "" & yr15$TARGET_DT == "",select= c(TYPE, count))
yr15.missing.close <- subset(yr15,yr15$CLOSED_DT == "" & yr15$TARGET_DT != "",select= c(TYPE, count))
yr15.missing.target <- subset(yr15, yr15$CLOSED_DT != "" & yr15$TARGET_DT == "",select= c(TYPE, count))
yr15.has.both <- subset(yr15, yr15$CLOSED_DT != "" & yr15$TARGET_DT != "",select= c(TYPE, count))

yr15.missing.both.table <- data.frame(table(yr15.missing.both$TYPE))
yr15.missing.close.table <- data.frame(table(yr15.missing.close$TYPE))
yr15.missing.target.table <- data.frame(table(yr15.missing.target$TYPE))
yr15.has.both.table <- data.frame(table(yr15.has.both$TYPE))

yr15.missing.full <- merge(yr15.missing.both.table,yr15.missing.close.table, by = "Var1")
yr15.missing.full <- merge(yr15.missing.full,yr15.missing.target.table, by = "Var1")
yr15.missing.full <- merge(yr15.missing.full,yr15.has.both.table, by = "Var1")

colnames(yr15.missing.full) <- c("type","no_both","no_close","no_target","has_both")

yr15.missing.full$rowsum <- yr15.missing.full[,2] + yr15.missing.full[,3] + yr15.missing.full[,4] + yr15.missing.full[,5] 

# ----- sort
yr15.missing.full.sort <- yr15.missing.full[order(-yr15.missing.full$has_both),]

# View(yr15.missing.full)
write.csv(yr15.missing.full.sort, file = "Missingness_by_type.csv", row.names = FALSE)
