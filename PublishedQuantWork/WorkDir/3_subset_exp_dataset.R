### EXPERIMENTAL DATASET: Creates analysis dataset
#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================

# ----- subsetting to citizen sources only
yr15.analysis <- subset(yr15,yr15$Source == "Constituent Call" | 
                          yr15$Source == "Citizens Connect App") # 156,073 obs

# ----- subsetting to geocoded tickets
yr15.analysis <- subset(yr15.analysis, 
                        yr15.analysis$Geocoded_Location != "(0, 0)") ## 142,442 obs

# ----- subsetting to tickets with target dates
yr15.analysis <- subset(yr15.analysis, 
                        yr15.analysis$TARGET_DT != "") ## 118,680 obs

# ----- subsetting to tickets with close dates
yr15.analysis <- subset(yr15.analysis,
                        yr15.analysis$CLOSED_DT != "") ## 108,716 obs

# ----- subsetting along same lines for other years
# ----- subsetting to citizen sources only
yr14.analysis <- subset(yr14,yr14$Source == "Constituent Call" | 
                          yr14$Source == "Citizens Connect App") 
# ----- subsetting to geocoded tickets
yr14.analysis <- subset(yr14.analysis, 
                        yr14.analysis$Geocoded_Location != "(0, 0)") 
# ----- subsetting to tickets with target dates
yr14.analysis <- subset(yr14.analysis, 
                        yr14.analysis$TARGET_DT != "") 
# ----- subsetting to tickets with close dates
yr14.analysis <- subset(yr14.analysis,
                        yr14.analysis$CLOSED_DT != "") 
# -----
# -----
# ----- subsetting to citizen sources only
yr13.analysis <- subset(yr13,yr13$Source == "Constituent Call" | 
                          yr13$Source == "Citizens Connect App") 
# ----- subsetting to geocoded tickets
yr13.analysis <- subset(yr13.analysis, 
                        yr13.analysis$Geocoded_Location != "(0, 0)") 
# ----- subsetting to tickets with target dates
yr13.analysis <- subset(yr13.analysis, 
                        yr13.analysis$TARGET_DT != "") 
# ----- subsetting to tickets with close dates
yr13.analysis <- subset(yr13.analysis,
                        yr13.analysis$CLOSED_DT != "") 
#
#
#
# ----- subsetting to citizen sources only
yr12.analysis <- subset(yr12,yr12$Source == "Constituent Call" | 
                          yr12$Source == "Citizens Connect App") 
# ----- subsetting to geocoded tickets
yr12.analysis <- subset(yr12.analysis, 
                        yr12.analysis$Geocoded_Location != "(0, 0)") 
# ----- subsetting to tickets with target dates
yr12.analysis <- subset(yr12.analysis, 
                        yr12.analysis$TARGET_DT != "") 
# ----- subsetting to tickets with close dates
yr12.analysis <- subset(yr12.analysis,
                        yr12.analysis$CLOSED_DT != "") 
#
#
#
# ----- subsetting to citizen sources only
yr11.analysis <- subset(yr11,yr11$Source == "Constituent Call" | 
                          yr11$Source == "Citizens Connect App") 
# ----- subsetting to geocoded tickets
yr11.analysis <- subset(yr11.analysis, 
                        yr11.analysis$Geocoded_Location != "(0, 0)") 
# ----- subsetting to tickets with target dates
yr11.analysis <- subset(yr11.analysis, 
                        yr11.analysis$TARGET_DT != "") 
# ----- subsetting to tickets with close dates
yr11.analysis <- subset(yr11.analysis,
                        yr11.analysis$CLOSED_DT != "") 


# ----- Preparing percent overdue data
## is city's overdue marker accurate?
yr15.analysis$target_clean <- parse_date_time(yr15.analysis$TARGET_DT, 'mdY HMS')
yr15.analysis$closed_clean <- parse_date_time(yr15.analysis$CLOSED_DT, 'mdY HMS')
yr15.analysis$diffs <- yr15.analysis$closed_clean - yr15.analysis$target_clean

yr15.analysis$overdue <- ifelse(yr15.analysis$OnTime_Status == "OVERDUE",1,0)
sum(yr15.analysis$overdue) #39242
yr15.analysis$overdue_clean <- ifelse(yr15.analysis$diffs>=0, 1,0)
sum(yr15.analysis$overdue_clean) #41840

table(yr15.analysis$Department)

overdue.table <- as.data.frame.matrix(table(yr15.analysis$Department, 
                                            yr15.analysis$overdue_clean == 1))

## limit to those departments with over 1k records
overdue.table$Department <- row.names(overdue.table)
overdue.table <- subset(overdue.table, Department == "BTDT" | 
                          Department == "ISD" |
                          Department == "PARK" |
                          Department == "PROP" |
                          Department == "PWDx")
colnames(overdue.table) <- c("On_Time","Overdue","Department")
overdue.table$pct_overdue <- overdue.table$Overdue / (overdue.table$Overdue + 
                                                        overdue.table$On_Time)
