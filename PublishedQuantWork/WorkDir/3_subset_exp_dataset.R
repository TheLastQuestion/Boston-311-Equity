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

