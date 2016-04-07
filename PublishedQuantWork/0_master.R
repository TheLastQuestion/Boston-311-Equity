### MASTER-FILE: Runs full analysis, chart creation etc.
#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
#===================================================

# ----- If none of the files are downloaded yet
# setwd(dirname(file.choose())) # find a file in workdir
# source("1_setup.R", echo = TRUE)
# source("2_missingness.R", echo = TRUE)
# source("3_subset_exp_dataset.R", echo = TRUE)
# source("4_basic_311_charts.R", echo = TRUE)
# source("5_shapefile_prep.R", echo = TRUE)
# source("6_acs_prep.R", echo = TRUE)
# source("7_mapping.R", echo = TRUE)
# source("8_equity_regressions.R", echo = TRUE)



# ----- If files are downloaded, run these to pull data locally
Sys.setenv("plotly_username" = "bryantrenaud")
Sys.setenv("plotly_api_key" = "f8dd9jduxd")
workdir <- dirname(file.choose())
setwd(workdir) # find a file in workdir
source("1_setup_local.R", echo = TRUE)
source("2_missingness.R", echo = TRUE)
source("3_subset_exp_dataset.R", echo = TRUE)
source("4_basic_311_charts.R", echo = TRUE)
source("5_shapefile_prep_local.R", echo = TRUE)
source("6_acs_prep_local.R", echo = TRUE)
source("7_mapping.R", echo = TRUE)
source("8_equity_regressions.R", echo = TRUE)
source("9_overdue_analysis.R", echo = TRUE)


