### 311 DESCRIPTIVE CHARTS: Creates charts describing 311 data
#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================

# ----- move into basic charts directory
setwd(paste0(workdir,"/Basic_Charts"))

# ----- HISTOGRAM: tickets by year
#===================================================
hist.year.df <- subset(tickets,open_yr<=2015)
hist.year.df$open_yr <- as.character(hist.year.df$open_yr)
hist.year.df$open_yr <- ifelse(hist.year.df$open_yr == 2011,
                               "-2011-",hist.year.df$open_yr)
hist.year.df$open_yr <- ifelse(hist.year.df$open_yr == 2012,
                               "-2012-",hist.year.df$open_yr)
hist.year.df$open_yr <- ifelse(hist.year.df$open_yr == 2013,
                               "-2013-",hist.year.df$open_yr)
hist.year.df$open_yr <- ifelse(hist.year.df$open_yr == 2014,
                               "-2014-",hist.year.df$open_yr)
hist.year.df$open_yr <- ifelse(hist.year.df$open_yr == 2015,
                               "-2015-",hist.year.df$open_yr)
hist.year <- plot_ly(x = hist.year.df$open_yr,type = "histogram")
hist.year <- layout(hist.year, title = "Tickets by Year",
                    xaxis = list(range = c("2011","2012","2013","2014","2015"),title="Year"),
                    yaxis = list(title="Num of Tickets"))
hist.year
plotly_IMAGE(hist.year, width = 500, height = 500, format = "png", scale = 2,
             out_file = "Tickets_by_year.png")
# plotly_POST(hist.year, filename="tickets_by_year")

# ----- HISTOGRAM: tickets over the course of 2015
#===================================================
hist.day <- plot_ly(x = yr15$date2,type = "histogram")
hist.day <- layout(hist.day, title = "Tickets by Day, 2015",
                    xaxis = list(title="Date"),
                    yaxis = list(title="Num of Tickets"))
hist.day
plotly_IMAGE(hist.day, width = 500, height = 500, format = "png", scale = 2,
             out_file = "Tickets_by_day.png")

# ----- HISTOGRAM: tickets over the course of a week
#===================================================
hist.week <- plot_ly(x = yr15$open_dow,type = "histogram")
hist.week <- layout(hist.week, title = "Tickets by Day of the Week",
                   xaxis = list(range = c("Sunday","Monday","Tuesday",
                                          "Wednesday","Thursday","Friday")
                                ,title="Day of the Week"),
                   yaxis = list(title="Num of Tickets"))
hist.week
plotly_IMAGE(hist.week, width = 500, height = 500, format = "png", scale = 2,
             out_file = "Tickets_by_week.png")
# plotly_POST(hist.week, filename="tickets_by_week")

# ----- HISTOGRAM: tickets over the course of a day
#===================================================
hist.hour <- plot_ly(x = tickets$open_hour,type = "histogram")
hist.hour <- layout(hist.hour, title = "Tickets by Hour of Day",
                    xaxis = list(title="Hour of Day"),
                    yaxis = list(title="Num of Tickets"))
hist.hour
plotly_IMAGE(hist.hour, width = 500, height = 500, format = "png", scale = 2,
             out_file = "Tickets_by_hour.png")

# ----- BAR: tickets by type over 2015
#===================================================
yr15.snow <- subset(yr15, yr15$TYPE == "Request for Snow Plowing")
yr15.trash <- subset(yr15, yr15$TYPE == "Missed Trash/Recycling/Yard Waste/Bulk Item")
yr15.street <- subset(yr15, yr15$TYPE == "Requests for Street Cleaning")
yr15.parking <- subset(yr15, yr15$TYPE == "Parking Enforcement")
yr15.potholes <- subset(yr15, yr15$TYPE == "Request for Pothole Repair")
yr15.lights <- subset(yr15, yr15$TYPE == "Street Light Outages")

hist.type <- plot_ly(x = yr15.lights$open_mon_str,type = "histogram",
                     name = "Street Light Outages")
hist.type <- add_trace(hist.type,x = yr15.street$open_mon_str,
                       name = "Requests for Street Cleaning")
hist.type <- add_trace(hist.type,x = yr15.parking$open_mon_str,
                       name = "Parking Enforcement")
hist.type <- add_trace(hist.type,x = yr15.potholes$open_mon_str,
                       name = "Request for Pothole Repair")
hist.type <- add_trace(hist.type,x = yr15.trash$open_mon_str,
                       name = "Missed Trash/Recycling/Yard Waste/Bulk Item")
hist.type <- add_trace(hist.type,x = yr15.snow$open_mon_str,
                       name = "Request for Snow Plowing")
hist.type <- layout(hist.type, title = "Tickets by Type, 2015 \n(Top 6 Types)",
                    xaxis = list(title="Month"),
                    yaxis = list(title="Num of Tickets"))
hist.type <- layout(hist.type, barmode = "stack")
hist.type
plotly_IMAGE(hist.type, width = 500, height = 500, format = "png", scale = 2,
             out_file = "Tickets_by_type.png")
# plotly_POST(hist.type, filename="tickets_by_type")


# ----- BAR: tickets by type over 2015
#===================================================
bar.overdue <- plot_ly(x=overdue.table$Department,
                        y=overdue.table$pct_overdue,type = "bar")
bar.overdue <- layout(bar.overdue, title = "Percent of Tickets Overdue <br>(Closed past target)",
                    xaxis = list(title="Department"),
                    yaxis = list(title="Percent overdue"))
bar.overdue

plotly_IMAGE(bar.overdue, width = 500, height = 500, format = "png", scale = 2,
             out_file = "Dept_Pct_Overdue.png")


# ----- moving back to workdir
setwd(workdir)

