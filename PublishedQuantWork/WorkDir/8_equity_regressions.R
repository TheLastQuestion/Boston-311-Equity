#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
### REGRESSIONS on EQUITY + Scatterplots


# ----- set up plots directory
setwd(paste0(workdir,"/Plots"))

# ----- setting plot parameters
pngwidth <- 4
pngheight <- 4
pngres <- 300

###########################################
##################################
########## 2015 regressions ##########
###############################

# ----- Regressing pc_count on %non-white
dataByTract$pct_nonwhite <- 100 - dataByTract$pct_white
png("pct_nonwhite.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_nonwhite <- lm(pc_count~pct_nonwhite,dataByTract)
plot(x = dataByTract$pct_nonwhite, y = dataByTract$pc_count,
     xlab="non-White Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and % non-White")
abline(reg.pct_nonwhite, col="Blue")
summary.lm(reg.pct_nonwhite)
dev.off()

# ----- adding weights
png("pct_nonwhite_w.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_nonwhite.w <- lm(pc_count~pct_nonwhite,dataByTract,weights=as.numeric(pop))
symbols(x = dataByTract$pct_nonwhite, y = dataByTract$pc_count,
        circles = dataByTract$pop, inches = 1/6, 
     xlab="non-White Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and % non-White \n (Population-weighted)")
abline(reg.pct_nonwhite, col="Blue")
abline(reg.pct_nonwhite.w, col="red")
summary.lm(reg.pct_nonwhite.w)
dev.off()

# ----- Regressing pc_count on %white
png("pct_white.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_white <- lm(pc_count~pct_white,dataByTract)
plot(x = dataByTract$pct_white, y = dataByTract$pc_count,
     xlab="White Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and % White")
abline(reg.pct_white, col="Blue")
summary.lm(reg.pct_white)
dev.off()

# ----- ADDING WEIGHTS
png("pct_white_w.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_white.w <- lm(pc_count~pct_white,dataByTract,weights=as.numeric(pop))
symbols(x = dataByTract$pct_white, y = dataByTract$pc_count,
        circles = dataByTract$pop, inches = 1/6,
     xlab="White Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and % White \n (Population-weighted)")
abline(reg.pct_white, col="Blue")
abline(reg.pct_white.w, col="red")
summary.lm(reg.pct_white.w)
dev.off()

# ----- Regressing pc_count on % hisp/latin
png("pct_hisp_latin.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_hisp_latin <- lm(pc_count~pct_hisp_latin,dataByTract)
plot(x = dataByTract$pct_hisp_latin, y = dataByTract$pc_count,
     xlab="Hispanic or Latin in Origin Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Percent Hispanic/Latin")
abline(reg.pct_hisp_latin, col="Blue")
summary.lm(reg.pct_hisp_latin)
dev.off()

# ----- Adding weights
png("pct_hisp_latin_w.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_hisp_latin.w <- lm(pc_count~pct_hisp_latin,dataByTract,weights=as.numeric(pop))
symbols(x = dataByTract$pct_hisp_latin, y = dataByTract$pc_count,
     circles = dataByTract$pop, inches = 1/6,
     xlab="Hispanic or Latin in Origin Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Percent Hispanic/Latin \n (Population-weighted)")
abline(reg.pct_hisp_latin, col="Blue")
abline(reg.pct_hisp_latin.w, col="red")
summary.lm(reg.pct_hisp_latin.w)
dev.off()

# ----- Regressing pc_count on % black
png("pct_black.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_black <- lm(pc_count~pct_black,dataByTract)
plot(x = dataByTract$pct_black, y = dataByTract$pc_count,
     xlab="Black Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Percent Black")
abline(reg.pct_black, col="blue")
summary.lm(reg.pct_black)
dev.off()

# ----- adding weights
png("pct_black_w.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_black.w <- lm(pc_count~pct_black,dataByTract,weights=as.numeric(pop))
symbols(x = dataByTract$pct_black, y = dataByTract$pc_count,
        circles = dataByTract$pop, inches = 1/6,
     xlab="Black Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Percent Black \n (Population-weighted)")
abline(reg.pct_black, col="blue")
abline(reg.pct_black.w, col="red")
summary.lm(reg.pct_black.w)
dev.off()

# ----- Regressing pc_count on median income
png("Med_Inc_K.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
dataByTract$Med_Inc_K <- dataByTract$Med_Inc/1000
reg.Med_Inc_K <- lm(pc_count~Med_Inc_K,dataByTract)
plot(x = dataByTract$Med_Inc_K, y = dataByTract$pc_count,
     xlab="Median Income (thousands)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Median Income")
abline(reg.Med_Inc_K, col="blue")
summary.lm(reg.Med_Inc_K)
dev.off()

# ----- adding weights
png("Med_Inc_K_w.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.Med_Inc_K.w <- lm(pc_count~Med_Inc_K,dataByTract,weights=as.numeric(pop))
symbols(x = dataByTract$Med_Inc_K, y = dataByTract$pc_count,
        circles = dataByTract$pop, inches = 1/6,
     xlab="Median Income (thousands)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Median Income \n (Population-weighted)")
abline(reg.Med_Inc_K, col="blue")
abline(reg.Med_Inc_K.w, col="red")
summary.lm(reg.Med_Inc_K.w)
dev.off()

# ----- Regressing pc_count on LOG OF median income
dataByTract$log_inc <- log(dataByTract$Med_Inc_K)
png("Log_Inc.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.log_inc <- lm(pc_count~log_inc,dataByTract)
plot(x = dataByTract$log_inc, y = dataByTract$pc_count,
     xlab="Log of Median Income", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Log of Median Income")
abline(reg.log_inc, col="blue")
summary.lm(reg.log_inc)
dev.off()

# ----- adding weights
png("Log_Inc_w.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.log_inc.w <- lm(pc_count~log_inc,dataByTract,weights=as.numeric(pop))
symbols(x = dataByTract$log_inc, y = dataByTract$pc_count,
        circles = dataByTract$pop, inches = 1/6,
        xlab="Log of Median Income", ylab="Per Capita 311 Use",
        main="Per Capita Tickets and Log of Median Income \n (Population-weighted)")
abline(reg.log_inc, col="blue")
abline(reg.log_inc.w, col="red")
summary.lm(reg.log_inc.w)
dev.off()

# ----- Regressing pc_count on % below poverty level
dataByTract$pct_below_pov <- as.numeric(dataByTract$Pct_Below_Pov_lev)
png("Pct_Below_PovLev.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.povlev <- lm(pc_count~pct_below_pov,dataByTract)
plot(x = dataByTract$pct_below_pov, y = dataByTract$pc_count,
     xlab="% Below Poverty Level", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and % Below Poverty Level")
abline(reg.povlev, col="blue")
summary.lm(reg.povlev)
dev.off()

# ----- adding weights
png("Pct_Below_PovLev_w.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.povlev.w <- lm(pc_count~pct_below_pov,dataByTract,weights=as.numeric(pop))
symbols(x = dataByTract$pct_below_pov, y = dataByTract$pc_count,
        circles = dataByTract$pop, inches = 1/6,
        xlab="% Below Poverty Level", ylab="Per Capita 311 Use",
        main="Per Capita Tickets and % Below Poverty Level \n (Population-weighted)")
abline(reg.povlev, col="blue")
abline(reg.povlev.w, col="red")
summary.lm(reg.povlev.w)
dev.off()


# ----- multiple regressions
reg.race_inc <- lm(pc_count ~ Med_Inc_K + pct_white + pct_black + pct_hisp_latin,
                   dataByTract)
reg.race_inc.w <- lm(pc_count ~ Med_Inc_K + pct_white + pct_black + pct_hisp_latin,
                   dataByTract, weights=as.numeric(pop))
reg.white.inc.w <- lm(pc_count ~ pct_white + Med_Inc_K,
                      dataByTract, weights=as.numeric(pop))
reg.hisp.inc.w <- lm(pc_count ~ pct_hisp_latin + Med_Inc_K,
                      dataByTract, weights=as.numeric(pop))
reg.black.inc.w <- lm(pc_count ~ pct_black + Med_Inc_K,
                     dataByTract, weights=as.numeric(pop))
reg.education.w <- lm(pc_count ~  as.numeric(bach_or_higher) + as.numeric(some_college) + as.numeric(hs_grad),
                      dataByTract, weights=as.numeric(pop))
#as.numeric(less_than_hs) +

# ----- moving into regression directory
setwd(paste0(workdir,"/Regressions"))

# ----- writing reg output to pretty html tables

write(stargazer(reg.povlev,reg.povlev.w,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html",
                add.lines = list(c("Pop Weights?", "No", "Yes")),
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("% Below PovLev"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = TRUE),
      file = "Reg_povlev.html")

write(stargazer(reg.log_inc,reg.log_inc.w,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html",
                add.lines = list(c("Pop Weights?", "No", "Yes")),
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("Log of Median Income (K)"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = TRUE),
      file = "Reg_log_med_inc.html")

write(stargazer(reg.pct_white,reg.pct_white.w,reg.white.inc.w,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html",
                add.lines = list(c("Pop Weights?", "No", "Yes","Yes")),
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("Percent of Tract White","Median Income (K)"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = TRUE),
      file = "Reg_pct_white.html")

write(stargazer(reg.pct_hisp_latin,reg.pct_hisp_latin.w,reg.hisp.inc.w,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html",
                add.lines = list(c("Pop Weights?", "No", "Yes","Yes")),
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("% Hisp. or Lat."),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = TRUE),
      file = "Reg_pct_hisp_latin.html")

write(stargazer(reg.pct_black,reg.pct_black.w,reg.black.inc.w,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html",
                add.lines = list(c("Pop Weights?", "No", "Yes","Yes")),
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("% Black"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = TRUE),
      file = "Reg_pct_black.html")

write(stargazer(reg.Med_Inc_K,reg.Med_Inc_K.w,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html",
                add.lines = list(c("Pop Weights?", "No", "Yes")),
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("Median Income"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = FALSE),
      file = "Reg_med_inc_k.html")

write(stargazer(reg.pct_white,reg.pct_hisp_latin, reg.pct_black, reg.Med_Inc_K,reg.race_inc,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html", 
                covariate.labels = c("Percent of Tract White"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets",
                model.numbers          = FALSE), 
      file = "Reg_Full.html")


write(stargazer(reg.education.w,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html", 
                covariate.labels = c("% Bach/higher","% Some College","% HS Grad","% Less Than HS"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets",
                model.numbers          = FALSE), 
      file = "Reg_education.html")

##################
## app vs constit calls
############
reg.app_black_inc <- lm(pc_count_app~pct_black+Med_Inc_K,dataByTract,weights=as.numeric(pop))
reg.app_hisp_inc <- lm(pc_count_app~pct_hisp_latin+Med_Inc_K,dataByTract,weights=as.numeric(pop))

reg.constit_black_inc <- lm(pc_count_constit~pct_black+Med_Inc_K,dataByTract,weights=as.numeric(pop))
reg.constit_hisp_inc <- lm(pc_count_constit~pct_hisp_latin+Med_Inc_K,dataByTract,weights=as.numeric(pop))

write(stargazer(reg.app_black_inc,reg.app_hisp_inc,
                title="App-Only Regression Results", 
                align=TRUE,digits = 5, type = "html",
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("% of Tract Black","% of Tract Hisp/Latino","Median Income (K)"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = FALSE),
      file = "Reg_app.html")


write(stargazer(reg.constit_black_inc,reg.constit_hisp_inc,
                title="Calls-Only Regression Results", 
                align=TRUE,digits = 5, type = "html",
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("% of Tract Black","% of Tract Hisp/Latino","Median Income (K)"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = FALSE),
      file = "Reg_calls.html")

##################
## multiple year regressions
############
# ----- Regressing pc_count on median income by year
reg.Med_Inc_K_15 <- lm(pc_count~Med_Inc_K,dataByTract,weights=as.numeric(pop))
reg.Med_Inc_K_14 <- lm(pc_count_14~Med_Inc_K,dataByTract,weights=as.numeric(pop))
reg.Med_Inc_K_13 <- lm(pc_count_13~Med_Inc_K,dataByTract,weights=as.numeric(pop))
reg.Med_Inc_K_12 <- lm(pc_count_12~Med_Inc_K,dataByTract,weights=as.numeric(pop))
reg.Med_Inc_K_11 <- lm(pc_count_11~Med_Inc_K,dataByTract,weights=as.numeric(pop))

write(stargazer(reg.Med_Inc_K_11,reg.Med_Inc_K_12,reg.Med_Inc_K_13,reg.Med_Inc_K_14,reg.Med_Inc_K_15,
                title="Regression Results", 
                column.labels = c("2011","2012","2013","2014","2015"),
                align=TRUE,digits = 5, type = "html",
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("Median Income (K)"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = FALSE),
      file = "Reg_inc_all_years.html")


# ----- basic summaries for paper
summary(dataByTract$count)
summary(dataByTract$pc_count)
summary(dataByTract$Med_Inc_K)
summary(dataByTract$pct_below_pov)
summary(dataByTract$pct_white)
summary(dataByTract$pct_black)
summary(dataByTract$pct_hisp_latin)
summary(as.numeric(dataByTract$bach_or_higher))
summary(as.numeric(dataByTract$some_college))
summary(as.numeric(dataByTract$hs_grad))
summary(as.numeric(dataByTract$less_than_hs))







# ----- moving back to workdir
setwd(workdir)

## using plotly
# scat.pct_nonwhite <- plot_ly(dataByTract, x = pct_nonwhite, y = pc_count, 
#                              text = paste("Tract: ",id,'<br>',"P/c Tickets: ", pc_count),
#         mode = "markers", size = as.numeric(pop), opacity = pct_nonwhite)
# scat.pct_nonwhite <- layout(scat.pct_nonwhite, title = 
#                               'Per Capita 311 Tickets by % Non-White<br>(Diameter ~ Population)',
#                             xaxis = list(title="% of Tract Non-White"),
#                             yaxis = list(title="Num of Tickets"))
# scat.pct_nonwhite