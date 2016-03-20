#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
### REGRESSIONS on EQUITY + Scatterplots


# ----- set up plots directory
setwd(paste0(getwd(),"/Plots"))

# ----- setting plot parameters
pngwidth <- 4
pngheight <- 4
pngres <- 300

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
     xlab="Hispanic or Latin in Origin Population (%)", ylab="Per Capita 311 Use",
     main="Per Capita Tickets and Percent Black")
abline(reg.pct_black, col="blue")
summary.lm(reg.pct_black)
dev.off()

# ----- adding weights
png("pct_black_w.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_black.w <- lm(pc_count~pct_black,dataByTract,weights=as.numeric(pop))
symbols(x = dataByTract$pct_black, y = dataByTract$pc_count,
        circles = dataByTract$pop, inches = 1/6,
     xlab="Hispanic or Latin in Origin Population (%)", ylab="Per Capita 311 Use",
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

# ----- multiple regression
reg.race_inc <- lm(pc_count ~ Med_Inc_K + pct_white + pct_black + pct_hisp_latin,
                   dataByTract)
reg.race_inc.w <- lm(pc_count ~ Med_Inc_K + pct_white + pct_black + pct_hisp_latin,
                   dataByTract, weights=as.numeric(pop))

# ----- moving into regression directory
setwd('..')
getwd()
setwd(paste0(getwd(),"/Regressions"))

# ----- writing reg output to pretty html tables
write(stargazer(reg.pct_white,reg.pct_white.w,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html",
                add.lines = list(c("Pop Weights?", "No", "Yes")),
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("Percent of Tract White"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets (2015)",
                model.numbers          = FALSE),
      file = "Reg_pct_white.html")













write(stargazer(reg.pct_white,reg.pct_hisp_latin, reg.pct_black, reg.Med_Inc_K,reg.race_inc,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html", 
                covariate.labels = c("Percent of Tract White"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Per Capita Tickets",
                model.numbers          = FALSE), 
      file = "Reg_Full.html")



## using plotly
# scat.pct_nonwhite <- plot_ly(dataByTract, x = pct_nonwhite, y = pc_count, 
#                              text = paste("Tract: ",id,'<br>',"P/c Tickets: ", pc_count),
#         mode = "markers", size = as.numeric(pop), opacity = pct_nonwhite)
# scat.pct_nonwhite <- layout(scat.pct_nonwhite, title = 
#                               'Per Capita 311 Tickets by % Non-White<br>(Diameter ~ Population)',
#                             xaxis = list(title="% of Tract Non-White"),
#                             yaxis = list(title="Num of Tickets"))
# scat.pct_nonwhite