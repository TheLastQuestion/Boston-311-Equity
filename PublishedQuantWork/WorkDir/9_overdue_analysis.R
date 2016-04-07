#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
### PERCENT OVERDUE ANALYSIS

# ----- Select folder for saving maps in maps folder
setwd(paste0(workdir,"/Maps"))

# ----- mapping pct_overdue + neighb overlay + NO labels with ggplot
map_overdue_neighbs <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pct_overdue), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "blue", name = "Percent",
                      breaks = pretty_breaks(n = 8)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Percent Overdue Tickets by Census Tract",
       fill = "") 
map_overdue_neighbs
ggsave("pct_overdue.png", plot = map_overdue_neighbs, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)

# ----- Regressing overdue on %non-white
png("overdue_pct_nonwhite.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.pct_nonwhite_overdue <- lm(pct_overdue~pct_nonwhite,dataByTract,weights=as.numeric(pop))
plot(x = dataByTract$pct_nonwhite, y = dataByTract$pct_overdue,
     xlab="non-White Population (%)", ylab="Per cent of tickets overdue",
     main="Percent Overdue and % non-White")
abline(reg.pct_nonwhite_overdue, col="Blue")
summary.lm(reg.pct_nonwhite_overdue)
dev.off()

# ----- Regressing overdue on median income
png("overdue_income.png", width = pngwidth, height = pngheight, units = "in",res=pngres)
reg.overdue_inc <- lm(pct_overdue~Med_Inc_K,dataByTract,weights=as.numeric(pop))
plot(x = dataByTract$Med_Inc_K, y = dataByTract$pct_overdue,
     xlab="Income in K", ylab="Per cent of tickets overdue",
     main="Percent Overdue and Income")
abline(reg.overdue_inc, col="Blue")
summary.lm(reg.overdue_inc)
dev.off()

# ----- Multiple regression
reg.pct_overdue_inc_nonw <- lm(pct_overdue~pct_nonwhite+Med_Inc_K,dataByTract,weights=as.numeric(pop))
summary.lm(reg.pct_overdue_inc_nonw)

# ----- Select regressions folder
setwd(paste0(workdir,"/Regressions"))

write(stargazer(reg.pct_nonwhite_overdue,reg.overdue_inc,reg.pct_overdue_inc_nonw,
                title="Regression Results", 
                align=TRUE,digits = 5, type = "html",
                add.lines = list(c("Pop Weights?", "Yes","Yes","Yes")),
                omit.stat = c("rsq", "f","ser"),
                covariate.labels = c("Percent of Tract non-White","Median Income (K)"),
                dep.var.labels.include = FALSE,
                dep.var.caption  = "Percent Tickets Overdue (2015)",
                model.numbers          = TRUE),
      file = "Reg_overdue.html")



