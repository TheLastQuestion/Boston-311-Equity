#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
### MAPPING the data geospatially


# ----- set up work directory
workdir <- workdir2

# ----- mapping per capita use + neighb overlay + neighb labels with ggplot
map_tickets_neighbs_labels <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pc_count), color = "light gray", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "dark gray", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "blue", name = "P/c Tickets",
                       breaks = pretty_breaks(n = 11)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Per Capita 311 Tickets by Census Tract",
       fill = "") +
  geom_text(data=neighb_labels, aes(x = V1, y = V2, label = NAME2), 
            size=5, fontface="bold", color = "black")  
map_tickets_neighbs_labels

# ----- mapping per capita use + neighb overlay + NO labels with ggplot
map_tickets_neighbs <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pc_count), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "blue", name = "P/c Tickets",
                      breaks = pretty_breaks(n = 11)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Per Capita 311 Tickets by Census Tract",
       fill = "") 
map_tickets_neighbs

# ----- mapping tickets + neighb overlay 
map_tickets_neighbs_raw_15 <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = reqs), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "blue", name = "Tickets",
                      limits = c(1,2000)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "311 Tickets by Census Tract (2015)",
       fill = "") 
map_tickets_neighbs_raw_15

# ----- mapping tickets + neighb overlay 2012 DATA
map_tickets_neighbs_raw_12 <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = reqs12), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "blue", name = "Tickets",
                      limits = c(1,2000)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "311 Tickets by Census Tract (2012)",
       fill = "") 
map_tickets_neighbs_raw_12

# ----- mapping percent white
plotData$pct_nonwhite <- 100-plotData$pct_white
map_white <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pct_white), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Percent",
                      breaks = pretty_breaks(n = 11)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Percent of Census Tract 'White'",
       fill = "") 
map_white

# ----- mapping percent hispanic or latino
map_hisp <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pct_hisp_latin), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Percent",
                      breaks = pretty_breaks(n = 11)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Percent of Census Tract 'Hispanic or Latino Origin'",
       fill = "") 
map_hisp

# ----- mapping percent black
map_black <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pct_black), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Percent",
                      breaks = pretty_breaks(n = 8)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Percent of Census Tract 'Black'",
       fill = "") 
map_black

# ----- mapping median income
map_inc <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = Med_Inc), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "darkred", name = "Median Inc.",
                      breaks = pretty_breaks(n = 8)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Median Income by Tract",
       fill = "") 
map_inc

# ----- mapping enrolment
map_enrol <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = pct_enrol), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "darkred", name = "Percent",
                      breaks = pretty_breaks(n = 8)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Grad + Undergrad Populaiton as % of Tract",
       fill = "") 
map_enrol


# ----- mapping poor, white, low-use tracts
plotData$enrol_poor_white_lowuse <- ifelse( plotData$Med_Inc < 50000 &
                                       plotData$pct_white > 60 &
                                       plotData$pc_count <.3,plotData$pct_enrol,
                                       NA)
map_enrol_pwl <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = enrol_poor_white_lowuse), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "darkred", name = "Percent",
                      breaks = pretty_breaks(n = 8)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Grad + Undergrad Populaiton as % of Tract \n(limited to income<50k, %white>60, P/c 311 <0.3",
       fill = "") 
map_enrol_pwl

# ----- getting at the student story
plotData$poor_white <- ifelse(plotData$Med_Inc>80000 |
                                plotData$pct_white<50,NA,plotData$Med_Inc)
map_poor_white <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = poor_white), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "darkred", name = "Med. Income",
                      breaks = pretty_breaks(n = 8)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Median Income \n(limited to income<80k, % white>50)",
       fill = "") 
map_poor_white

# ------ student income
plotData$highstud_inc <- ifelse(plotData$pct_enrol<20,
                                  NA,plotData$Med_Inc)
map_highstud_inc <- ggplot() +
  geom_polygon(data = plotData, aes(x = long.x, y = lat.x, group = group,
                                    fill = highstud_inc), color = "black", size = .05) +
  geom_polygon(data = bos_neighbs, mapping = aes(long,lat,group=group), # tracts plot
               color = "black", fill = NA, size = 1) + # adding neighborhood trace
  coord_map() + # preventing stretching
  scale_fill_gradient(low = "white", high = "darkred", name = "Med. Income",
                      breaks = pretty_breaks(n = 8)) + # setting fill + fill breaks
  guides(fill = guide_legend(reverse = TRUE)) + # denser = darker, at top
  theme_nothing(legend = TRUE) + # drop off coordinate plane background
  labs(title = "Median Income \n(limited to 20+% university students",
       fill = "") 
map_highstud_inc

# ----- Select folder for saving maps in maps folder
setwd(paste0(workdir,"/Maps"))

ggwidth <- 4
ggheight <- 4
ggscale <- 1  
ggdpi <- 300

ggsave("Map_PCtickets_NeighbLabels.png", plot = map_tickets_neighbs_labels, 
       scale = 2.5, width = 4, height = 4, units = c("in"), dpi = 300)
ggsave("Map_PCtickets_Neighb_Lines.png", plot = map_tickets_neighbs, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_white.png", plot = map_white, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_hisp.png", plot = map_hisp, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_black.png", plot = map_black, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_inc.png", plot = map_inc, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_enrol.png", plot = map_enrol, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_enrol_poor_white.png", plot = map_enrol_pwl, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_TOTAL_tickets_15.png", plot = map_tickets_neighbs_raw_15, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_TOTAL_tickets_12.png", plot = map_tickets_neighbs_raw_12, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_poor_white.png", plot = map_poor_white, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)
ggsave("map_high_students_income.png", plot = map_highstud_inc, 
       scale = ggscale, width = ggwidth, height = ggheight, units = c("in"), dpi = ggdpi)


# ----- moving back to workdir
setwd(workdir)
                          

# ------- mapping with plotly
# plotData$hover <- with(plotData, paste(id, '<br>', "PC_Use", pc_count, "% White", pct_white))
# n <- ggplot() +
#   geom_polygon(data = plotData, aes(x = long.x, y = lat.x, 
#                                     group = group, fill = pc_count), color = "black", size = 0.05) +
#   coord_map() +
#   scale_fill_distiller(palette = "Greens", direction=1) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   theme(legend.position = "bottom") +
#   labs(title = "Total 311 Tickets in 2015 (in '000s)",
#        fill = "")
# n2 <- ggplotly(n)
# n3 <- layout(n2, title = 
#             'Per Capita 311 Tickets',
#             text = plotData$hover)
# n3

