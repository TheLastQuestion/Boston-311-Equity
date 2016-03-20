#===================================================
## Equity in Boston's 311 system
## HKS Master's Capstone Project, 2016
## Bryant Renaud + Nick Ryan
#===================================================
### MAPPING the data geospatially

# ----- set up work directory
setwd(dirname(file.choose())) # find a file in workdir

# ----- mapping per capita use + neighb overlay + neighb labels with ggplot
map_tickets_neighbs_labels <- ggplot() +
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
       fill = "") +
  geom_text(data=neighb_labels, aes(x = V1, y = V2, label = NAME2), 
            size=5, fontface="bold", color = "Red")  
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

# ----- Select folder for saving maps in maps folder
setwd(dirname(file.choose())) # find a file in workdir
ggsave("Map_PCtickets_NeighbLabels.png", plot = map_tickets_neighbs_labels, 
       scale = 2.5, width = 4, height = 4, units = c("in"), dpi = 300)
ggsave("Map_PCtickets_Neighb_Lines.png", plot = map_tickets_neighbs, 
       scale = 2.5, width = 4, height = 4, units = c("in"), dpi = 300)
ggsave("map_white.png", plot = map_white, 
       scale = 2.5, width = 4, height = 4, units = c("in"), dpi = 300)
ggsave("map_hisp.png", plot = map_hisp, 
       scale = 2.5, width = 4, height = 4, units = c("in"), dpi = 300)
ggsave("map_black.png", plot = map_black, 
       scale = 2.5, width = 4, height = 4, units = c("in"), dpi = 300)
ggsave("map_inc.png", plot = map_inc, 
       scale = 2.5, width = 4, height = 4, units = c("in"), dpi = 300)
ggsave("map_enrol.png", plot = map_enrol, 
       scale = 2.5, width = 4, height = 4, units = c("in"), dpi = 300)


                          

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

