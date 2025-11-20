# general
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(knitr)
library(tidyr)
library(sf)
library(stars)
library(readr)
library(janitor)
library(here)
library(purrr)

#IDW
library(sp)
library(gstat)
library(raster)
library(tmap)
#library(rgdal) #figure out

# Visualization
library(ggmap)
library(ggspatial)
library(deltamapr)
library(viridis)

project <- here()

load(here(project, 'input_data/contours_allalts.Rdata'))
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")
inflow_labels = c("Low Sacramento - Low San Joaquin", "Low Sacramento - Medium San Joaquin", "Low Sacramento - High San Joaquin",
                  "Medium Sacramento - Low San Joaquin", "Medium Sacramento - Medium San Joaquin", "Medium Sacramento - High San Joaquin",
                  "High Sacramento - Low San Joaquin", "High Sacramento - Medium San Joaquin", "High Sacramento - High San Joaquin")

alt2a <- contours_all_Alt2a %>%
  mutate(grouper = paste0(group, "_", flow, group2),
         label = paste0(group2, "_", flow)) %>%
  rename(Inflow = group2) 

omr_levels = unique(alt2a$OMR_flow)

alt2a <- alt2a %>%
  mutate(Inflow_text = factor(Inflow, levels = inflow_order, labels = inflow_labels),
         Inflow = factor(Inflow, levels = inflow_order),
         OMR_flow = factor(OMR_flow, levels = omr_levels))

rm(list = ls(pattern = 'contours_all'))

# read in nodes, channels data
nodes <- st_read(here(project,"input_data/shapefiles/nodes.shp")) %>%
  dplyr::select(node)
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
channels0 <- read_csv(here(project, "input_data/DSM2_Version822_Grid_20231102.csv")) %>%
  janitor::clean_names()  %>%
  rename(channel_number = chan_no) %>%
  dplyr::select(-manning, -dispersion)

delta <- st_read(here(project, "input_data/shapefiles/Bay_Delta_Poly_New.shp"))


# Change projections to 4326 (WGS)
delta_4326 <- st_transform(delta, crs = 4326) %>%
  mutate(line = "analysis boundary")
nodes_4326 <- st_transform(nodes, crs = 4326) %>%
  mutate(points = "DSM2 nodes")
WW_Delta_4326 <- st_transform(WW_Delta, crs = st_crs(delta_4326))
WW_Delta_crop <- st_crop(WW_Delta_4326,xmin = -122.2, xmax = -121, ymin = 37.5, ymax = 38.8) %>%
  filter(HNAME!= "SAN FRANCISCO BAY") %>%
  st_simplify(dTolerance = 200)


n_colors <- length(omr_levels)
omr_colors <- viridis::viridis(n_colors, option = "turbo")
names(omr_colors) <- omr_levels

plots <- alt2a %>%
  filter(contour == 0.75) %>%
  group_split(Inflow) %>%
  map(~ {
    # Create dummy data with all OMR levels (won't be visible on plot)
    dummy <- data.frame(
      long = NA, 
      lat = NA, 
      OMR_flow = factor(omr_levels, levels = omr_levels),
      grouper = NA
    )
    
    ggplot() +
      geom_sf(data = WW_Delta_crop, fill = "steelblue", color = NA, alpha = 0.6, inherit.aes = FALSE) +
      geom_path(data = dummy, aes(x = long, y = lat, color = OMR_flow, group = grouper), linewidth = 1) +  # Dummy layer
      geom_path(data = .x, 
                aes(x = long, group = grouper, y = lat, color = OMR_flow), 
                linewidth = 1, inherit.aes = FALSE) +
      ylim(c(37.7, 38.1)) +
      xlim(c(-121.8, -121.2)) +
      scale_color_manual(values = omr_colors) +
      labs(title = 'Zone of Influence', 
           subtitle = paste('Hydrology:', unique(.x$Inflow_text)),
           color = "OMR Bin") +
      theme_bw() +
      theme(axis.title = element_blank(),
            legend.position = 'bottom')
  }) %>%
  set_names(unique(alt2a$Inflow[alt2a$contour == 0.75]))

saveRDS(plots, 'input_data/ZOI_maps.rds')
