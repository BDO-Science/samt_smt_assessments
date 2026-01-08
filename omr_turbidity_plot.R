# Quick plots for stormflex assessment: OMR turbidity
#lrm 01/07/2026

# need to run environmental table pull from sacpas in smelt_data_extraction.R

turb <- env_table
turb <- turb %>% 
  clean_names()
turb$date <- ymd(turb$date)
turb$water_turbidity_1_day_old_river_at_bacon_island_usgs_fnu_cdec_obi <- as.numeric(turb$water_turbidity_1_day_old_river_at_bacon_island_usgs_fnu_cdec_obi)
turb$water_turbidity_1_day_old_river_at_franks_tract_fnu_cdec_osj <- as.numeric(turb$water_turbidity_1_day_old_river_at_franks_tract_fnu_cdec_osj)
turb$water_turbidity_1_day_sjr_holland_cut_fnu_cdec_hol <- as.numeric(turb$water_turbidity_1_day_sjr_holland_cut_fnu_cdec_hol)


turb_long <- turb %>% 
  select(date, water_turbidity_1_day_old_river_at_bacon_island_usgs_fnu_cdec_obi, water_turbidity_1_day_old_river_at_franks_tract_fnu_cdec_osj, 
         water_turbidity_1_day_sjr_holland_cut_fnu_cdec_hol)
colnames(turb_long) <- c("date", "OBI", "OSJ", "HOL")

turb_long <- turb_long %>% 
  pivot_longer(cols = -date,
               names_to = "station",
               values_to = "turbidity")


ggplot(turb_long, aes(x = date, y = turbidity, color = station, shape = station, group = station)) +
  geom_hline(yintercept = 12, linetype = "dashed", linewidth = 1, color = "darkgrey") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_date(
    date_breaks = "2 days",
    date_labels = "%b %d"
  ) +
  labs(
    x = "Date (2025-2026)",
    y = "Turbidity (FNU)",
    color = "Station",
    shape = "Station",
    title = "OMR Corridor Turbidity"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )