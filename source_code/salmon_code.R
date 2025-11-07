library(tidyverse)
library(rvest)
library(plotly)
library(janitor)

wy <- 2026
url <- 'https://filelib.wildlife.ca.gov/Public/salvage/Salmon%20Monitoring%20Team%20and%20Sturgeon/' #site with salvage files
season_start <- ymd(paste0(wy-1,'-10-01'))
season_end <- ymd(paste0(wy,'-06-30'))
##########################################
#pull in latest salmon and steelhead files
##########################################

#isolating csv files on CDFW ftp site
url_root <- 'https://filelib.wildlife.ca.gov'
page <- read_html(url) #read url in
links <- page %>% html_nodes("a") %>% html_attr("href") # Extract all links from the webpage

salmon_links <- grep(paste0("Salmon_",wy), links, value = TRUE, ignore.case = TRUE)
salmon_file <- max(grep("\\.csv$", salmon_links, value = TRUE, ignore.case = TRUE))

steelhead_links <- grep(paste0('Steelhead_Salvage_Summary_', wy), links, value = TRUE, ignore.case = TRUE)
steelhead_file <- max(grep("\\.csv$", steelhead_links, value = TRUE, ignore.case = TRUE))

#reading csv files
salmon <- tryCatch(
  read_csv(paste0(url_root, salmon_file)),
  error = function(e) NULL
)
steelhead <- tryCatch(read_csv(paste0(url_root, steelhead_file)),
                      error = function(e) NULL
                      )


###########################################
#summarize data for report and graphs
###########################################
dates <- data.frame(Date = seq(season_start, season_end, by = '1 days'))
#natural winter-run
# wr_loss <- salmon %>% 
#   filter(AdClip == 'N',
#          Size_Run == 'W' & (DNA_Run == 'W' | is.na(DNA_Run))) %>%
#   pivot_wider(names_from = 'BuildingCode', values_from = 'LOSS') %>%
#   right_join(dates, by = c('SampleDate' = 'Date')) %>%
#   mutate(Loss = if_else(is.na(Loss), 0, LOSS)) %>%
#   arrange(SampleDate) %>%
#   mutate(cumul_loss = cumsum(LOSS)) %>%
#   mutate(text = paste(
#     "<span style='font-size:16px;'><b>", SampleDate, "</b></span><br>",
#     "<br><b>Daily Loss (M):</b> ", pop,
#     "<br><b>Life Expectancy (years):</b> ", round(lifeExp, 2),
#     "<br><b>Gdp per capita:</b> $", round(gdpPercap, 2),
#     sep=""
#   ))

#hatchery winter-run
wr_hatch_loss <- tryCatch(salmon %>%
  filter(AdClip == 'C',
         CWT_Run == 'W') %>%
  pivot_wider(names_from = 'BuildingCode', values_from = 'LOSS') %>%
  right_join(dates, by = c('SampleDate' = 'Date')) %>%
  mutate(across((ncol(.) - 1):ncol(.), ~replace_na(.x, 0))) %>%
  mutate(Loss = F + NS) %>%
  mutate(Loss = if_else(is.na(Loss), 0, Loss)) %>%
  arrange(SampleDate) %>%
  mutate(cumul_loss = cumsum(Loss)) %>%
  mutate(text = paste(
    "<span style='font-size:16px;'><b>", SampleDate, "</b></span><br>",
    "<br><b>Cumulative Loss:</b> ", cumul_loss,
    "<br><b>Total Daily Loss:</b> ", Loss,
    "<br><b>CVP Daily Loss :</b> ", F,
    "<br><b>SWP Daily Loss", NS,
    sep="")),
  error = function(e) NULL)

# hatch_facility_loss <- salmon %>%
#   filter(AdClip == 'C',
#          CWT_Run == 'W') %>%
#   mutate(BuildingCode = factor(BuildingCode, levels = c('F', 'NS'),
#                                labels = c('CVP', 'SWP')))
# 
# hatch_graph <- ggplot() +
#   geom_line(wr_hatch_loss, mapping = aes(x = SampleDate, y = cumul_loss, group = 1, text = text)) +
#   geom_col(hatch_facility_loss, 
#            mapping = aes(x = SampleDate, y = LOSS, fill = BuildingCode)) +
#   labs(x = 'Date', y = 'Cumulative Loss', fill = 'Facility') +
#   scale_fill_manual(values = c("#009E73", "#D55E00")) +
#   xlim(min_date, season_end) +
#   theme_bw() +
#   theme(legend.position = 'bottom',
#         plot.margin = margin(0.2, 0.5, 0.2, 0.2, unit = 'cm'),
#         axis.title.y = element_text(margin = margin(r = 15), size = 15),
#         axis.title.x = element_text(margin = margin(t = 15), size = 15),
#         strip.text = element_text(size = 13),
#         axis.text.x = element_text(size = 13),  # Adjust angle
#         axis.text.y = element_text(size = 13),)
# hatch_graph
# 
# hatch_graph_interact <- ggplotly(hatch_graph, tooltip="text") %>%
#   layout(legend = list(orientation = "h",   # horizontal
#                        x = 0.5, y = -0.2,   # position
#                        xanchor = "center",
#                        yanchor = "top"))
# hatch_graph_interact
#steelhead


#hatchery steelhead

###########################################
#pull in migration timing table
###########################################

timing_url <- 'https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY2026/samt_hrt.html'
timing <- read_html(timing_url) %>% 
  html_nodes("table") %>%
  html_table(fill = T) 
timing_table <- timing[[1]] %>%
  mutate(across(2:7, ~gsub("^(-?\\d+\\.?\\d*%).*", "\\1", .))) %>%
  select(1,2,3,4,'Sac Trawl (Sherwood)' = 5, 'Chipps Island Trawl' = 6, 7)

wr_natural_timing <- timing_table %>%
  slice(1,4)

delta_entry_wr <- wr_natural_timing[1,4] %>%
  pull()
delta_exit_wr <- wr_natural_timing[1,6] %>%
  pull()
salvage_wr <- wr_natural_timing[2,7] %>%
  pull()

sh_natural_timing <- timing_table %>%
  filter(grepl('steelhead', Species, ignore.case = TRUE)) %>%
  mutate(Species = 'Steelhead, Unclipped') %>%
  pivot_longer(names_to = 'measure',
               values_to = 'value', -1) %>%
  group_by(Species, measure) %>%
  summarize(value = max(value)) %>%
  pivot_wider(names_from = 'measure',
              values_from = 'value') %>%
  select(4,7,3,5,2,6) %>%
  mutate_all(na_if,"")

delta_entry_sh <- sh_natural_timing[1,4] %>%
  pull()
delta_exit_sh <- sh_natural_timing[1,6] %>%
  pull()
salvage_sh <- sh_natural_timing[1,7] %>%
  pull()