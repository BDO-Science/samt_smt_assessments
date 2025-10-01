library(tidyverse)
library(rvest)

wy <- 2025
url <- 'https://filelib.wildlife.ca.gov/Public/salvage/Salmon%20Monitoring%20Team%20and%20Sturgeon/' #site with salvage files
#####pull in latest salmon and steelhead files
url_root <- 'https://filelib.wildlife.ca.gov'
page <- read_html(url) #read url in
links <- page %>% html_nodes("a") %>% html_attr("href") # Extract all links from the webpage
salmon <- max(grep(paste0("Salmon_",wy), links, value = TRUE, ignore.case = TRUE))

salmon <- read_csv(paste0(url_root,salmon))
steelhead
