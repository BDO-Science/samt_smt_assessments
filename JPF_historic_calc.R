# JPF Historic data pull [manually, historic files are not online yet]

#lrm 1/26/26

library(pdftools)
library(slider)
library(dplyr)
library(stringr)
library(here)

## Jersey Point Flow ---------------------------------

## Pull historic data (this covers messy period before JPF calc was included for WY26)
hydro_rec <- load(here("JPF_historic_WY26.csv"))

# code to pull data for JPF_historic_WY26.csv
#############################################
### If you need to manually pull data:
#    - this code will read pdfs from DWR, may need to download all data first
#      or request from DWR


# ---- download the PDF Delta Hydrology Conditions (DWR)----
url1 <- "https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/Delta-Hydrologic-Conditions-Daily-Summary.pdf"
tmp1 <- tempfile(fileext = ".pdf")
download.file(url1, tmp1, mode="wb")

pdf1_path <- "data_raw/smelt/20251031rptHydro.pdf"
pdf2_path <- "data_raw/smelt/20251104rptHydro.pdf"
pdf3_path <- "data_raw/smelt/20251202rptHydro.pdf"

# For PDF without JPF
# ---- extract raw text ---- 
txt <- pdf_text(pdf3_path) #change this for specific file

# ---- parse page 1 table ----
page1 <- txt[1] %>% str_split("\n") %>% unlist()

# find lines with data (dates etc.)
data_lines1 <- page1[str_detect(page1, "\\d{1,2}/\\d{1,2}")]

# split each row by whitespace
hydro1 <- data_lines1 %>%
  str_squish() %>%
  str_replace_all("[^[:print:]]", "") %>%
  .[str_detect(., "^\\d{1,2}/\\d{1,2}/\\d{2,4}")] %>%
  str_split_fixed(" ", n = 11) %>%
  as.data.frame(stringsAsFactors = FALSE)

# add column names
colnames(hydro1) <- c(
  "Date","SR_at_Freeport_SRWTP","Yolo_Rumsey_FRE_FWB", "E_side_streams",
  "SJR_a_Vernalis", "Stockton_rain_in","CCF_cfs",
  "Tracy_cfs","CCWD_cfs","Barker_Slough_cfs",
  "Byron_Bethany_cfs"
)

numeric.col1 <- c("SR_at_Freeport_SRWTP","Yolo_Rumsey_FRE_FWB", "E_side_streams",
                  "SJR_a_Vernalis", "Stockton_rain_in","CCF_cfs",
                  "Tracy_cfs","CCWD_cfs","Barker_Slough_cfs",
                  "Byron_Bethany_cfs")
hydro1 <- hydro1 %>% 
  dplyr::filter(str_detect(Date, "\\d{1,2}/\\d{1,2}")) %>% 
  mutate(across(all_of(numeric.col1),
                ~ str_remove_all(.x, "[^0-9.-]") %>% as.numeric())) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))



# ---- parse page 2 table ----
page2 <- txt[2] %>% str_split("\n") %>% unlist()
data_lines2 <- page2[str_detect(page2, "\\d{1,2}/\\d{1,2}")]
hydro2 <- data_lines2 %>%
  str_squish() %>%
  str_replace_all("[^[:print:]]", "") %>%
  .[str_detect(., "^\\d{1,2}/\\d{1,2}/\\d{2,4}")] %>%
  str_split_fixed(" ", n = 9) %>%
  as.data.frame(stringsAsFactors = FALSE)

colnames(hydro2) <- c(
  "Date","Banks_PP_cfs","Delta_GCD_cfs","Rio_Vista_Flow_cfs",
  "QWEST_cfs", "NDOI_cfs","EI_3day","EI_14day","Delta_Status"
)

numeric.col2 <- c("Banks_PP_cfs","Delta_GCD_cfs","Rio_Vista_Flow_cfs",
                  "QWEST_cfs", "NDOI_cfs","EI_3day","EI_14day")

hydro2 <- hydro2 |>
  dplyr::filter(str_detect(Date, "\\d{1,2}/\\d{1,2}")) %>% 
  mutate(across(all_of(numeric.col2),
                ~ str_remove_all(.x, "[^0-9.-]") %>% as.numeric())) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))


#join tables
hydro_comb <- hydro1 %>%
  left_join(hydro2, by = "Date") %>% 
  mutate(JPF_cfs = NA)

#select cols of interest
hydroOct <- hydro_comb %>%
  select(Date, SJR_a_Vernalis, E_side_streams, SR_at_Freeport_SRWTP, Stockton_rain_in,
         Delta_GCD_cfs, JPF_cfs, Banks_PP_cfs, CCF_cfs, Tracy_cfs)

hydroNov <- hydro_comb %>%
  select(Date, SJR_a_Vernalis, E_side_streams, SR_at_Freeport_SRWTP, Stockton_rain_in,
         Delta_GCD_cfs, JPF_cfs, Banks_PP_cfs, CCF_cfs, Tracy_cfs)

hydroNov2 <- hydro_comb %>%
  select(Date, SJR_a_Vernalis, E_side_streams, SR_at_Freeport_SRWTP, Stockton_rain_in,
         Delta_GCD_cfs, JPF_cfs, Banks_PP_cfs, CCF_cfs, Tracy_cfs)

##############################################
# ---- For PDF WITH JPF
#pdf4_path <- "data_raw/smelt/20260101rptHydro.pdf"
pdf5_path <- "data_raw/smelt/20260129rptHydro.pdf"


# ---- extract raw text ---- 
txt <- pdf_text(pdf5_path)

# ---- parse page 1 table ----
page1 <- txt[1] %>% str_split("\n") %>% unlist()

# find lines with data (dates etc.)
data_lines1 <- page1[str_detect(page1, "\\d{1,2}/\\d{1,2}")]

# split each row by whitespace
hydro1 <- data_lines1 %>%
  str_squish() %>%
  str_replace_all("[^[:print:]]", "") %>%
  .[str_detect(., "^\\d{1,2}/\\d{1,2}/\\d{2,4}")] %>%
  str_split_fixed(" ", n = 11) %>%
  as.data.frame(stringsAsFactors = FALSE)

# add column names
colnames(hydro1) <- c(
  "Date","SR_at_Freeport_SRWTP","Yolo_Rumsey_FRE_FWB", "E_side_streams",
  "SJR_a_Vernalis", "Stockton_rain_in","CCF_cfs",
  "Tracy_cfs","CCWD_cfs","Barker_Slough_cfs",
  "Byron_Bethany_cfs"
)

numeric.col1 <- c("SR_at_Freeport_SRWTP","Yolo_Rumsey_FRE_FWB", "E_side_streams",
                  "SJR_a_Vernalis", "Stockton_rain_in","CCF_cfs",
                  "Tracy_cfs","CCWD_cfs","Barker_Slough_cfs",
                  "Byron_Bethany_cfs")
hydro1 <- hydro1 %>% 
  dplyr::filter(str_detect(Date, "\\d{1,2}/\\d{1,2}")) %>% 
  mutate(across(all_of(numeric.col1),
                ~ str_remove_all(.x, "[^0-9.-]") %>% as.numeric())) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))



# ---- parse page 2 table ----
page2 <- txt[2] %>% str_split("\n") %>% unlist()
data_lines2 <- page2[str_detect(page2, "\\d{1,2}/\\d{1,2}")]
hydro2 <- data_lines2 %>%
  str_squish() %>%
  str_replace_all("[^[:print:]]", "") %>%
  .[str_detect(., "^\\d{1,2}/\\d{1,2}/\\d{2,4}")] %>%
  str_split_fixed(" ", n = 10) %>%
  as.data.frame(stringsAsFactors = FALSE)

colnames(hydro2) <- c(
  "Date","Banks_PP_cfs","Delta_GCD_cfs","Rio_Vista_Flow_cfs",
  "QWEST_cfs", "JPF_cfs", "NDOI_cfs","EI_3day","EI_14day","Delta_Status"
)

numeric.col2 <- c("Banks_PP_cfs","Delta_GCD_cfs","Rio_Vista_Flow_cfs",
                  "QWEST_cfs","JPF_cfs", "NDOI_cfs","EI_3day","EI_14day")

hydro2 <- hydro2 |>
  dplyr::filter(str_detect(Date, "\\d{1,2}/\\d{1,2}")) %>% 
  mutate(across(all_of(numeric.col2),
                ~ str_remove_all(.x, "[^0-9.-]") %>% as.numeric())) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

#join tables
hydro_comb <- hydro1 %>%
  left_join(hydro2, by = "Date")

#select cols of interest
hydroJan <- hydro_comb %>%
  select(Date, SJR_a_Vernalis, E_side_streams, SR_at_Freeport_SRWTP, Stockton_rain_in,
         Delta_GCD_cfs, JPF_cfs, Banks_PP_cfs, CCF_cfs, Tracy_cfs)



# Combination of files

hydro_rec <- rbind.data.frame(hydroOct, hydroNov[27:28,], hydroNov2, hydroDec[1:27,], hydroJan)

JPF_hist <- hydro_rec[,c(1,7)]

#Save for SacPas
write_csv(JPF_hist, "JPF_historic_WY26.csv")

##################################################################################
# CALCULATE HISTORIC JPF (use for data prior to 12/14)

# once you have data pulled into R (see above)

#set up variables
hydro_calc <- hydro_rec %>%
  mutate(
    QXGEO = ((0.133 * SR_at_Freeport_SRWTP) + 829),
    #Delta_precip = Stockton_rain_in / 12/ 5 * 682230 * 0.5041666604 *0.65,
    Delta_precip = (Stockton_rain_in * 679000 ) / ( 12 * 1.9835 *5),
    Delta_prec5d = slide_dbl(Delta_precip, ~sum(.x), .before = 5, .after = -1,
                             .complete= TRUE),
    Delta_div = Delta_GCD_cfs, 
    pumps = CCF_cfs + Tracy_cfs #updated to use CCF- Bogdon says a better gauge for what the pump is taking
  )

#calc JPF

hydro_calc <- hydro_calc %>%
  arrange(Date) %>% 
  mutate(JPF_calc =
           lag(SJR_a_Vernalis) + # one day lagged
           lag(E_side_streams) + # one day lagged
           lag(QXGEO) - # one day lagged
           (0.65* (lag(Delta_div) - Delta_prec5d)) - #Delta div one day lagged
           pumps) # same day




# Practice figure
#ann_start_jpf <- JPF_hist %>% filter(Date == date(start)) %>% pull(JPF_cfs)

jpf_plot <- jpf_all %>%
  ggplot(aes(x = date)) +
  labs(y = 'Flow (cfs)') +
  annotate("label", x = date(start)+10, y = 15000, label = "Jersey Point Flow", color = "darkturquoise")+
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#888888', linewidth= 1)+
  geom_line(aes(y = jpf_cfs), color= "darkturquoise", linewidth = 1) +
  #scale_color_manual(values = c("#43a419", "gray15")) +
  scale_x_date(date_breaks = '2 weeks', date_labels = '%b %d') +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 15),
        axis.title= element_text(size = 15))