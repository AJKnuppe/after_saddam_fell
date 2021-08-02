##=============================================================================
## Preprocess Iraqi Body Count (IBC) Data
## Austin Knuppe
## 24 June 2021
##=============================================================================

##-----------------
# clear environment
rm(list=ls())
options(stringsAsFactors = FALSE, scipen = 999)
# source("R/functions.R")

##--------
# Set seed
seed <- sample.int(.Machine$integer.max, 1)
set.seed(seed)

##-------------
# Load Packages
ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "lubridate", "stringr", "janitor", "sf")

ipak(packages)

##---------
# Load Data
# ibc_dat <- read.csv("~/Dropbox/Archive/book_project/data/iraq_body_count/ibc_incidents.csv",
#    stringsAsFactors=FALSE) %>%
#    janitor::clean_names()
# 
irq_admin_units <- read_sf("~/Dropbox/Archive/book_project/data/iraq_shape_files/irq_admbnda_adm3_cso_20190603.shp") %>% 
   janitor::clean_names()

load("~/Dropbox/Archive/book_project/data/iraq_body_count/ibc_dat.RData")

##------------
# Format Dates
# ibc_dat <- ibc_dat %>%
#   mutate(start_date = lubridate::dmy(start_date),
#          end_date = lubridate::dmy(end_date),
#          date = round_date(end_date, unit = "day")) %>%
#   mutate(yrmo = floor_date(end_date, unit = "month")) %>%
#   mutate(week = floor_date(end_date, unit = "week",
#                            week_start = getOption("lubridate.week.start", 7)))

# ##-----------------
# # Format Casualties         
# ibc_dat <- ibc_dat %>%
#   mutate(cas_mean = (reported_minimum + reported_maximum)/2) %>%
#   mutate(across(where(is.character), ~na_if(., "")))

# ##-----------------------
# # Format Provinces Column

## 18 governorates in Iraq
# baghdad <- c("Green Zone|Youssifiyah|Al Mada'in|Sadr City|Sadr|Bagjdad|Yousifiyah|Nahrawan|Abu Ghraib|baghdad|baghead|baghdaad|baghd|baghad|bagjhdad|bagdhad|bagdad|Yusufiya|latifiya|latifiyah|al-latifiya|Mahmoudiya|Mahmudiya|madaen|madain|al-Madaen|madean|Al-Madaen")
# ninewa <- c("Sinjar|Mosul|Ninewa|ninawa|nineveh|ninevah|ninwi|Tall Afar|Talafar|Telafar|Tel Afar|Tal-Afar|Tal Afar")
# anbar <- c("Rutba|Qaim|Garma|Haditha|Anbar|Hit|Heet|Ramadi|Fallujah|Falluja")
# saladin <- c("Sharqat|Dujail|Ishaqi|Dhuluiya|Shirqat|Tarmiya|Bayji|Beiji|Baaj|Beji|Baiji|Balad|taji|tuz|toz|Salah Al-Din|Salahuddin|Tikrit|Samarra")
# maysan <- c("Amara|Maysan")
# babil <- c("Jurf al-Sakhar|Jurf al-Sakhr|Iskandariya|Hilla|Babil|Mahaweel|Al-Mahaweel|Mahawil|Al-Mahawil|al-Mahaweel|Al-Mahaweel|Mussayab|Al-Mussayab|Al-Musayyib|al-Musayab|al-Musayyab|Musayyib|	
# Mussayib")
# diyala <- c("Abu Saida|Mandali|Hibhib|Khanaqin|Bani Saad|Buhruz|Ba'quba|Khalis|Al-Khalis|Jalawla|Khan Bani Saad|Diyala|Baquba|Baqubah|Ba'qubah|Baqouba|Baqbuah|Baqbuah|Baquabah|Buhriz|Muqdadiya|Balad Ruz")
# duhok <- c("Dohuk|Dahok|Duhuk|Zakho")
# qadissiya <-c("Qadissiya|Qadissiyah|Diwaniya")
# kerbala <- c("Kerbala|Karbala|Al Hindiyah")
# wassit <- c("Wassit|Kut|Suwayra|Numaniya")
# kirkuk <- c("Kirkuk|Hawija|Daquq")
# basra <- c("Basra|Al-Faw|Shat Al-Arab")
# muthanna <- c("Muthanna|Muthana|Rumaitha|Samawa")
# sulaymania <- c("Sulaymaniyah|Halabja|Sulaimaniyah")
# erbil <- c("Erbil|Irbil|Arbil")
# thiqar <- c("Thi Qar|Dhi Qar|Nasiriyah|Nassiriya")
# najaf <- c("Najaf|Shabaka|Al-Najaf|Kufa|Imman Ali Mosque")
# 
# ## Fill in the province IDs for Iraq's 18 provinces lised in the `location` column
# ibc_dat <- ibc_dat %>% 
#   filter(!is.na(location)) %>% # 4 events has no location listed
#   mutate(adm1_pcode = case_when(
#     str_detect(location, regex(anbar, ignore_case = TRUE)) ~ "IQG01",
#     str_detect(location, regex(basra, ignore_case = TRUE)) ~ "IQG02",
#     str_detect(location, regex(muthanna, ignore_case = TRUE)) ~ "IQG03",
#     str_detect(location, regex(najaf, ignore_case = TRUE)) ~ "IQG04",
#     str_detect(location, regex(qadissiya, ignore_case = TRUE)) ~ "IQG05",
#     str_detect(location, regex(sulaymania, ignore_case = TRUE)) ~ "IQG06",
#     str_detect(location, regex(babil, ignore_case = TRUE)) ~ "IQG07",
#     str_detect(location, regex(baghdad, ignore_case = TRUE)) ~ "IQG08",
#     str_detect(location, regex(duhok, ignore_case = TRUE)) ~ "IQG09",
#     str_detect(location, regex(diyala, ignore_case = TRUE)) ~ "IQG10",
#     str_detect(location, regex(erbil, ignore_case = TRUE)) ~ "IQG11",
#     str_detect(location, regex(kerbala, ignore_case = TRUE)) ~ "IQG12",
#     str_detect(location, regex(kirkuk, ignore_case = TRUE)) ~ "IQG13",
#     str_detect(location, regex(maysan, ignore_case = TRUE)) ~ "IQG14",
#     str_detect(location, regex(ninewa, ignore_case = TRUE)) ~ "IQG15",
#     str_detect(location, regex(saladin, ignore_case = TRUE)) ~ "IQG16",
#     str_detect(location, regex(thiqar, ignore_case = TRUE)) ~ "IQG17",
#     str_detect(location, regex(wassit, ignore_case = TRUE)) ~ "IQG18",
#     TRUE ~ as.character(NA)))
# 
# ##-----------------------
# # Merge missing provinces
# missing_provinces <- ibc_dat %>% # create a dataframe called missing_provinces
#   select(location, adm1_pcode) %>% # select the location and admin1 columns
#   filter(is.na(adm1_pcode)) %>% # filter out rows that have the value N/A
#   count(location) %>% # count each unique location value and insert the sums in a new column called `n`
#   arrange(-n) # arrange the dataframe by the counts of each location

##--------------
# Baghdad events

#st_geometry(irq_admin_units) <- NULL
baghdad_units <- irq_admin_units %>%
  filter(adm1_en == "Baghdad")

  # select(adm2_en, adm2_pcode, adm3_en, adm3_pcode) %>% 
  # filter(adm3_pcode %in% c("IQG08Q03N03", "IQG08Q06N01", "IQG08Q03N02", 
  #                          "IQG08Q02N01", "IQG08Q01N02", "IQG08Q01N01", 
  #                         "IQG08Q07N01", "IQG08Q06N02", "IQG08Q04N02",
  #                         "IQG08Q03N01"))
  
# pnts <- data.frame(
#   x = c(44.34395307667286), # Zafraniya, Dora, Sadr
#   y = c(33.2685453234678)
# )
# 
# tt1 <- baghdad_units
# 
# pnts$region <- apply(pnts, 1, function(row) {  
#   # transformation to palnar is required, since sf library assumes planar projection 
#   tt1_pl <- st_transform(tt1, 2163)   
#   coords <- as.data.frame(matrix(row, nrow = 1, 
#                                  dimnames = list("", c("x", "y"))))   
#   pnt_sf <- st_transform(st_sfc(st_point(row),crs = 4326), 2163)
#   # st_intersects with sparse = FALSE returns a logical matrix
#   # with rows corresponds to argument 1 (points) and 
#   # columns to argument 2 (polygons)
#   
#   tt1_pl[which(st_intersects(pnt_sf, tt1_pl, sparse = FALSE)), ]$adm3_en 
# })
#   

sadr <- c("Sadr|Sadr City| Al Thawra")
karkh <- c("Karkh|Mansour|Ghazaliya|Green Zone|Bayaa")
rusafa <- c("Karrada|Rusafa|Zafaraniya|Al-Zafaraniyah|al-Zafaraniya|Zafraniyah|Zafraniya|Zafaraniyeh|Zafraniya|New Baghdad|Husseiniya")
adhamiya <- c("Adhamiya|Adhamiyah|Al-Adhamiya|Shaab")
kadhimiya <- c("Taji|kadhimiya|Kadhamiyah|Al-Kadhimiya|Abu Ghraib")
mahmoudiya <- c("Mahmoudiya|Mahmudiya|Amil|dora|dura|doura|Saidiyah|Jihad")
madain <- c("madaen|madain|al-Madaen|madean|Al-Madaen|Al Mada'in|Yusufiya")


ibc_baghdad <- ibc_dat %>% 
  filter(adm1_pcode == "IQG08") %>% 
  mutate(adm2_pcode = NA, adm3_pcode = NA) %>% 
  #separate(location, c("loc1", "loc2", "loc3", "loc4"), sep = "\\,", 
  #         remove = FALSE) %>% 
  filter(!str_detect(location, "Maysan")) %>% 
  mutate(adm2_pcode = case_when(
    str_detect(location, regex(sadr, ignore_case = TRUE)) ~ "IQG08Q07",
    str_detect(location, regex(karkh, ignore_case = TRUE)) ~ "IQG08Q03",
    str_detect(location, regex(adhamiya, ignore_case = TRUE)) ~ "IQG08Q01",
    str_detect(location, regex(rusafa, ignore_case = TRUE)) ~ "IQG08Q06",
    str_detect(location, regex(madain, ignore_case = TRUE)) ~ "IQG08Q04",
    str_detect(location, regex(mahmoudiya, ignore_case = TRUE)) ~ "IQG08Q05",
    str_detect(location, regex(kadhimiya, ignore_case = TRUE)) ~ "IQG08Q02",
    TRUE ~ as.character(NA))) 

  # mutate(adm3_pcode = case_when(
  #   str_detect(location, regex("Abu Ghraib", ignore_case = TRUE)) ~ "IQG08Q02N02",
  #   str_detect(location, regex("dora|dura|doura", ignore_case = TRUE)) ~ "IQG08Q05N02",
  #   str_detect(location, regex("|Zafaraniya|Al-Zafaraniyah|al-Zafaraniya|Zafraniyah|Zafraniya|Zafaraniyeh|Zafraniya", 
  #                              ignore_case = TRUE)) ~ "IQG08Q06N01",
  #   TRUE ~ as.character(NA)))

missing_baghdad <- ibc_baghdad %>% # create a dataframe called missing_provinces
  select(location, adm2_pcode) %>% # select the location and admin1 columns
  filter(is.na(adm2_pcode)) %>% # filter out rows that have the value N/A
  count(location) %>% # count each unique location value and insert the sums in a new column called `n`
  arrange(-n) # arrange the dataframe by the counts of each location


##---------
# Save data
save(ibc_dat, irq_admin_units, missing_provinces, baghdad_units, ibc_baghdad,
     missing_baghdad,
     file = "~/Dropbox/Archive/book_project/data/iraq_body_count/ibc_dat.RData")

rm(list = ls())
##=============================================================================
## End of File
##=============================================================================
