library(tidyverse)
library(readxl)
library(lubridate)

# import and save epcdata -------------------------------------------------

# # local file path
# xlsx <- here('data-raw', '2018_Results_Updated.xls')
# 
# # import data
# epcdata <- read_importwq(xlsx, download_latest_epchc = F)

# local file path
xlsx <- 'T:/03_BOARDS_COMMITTEES/05_TBNMC/BAY_TARGETS/2019_update/data-raw/epchc.xlsx'

# import and download if new
epcdata <- read_importwq(xlsx, download_latest_epchc = F, tryurl = T, connecttimeout = 20)

save(epcdata, file = 'data/epcdata.RData', compress = 'xz')

# algae data --------------------------------------------------------------

data(epcdata)
locs <- epcdata %>% 
  select(epchc_station, Longitude, Latitude) %>% 
  unique

algraw <- read_excel('C:/users/Marcus.SCCWRP2K/Desktop/PlanktonDataList_ThroughCurrentReportMonth.xlsx')

algdat <- algraw %>% 
  select(epchc_station = StationNumber, Date = SampleTime, phylum = PHYLUM, name = NAME, count = COUNT, units = Units) %>% 
  filter(epchc_station %in% locs$epchc_station) %>% 
  mutate(
    Date = as.Date(Date),
    # qrt = quarter(Date, with_year = TRUE),
    name = case_when(
      name %in% c('Pyrodinium bahamense', 'Karenia brevis', 'Tripos hircus', 'Pseudo-nitzschia sp.', 'Pseudo-nitzschia pungens') ~ name, 
      phylum %in% c('Bacillariophyta', 'Cyanobacteria') ~ phylum, 
      !is.na(phylum) ~ 'other', 
      T ~ NA_character_
    )
  ) %>% 
  filter(!is.na(name)) %>% 
  group_by(epchc_station, Date, name, units) %>% 
  summarise(count = sum(count, na.rm = T)) %>% 
  ungroup %>% 
  mutate(
    yrqrt = floor_date(Date, unit = 'quarter')
  ) 

save(algdat, file = here::here('data', 'algdat.RData'), compress = 'xz')
