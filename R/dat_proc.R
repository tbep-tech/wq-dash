library(tidyverse)
library(readxl)
library(lubridate)
library(tbeptools)
library(plotly)
library(extrafont)
library(leaflet)

source('R/funcs.R')

loadfonts(device = 'pdf', quiet = T)
if(Sys.info()[1] == 'Windows')
  loadfonts(device = 'win', quiet = T)

maxyr <- 2019
fml <- "Lato Light"

# minor theme tweaks
pthm <- theme(
  axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
  legend.text = element_text(size = 12), 
  axis.title.y = element_text(size = 12),
  text = element_text(fml), 
  legend.position = 'top',
  # panel.grid.minor=element_blank(),
  # panel.grid.major=element_blank(),
  panel.background = element_rect(fill = '#ECECEC')
) 

# plotly secondary axix
ax <- list(
  tickfont = list(size=14),
  overlaying = "x",
  nticks = 4,
  side = "top"
)
seg <- c('OTB', 'HB', 'MTB', 'LTB') 
nms <- factor(seg, levels = seg)

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
    yrqrt = floor_date(Date, unit = 'quarter'),
    yr = year(Date), 
    mo = month(Date, label = T)
  ) 

save(algdat, file = here::here('data', 'algdat.RData'), compress = 'xz')

# graphics for dash -------------------------------------------------------

# attainment matrix
plo <- show_matrix(epcdata, yrrng = c(1975, maxyr), family = fml, txtsz = NULL) + 
  theme(
    axis.text = element_text(size = 12),
    text = element_text(family = fml),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  )
attmat <- ggplotly(plo, tooltip = 'Action') %>%
  add_bars(x = nms,y = c(1, 1, 1, 1), xaxis = "x2", inherit = F) %>%
  layout(xaxis2 = ax)

# chl matrix
plo <- show_wqmatrix(epcdata, param = 'chla', yrrng = c(1975, maxyr), family = fml, txtsz = NULL) + 
  theme(
    axis.text = element_text(size = 12), 
    text = element_text(family = fml),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) 
chlmat <- ggplotly(plo, tooltip = 'Result') %>%
  add_bars(x = nms,y = c(1, 1, 1, 1), xaxis = "x2", inherit = F) %>%
  layout(xaxis2 = ax)
  
# la matrix
plo <- show_wqmatrix(epcdata, param = 'la', yrrng = c(1975, maxyr), family = fml, txtsz = NULL) + 
  theme(
    axis.text = element_text(size = 12), 
    text = element_text(family = fml),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) 
lamat <- ggplotly(plo, tooltip = 'Result') %>%
  add_bars(x = nms,y = c(1, 1, 1, 1), xaxis = "x2", inherit = F) %>%
  layout(xaxis2 = ax)

# threshold plots
thrplototb <- thrplotly(epcdata, 'OTB', maxyr, fml, pthm)
thrplothb <- thrplotly(epcdata, 'HB', maxyr, fml, pthm)
thrplotmtb <- thrplotly(epcdata, 'MTB', maxyr, fml, pthm)
thrplotltb <- thrplotly(epcdata, 'LTB', maxyr, fml, pthm)

save(thrplototb, thrplothb, thrplotmtb, thrplotltb, attmat, chlmat, lamat, file = 'data/plotlys.RData', compress = 'xz')


