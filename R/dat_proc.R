library(tidyverse)
library(readxl)
library(lubridate)
library(tbeptools)
# devtools::load_all("../tbeptools")
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

# local file path
xlsx <- 'C:/Users/Marcus/Desktop/epcdata.xls'

# import data
epcdata <- read_importwq(xlsx, download_latest_epchc = T) %>%
  filter(yr <= 2019) %>% 
  mutate(
    sd_q = case_when(
      is.na(sd_q) ~ T, 
      !is.na(sd_q) ~ F
    )
  )

# # local file path
# xlsx <- 'T:/03_BOARDS_COMMITTEES/05_TBNMC/BAY_TARGETS/2019_update/data-raw/epchc.xlsx'
# 
# # import and download if new
# epcdata <- read_importwq(xlsx, download_latest_epchc = F, tryurl = T, connecttimeout = 20)
# 
save(epcdata, file = 'data/epcdata.RData', compress = 'xz')

# # algae data --------------------------------------------------------------
# 
# # file path
# xlsx <- 'C:/Users/Marcus/Desktop/phyto_data.xlsx'
# 
# # load and assign to object
# algdat <- read_importphyto(xlsx, download_latest_epchc = T, tryurl = T) %>% 
#   filter(yr < 2020)
# 
# save(algdat, file = here::here('data', 'algdat.RData'), compress = 'xz')

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
  layout(xaxis2 = ax) %>% 
  config(displayModeBar = FALSE)

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
  layout(xaxis2 = ax) %>% 
  config(displayModeBar = FALSE)
  
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
  layout(xaxis2 = ax) %>% 
  config(displayModeBar = FALSE)

# threshold plots
thrplototb <- show_segplotly(epcdata, 'OTB', c(1975, maxyr), fml)
thrplothb <- show_segplotly(epcdata, 'HB', c(1975, maxyr), fml)
thrplotmtb <- show_segplotly(epcdata, 'MTB', c(1975, maxyr), fml)
thrplotltb <- show_segplotly(epcdata, 'LTB', c(1975, maxyr), fml)

save(thrplototb, thrplothb, thrplotmtb, thrplotltb, attmat, chlmat, lamat, file = 'data/plotlys.RData', compress = 'xz')

# site data for map -------------------------------------------------------

load(file = 'data/epcdata.RData')

# data to map
mapdat <- tibble(thr = c('chla', 'la')) %>% 
  crossing(epcdata) %>% 
  group_by(thr) %>% 
  nest %>% 
  mutate(
    dat = purrr::pmap(list(data, thr), function(data, thr){
      
      out <- data %>% 
        anlz_avedatsite %>% 
        anlz_attainsite(thr = thr)
      
      return(out)
      
    })
  ) %>% 
  select(-data) %>% 
  unnest(dat) %>% 
  select(-met) %>% 
  ungroup()

save(mapdat, file = 'data/mapdat.RData', compress = 'xz')

# matrix data for map -----------------------------------------------------

avedat <- epcdata %>% 
  anlz_avedat %>% 
  anlz_attain %>% 
  mutate(
    action = case_when(
      outcome == 'green' ~ 'Stay the Course', 
      outcome == 'yellow' ~ 'Caution', 
      outcome == 'red' ~ 'On Alert'
    ), 
    outcome = case_when(
      outcome == 'green' ~ 'lightgreen', 
      T ~ outcome
    )
  )

save(avedat, file = 'data/avedat.RData', compress = 'xz')
