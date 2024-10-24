knitr::opts_chunk$set(echo = F, message = F, warning = F)

box::use(
  dplyr[`%>%`]
)

source(here::here('R/funcs.R'))

# load('data/epcdata.RData')
load(url('https://tbep-tech.github.io/wq-dash/data/epcdata.RData'))
load(url('https://tbep-tech.github.io/wq-dash/data/algdat.RData'))
load(here::here('data/avedat.RData'))
load(here::here('data/mapdat.RData'))
load(here::here('data/plotlys.RData')) # thrplototb, thrplothb, thrplotmtb, thrplotltb, attmat, chlmat, lamat

prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
maxyr <- 2023

# minor theme tweaks
pthm <- ggplot2::theme(
  axis.text.x = ggplot2::element_text(size = 11, angle = 45, hjust = 1),
  legend.text = ggplot2::element_text(size = 12), 
  axis.title.y = ggplot2::element_text(size = 12),
  legend.position = 'top',
  panel.background = ggplot2::element_rect(fill = '#ECECEC')
) 

# locations
locs <- epcdata %>% 
  dplyr::select(epchc_station, Longitude, Latitude) %>% 
  unique %>% 
  sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)

# algae names and colors
algnms <- c('Bacillariophyta', 'Cyanobacteria', 'Karenia brevis', 'Pseudo-nitzschia pungens', 'Pseudo-nitzschia sp.', 'Pyrodinium bahamense', 'Tripos hircus', 'other')
cols <- pal_alg(algnms)
names(cols) <- algnms
