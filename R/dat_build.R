library(tbeptools)
library(dplyr)

# import and save epcdata -------------------------------------------------
  
# local file path
xlsx <- 'epcdata.xls'

# import data
epcdata <- read_importwq(xlsx, download_latest = T) %>%
  mutate(
    sd_q = case_when(
      is.na(sd_q) ~ T,
      !is.na(sd_q) ~ F
    )
  )

save(epcdata, file = 'data/epcdata.RData', version = 2)

file.remove(xlsx)

# algae data --------------------------------------------------------------

# file path
xlsx <- 'phyto_data.xlsx'

# load and assign to object
algdat <- read_importphyto(xlsx, download_latest = T) 

save(algdat, file = 'data/algdat.RData', version = 2)

file.remove(xlsx)