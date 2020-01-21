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