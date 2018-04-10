# extrem values on precipitation
library(fst)
library(dplyr)
library(data.table)
library(tidyr)
# path <- '//dapadfs/Workspace_cluster_9/AgMetGaps/4_daily_precipitation/weather_analysis/precipitation_points/weather_stations/'
path <- '/mnt/workspace_cluster_9/AgMetGaps/4_daily_precipitation/weather_analysis/precipitation_points/weather_stations/'
type_crop <- 'Maize.fst'

x <- fst::read_fst(paste0(path, type_crop), as.data.table = T) %>%
  as_tibble()


x <- x %>%
  group_by(id, lat, long) %>%
  nest()

plan(sequential)
plan(multisession, workers = availableCores() - 3)
x <- x %>%
  mutate(climate = purrr::map(.x = data, .f = ~future(gather(.x, date, precip)))) %>%
  mutate(climate = purrr::map(.x = data, .f = ~value(gather(.x, date, precip))))

