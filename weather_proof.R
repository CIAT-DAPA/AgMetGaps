# =-=-=-= Weather data new code... 
# by: Alejandra Esquivel 
# August 16 - 2018
# ............................................
# ............................................
# ............................................


# =-=-=-= This part is work about weather data. 


############### packages
library(tidyverse)
library(raster)
library(rgdal)
library(ncdf4)
library(rgeos)
library(sf) # (-.-)
library(data.table)
library(fst)


# =-=-=-= routes

rootPath <- '/mnt/workspace_cluster_9/AgMetGaps/'
calendarPath <-  '/mnt/workspace_cluster_9/AgMetGaps/0_general_inputs/calendar_5min/'
weatherPath <- '/mnt/workspace_cluster_9/AgMetGaps/weather_analysis/precipitation_points/weather_stations/'
shpPath <- paste0(rootPath, '0_general_inputs/shp/')


# =-=-=-=-= Crop - i should incluide in this part wheat winter. 
crop <- 'Maize'
season <- 'Maize.crop'


# =-=-=-=-= Considering a sowing window ----  with grilled data

# =-=-=-=-= Calendar

# start planting date
plant.start <- list.files(paste0(calendarPath), pattern = glob2rx(paste0('*',season,'*.nc$')), full.names = TRUE ) %>%
  raster::raster(varname = 'plant.start' ) %>% raster::crop(extent(-180, 180, -50, 50))
# end planting date 
plant.end<- list.files(paste0(calendarPath), pattern = glob2rx(paste0('*',season,'*.nc$')), full.names = TRUE ) %>%
  raster::raster(varname = 'plant.end' ) %>% raster::crop(extent(-180, 180, -50, 50))

# Calendar: window planting dates
calendar <-  stack(plant.start, plant.end) 

rm(plant.start, plant.end)





# =-=-=-=-= Read precipitation file with all information
all_crop_data <- paste0(weatherPath, crop, '.fst') %>% 
  read_fst(as.data.table = TRUE) %>%
  as.tibble()

gdata::humanReadable(object.size(all_crop_data), unit="MiB") # that is for see the table size. 
# For Maize: 2532.1 MiB
all_crop_data 

coord <-  all_crop_data %>% 
  dplyr::select(long, lat) %>% 
  data.frame()


points_calendar <- raster::extract(calendar, coord) %>% 
  as_tibble() %>% 
  setNames(c('start_planting', 'end_panting'))

rm(coord)


# =-=-=-=-= Re-order all dataset + id 

tictoc::tic('gather')
tidy_climate <- all_crop_data %>%
  bind_cols(points_calendar, .) %>% 
  gather(date_raster, precip, -id, -lat, -long, -start_planting, -end_panting) %>%
  mutate(year = lubridate::year(date_raster),
         julian = lubridate::yday(date_raster))
tictoc::toc() # 30.84202


gdata::humanReadable(object.size(tidy_climate), unit="MiB") # that is for see the table size. 
# For Maize: 21501.0 MiB
rm(all_crop_data, points_calendar)


tidy_climate <-  tidy_climate %>% 
  na.omit()

tictoc::tic('split')
full_data <- split(tidy_climate, list(tidy_climate$id))
tictoc::toc() # 1.522667


# tidy_climate <- full_data%>% bind_rows() %>% na.omit()

gdata::humanReadable(object.size(full_data), unit="MiB") # that is for see the table size. 
# For Maize: 38783.3 MiB
# rm(tidy_climate)


# full_data[[1]] %>% 
#   dplyr::select(year) %>% 
#   table

# =-=-=-=-= filter data only for the planting window. 
# .x <- full_data[[1]]
filterD <- function(.x){
  
  Time <- .x %>% 
    dplyr::select(start_planting, end_panting) %>% 
    unique  
  
  a <- as.numeric(Time[1]);  b <- as.numeric(Time[2])
  
  if(a < b){
    
    post <- a:b 
    data <- .x %>% 
      dplyr::filter(julian %in% post) %>% 
      filter(year <2012)
    
  } else if(a > b){
    
    data <-.x %>% 
      filter(year < 2013) %>% 
      #mutate(start = a, end = b) %>%
      filter( julian <= b| a <= julian ) %>%
      mutate(year = ifelse(julian <= b, year -1 , year)) %>% 
      filter(year != 1980 & year != 2012)
  }
  
  return(data)
}




# library(future)
# library(future.apply)
# library(tictoc)
# #options(future.globals.maxSize= 8912896000)
# plan(sequential)
# plan(future::multisession, workers = 10)
# 
# tic("future lapply test")
# full_data <- future_lapply(X = full_data, FUN = filterD)
# toc()



tictoc::tic(filter)
full <- full_data %>% 
  lapply(. , filterD)
tictoc::toc() # 14.87563




