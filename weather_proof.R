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





# =-=-=-=-=-=-= August 17 - 2018

gdata::humanReadable(object.size(full), unit="MiB") # that is for see the table size. 
# For Maize: 6302.7 MiB

tictoc::tic(bind_rows)
full <- bind_rows(full)
tictoc::toc() 


gdata::humanReadable(object.size(full), unit="MiB") # that is for see the table size. 
# For Maize: 3433.2 MiB


# =-=-=   descriptive analysis 


# with this graph we prove if exist changes years in our sowing windows.
full %>% 
  dplyr::select(id, start_planting, end_panting) %>% 
  mutate(condition = ifelse(start_planting > end_panting, 'yes', 'no'), dif = end_panting - start_planting) %>%
  count(dif) %>% 
  ggplot(aes(as.factor(dif),  n)) + geom_bar(stat = 'identity') + 
  theme_bw()



# full to the data in the sowing windows.
full %>% 
  # dplyr::select(id, start_planting, end_panting, year) %>% 
  filter(year == 2011) %>%
  nest(-id) %>% 
  mutate(leng = purrr::map(.x = data, .f = nrow)) %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  ggplot(aes(as.factor(leng))) + geom_bar() + 
  theme_bw()






# Analisys to dry and wet days

# =-=-=-=-= This function count number of dry and wet days.
number_of_days <- function(.x){
  summ <- .x %>% 
    summarise(number_dry = sum(dry_days), number_wet = sum(wet_days))
  return(summ)}



# =-=-=-=-= It make a variable to clasificate a day dry or wet and after count the wet and dry days.
tictoc::tic('dry_wet_days')
test_days <- full %>% 
  filter(year < 2012) %>% # AgMetGaps data is only available until 2011
  mutate(dry_days = ifelse(precip == 0, 1, 0), wet_days = ifelse(precip > 0, 1, 0) ) %>% 
  nest(-id, -year, -long, -lat) %>% 
  mutate(number_days = purrr::map(.x = data, .f = number_of_days)) %>% 
  mutate(total_rows = purrr::map(.x = data, .f = nrow)) %>% 
  mutate(probability = purrr::map2(.x = number_days, .y = total_rows, .f = function(.x,.y){.x / as.numeric(.y)})) 
tictoc::toc() # 14.94127



# test_days <- test_days %>% 
#   dplyr::select(-data) %>% 
#   unnest %>% 
#   rename(probability_dry = 'number_dry1', probability_wet = 'number_wet1')


# =-=-=-= Plot to the frecuency of the days dry or wet...
test_days %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  rename(probability_dry = 'number_dry1', probability_wet = 'number_wet1')%>%
  group_by(id, lat, long) %>% 
  summarise(prob_dry_mean = mean(probability_dry), prob_wet_mean = mean(probability_wet)) %>%
  gather(type, prob, -id, -lat, -long) %>% 
  ggplot(aes(x = long , y = lat, fill = prob)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed() +
  facet_grid(~type) + 
  labs(x = 'Longitude', y = 'Latitude', fill = 'Frecuency') + 
  theme_bw() +
  theme(legend.position = 'bottom')



# gridExtra::grid.arrange(a1,a2, ncol = 2)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
# =-=-=-=  Find the ocurrences is not necesary in this time... but... 

# .x <- test_days %>% filter(id == 1, year == 2011) 

# =-=-=-=-=  With this function we count the number of consecutive days dry or wet. 
find_ocurrences <- function(.x, k){
  
  dataset.w <- setDT(.x)[, counter := seq_len(.N), by=rleid(wet_days)] %>% 
    filter(wet_days == 1) %>% 
    summarise(count = sum(counter == k)) %>% 
    cbind(type = 'wet_days', .)
  
  dataset.d <- setDT(.x)[, counter := seq_len(.N), by=rleid( dry_days)] %>% 
    filter(dry_days == 1) %>% 
    summarise(count = sum(counter >= k)) %>% 
    cbind(type = 'dry_days', .)
  
  
  dataset <- bind_rows(dataset.w, dataset.d) %>% as.tibble
  
  return(dataset)}


# August 21-2018


# In this part, count the number of the consecutive days dry and wet, in k days (for this case is 4).
# tictoc::tic()
# kc_days <- test_days %>% 
#   mutate(ocurrences = purrr::map(.x = data, 
#                                  .f = find_ocurrences, k = 4)) %>%
#   dplyr::select(id, long, lat,  year, ocurrences) %>% unnest
# tictoc::toc() #  1.839903 Horas
# 
# 




library(future)
library(future.apply)
library(tictoc)
#options(future.globals.maxSize= 8912896000)
plan(sequential)
plan(future::multisession, workers = 10)



list_days <- test_days %>% #filter(id %in% 1:10) %>% 
  select(id, lat, long, year, data) %>% 
  unnest() %>% 
  split(. , list(.$id, .$year))

tic("future lapply test")
data_test <- future_lapply(X = list_days, FUN = find_ocurrences, k = 3)
toc() #   17.11175




# try_data <- 


data_test1 <- data_test %>%
  bind_rows()  %>%
  mutate(id = rep(1:(nrow(.)/2),  each = 2)) %>% 
  nest(-id) %>% 
  dplyr::select(-id)

names_data_test <- data_test %>% 
  names %>% 
  tibble %>% 
  rename(id_year = '.') %>% 
  separate(id_year, c('id', 'year')) 


# 
data_test <- bind_cols(names_data_test, data_test1) 

rm(names_data_test, data_test1)



data_test <- data_test%>% 
  mutate(id = as.numeric(id), year = as.numeric(year))


data_test <- test_days %>% 
  dplyr::select(id, lat, long, year) %>% 
  distinct() %>% 
  left_join(., data_test, by = c('id', 'year')) %>% 
  unnest









# 772,329
