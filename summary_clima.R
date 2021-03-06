
# =-=-=-=-= Summary variables 
# Script Date: (5-6)/Jun/2018

# =-=-=-= Packages

library(tidyverse)
library(raster)
library(rgdal)
library(ncdf4)
library(rgeos)
library(sf)
library(data.table)
library(fst)


# =-=-=-= routes

rootPath <- '/mnt/workspace_cluster_9/AgMetGaps/'
climatePath <- '/mnt/workspace_cluster_9/AgMetGaps/3_monthly_climate_variability/katia_climate/'
IizumiPath <- '/mnt/workspace_cluster_9/AgMetGaps/1_crop_gaps/iizumi_processed/'
out_path <- '/mnt/workspace_cluster_9/AgMetGaps/3_monthly_climate_variability/summary/'


# Crop 
crop <- 'Wheat'  ## switch between: 'Maize', 'Rice', 'Wheat'
seasonCrop <- 'wheat_spring' ## switch between: 'maize_major', 'rice_major', 'wheat_spring'


out_path <- paste0(out_path, crop, '/')

# create folder
if(dir.exists(out_path) == FALSE){dir.create(out_path)}


# functions 
n_bands <- length(1981:2011)

read_bands <- function(x, n_bands){
  
  stack(x, bands = 1:n_bands) %>%
    flip(direction = 2) %>%
    t()
  
}
mean_sd <- function(stack_var){
  points <- stack_var %>% rasterToPoints %>% as.tibble() %>% mutate(id = 1:length(x)) 
  
  coord <- points %>% dplyr::select(id, x, y)
  
  ind <- points %>% 
    gather(year, value, -x, -y, -id) %>% na.omit() %>% 
    #separate(year, c('year1', 'other'), sep = '_detrending', remove = TRUE) %>% 
    #dplyr::select(-other) %>% 
    group_by(id) %>% 
    summarise(mean = mean(value), sd = sd(value)) %>% 
    inner_join(coord, . , by = 'id') # %>% ggplot(aes(x, y, fill =  sd)) + geom_tile() + coord_fixed() + theme_bw()
}
write_files <- function(type, stack_var){
  
  ind <- stack_var %>% mean_sd
  write.csv(ind,  paste0(out_path, type, '_ind.csv'), row.names = FALSE)
  
  rasterize(ind %>% dplyr::select(x, y), stack_var[[1]], ind %>% dplyr::select(sd)) %>%
    writeRaster(x = ., filename = paste0(out_path, type, '_sd.tif') )
  
  rasterize(ind %>% dplyr::select(x, y), stack_var[[1]], ind %>% dplyr::select(mean)) %>%
    writeRaster(x = ., filename = paste0(out_path, type, '_mean.tif') )
  
  return(ind)}


# ind %>%  ggplot(aes(x, y, fill =  mean)) + geom_tile() + coord_fixed() + theme_bw()



#gap_iizumi %>% write_files(type = 'gap_D', stack_var = .)



# Climate

# read climate data 
trimester_files <- list.files(paste0(climatePath, crop, '_DetPrecip&Temp'), full.names = T)


extract_trimestre <- function(x){
  # list.files(paste0(climatePath, crop), full.names=T)
  
  x %>%
    basename() %>% 
    stringr::str_replace(pattern = "[0-9]+.nc$", replacement = '')
}


type <- trimester_files %>%
  data_frame(path = .) %>%
  mutate(type = basename(path),
         trimestre = extract_trimestre(path)) %>%
  dplyr::select(trimestre, everything()) %>%
  pull(trimestre)





proof <- tibble(trimester_files , type) %>% 
  mutate(stack_var = purrr::map(.x = trimester_files, .f = read_bands, n_bands = n_bands)) %>% 
  mutate(Ind = purrr::map2(.x = type, .y = stack_var, .f = write_files) )




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


new_files <- paste0(IizumiPath, seasonCrop, '/yield_', 1981:2011, '_detrending_detrending_gap.tif')

# list.files(path = paste0(IizumiPath, seasonCrop), pattern = 'detrending_detrending_gap.tif', full.names = TRUE)
gap_iizumi <- new_files %>% 
  raster::stack() %>% 
  raster::crop(extent(-180, 180, -50, 50))


gap <- write_files(type = 'GapDet', stack_var = gap_iizumi)

# =-=-=-=

yield_files <- paste0(IizumiPath, seasonCrop, '/yield_', 1981:2011, '_detrending.tif')

# list.files(path = paste0(IizumiPath, seasonCrop), pattern = 'detrending_detrending_gap.tif', full.names = TRUE)
yield_iizumi <- yield_files %>% 
  raster::stack() %>% 
  raster::crop(extent(-180, 180, -50, 50))

yield <- write_files(type = 'YieldDet', stack_var = yield_iizumi)













######
######
######
# =-=-=-=-=-=-= GROC data



GROC_path <- '/mnt/workspace_cluster_9/AgMetGaps/3_monthly_climate_variability/GROC/'

GROC_files <- list.files(paste0(GROC_path, crop), full.names = TRUE) %>% 
  stack() 


namesG <- list.files(paste0(GROC_path, crop), full.names = TRUE) %>% 
  basename() 

for(i in 1:6){
  GROC_files[[i]][which(GROC_files[[i]][] <= 0.5)] = NA
  writeRaster(GROC_files[[i]], paste0(out_path,  namesG)[i])  
}


rm(list = ls())
gc()



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
library(sf)  # (-.-)
library(data.table)
library(fst)

# =-=-=-= routes

rootPath <- '/mnt/workspace_cluster_9/AgMetGaps/'
calendarPath <-  '/mnt/workspace_cluster_9/AgMetGaps/0_general_inputs/calendar_5min/'
weatherPath <- '/mnt/workspace_cluster_9/AgMetGaps/weather_analysis/precipitation_points/weather_stations/'
shpPath <- paste0(rootPath, '0_general_inputs/shp/')

#out_path <- #


shp_colombia <- st_read(dsn = paste0(shpPath, 'all_countries.shp')) %>%
  as('Spatial') %>% crop(extent(-180, 180, -50, 50)) 




# =-=-=-=-= Crop
crop <- 'Wheat'
season <- 'Wheat.Winter'
# crop


# =-=-=-=-= Calendar Analysis

# start planting date

plant_start <- list.files(calendarPath, pattern = glob2rx(paste0('*',season,'*.nc$')), full.names = TRUE ) %>%
  raster::raster(varname = 'plant.start' ) %>%
  raster::crop(extent(-180, 180, -50, 50))

# end planting date 
plant_end<- list.files(paste0(calendarPath), pattern = glob2rx(paste0('*',season,'*.nc$')), full.names = TRUE ) %>%
  raster::raster(varname = 'plant.end' ) %>% 
  raster::crop(extent(-180, 180, -50, 50))

# Calendar: window planting dates
calendar <-  stack(plant_start, plant_end) 


# =-=-=-=-= Read precipitation file with all information
all_crop_data <- paste0(weatherPath, crop, '.fst') %>% 
  read_fst(as.data.table = TRUE) %>%
  as.tibble()


# =-=-=-=-= Re-order all dataset + id 
# test
tidy_climate <- all_crop_data %>% 
  gather(date_raster, precip, -id, -lat, -long) %>% 
  mutate(year = lubridate::year(date_raster), 
         julian = lubridate::yday(date_raster))  %>% 
  nest(-id, -lat, -long)


# =-=-=-=-= Coordenates for each point 

coords <- tidy_climate %>% 
  dplyr::select(long, lat)

points_calendar <- extract(calendar, coords) %>%
  as.tibble
# add the coordenates to all data
tidy_climate <- bind_cols(tidy_climate, points_calendar)


# =-=-=-=-= Create tibble --- (for use in each row).

tibbleE <- function(.x, .y){
  
  date <-  data.frame(start = .x, end = .y)
  return(date)
  }


# =-=-=-=-= filter data only for the planting window. 

filterD <- function(.x, .y){
  
  # [primero que todo... crear las condiciones para el if... dentro de esta funcion... ]  
  
  a <- as.numeric(.y[1]);  b <- as.numeric(.y[2])
  
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



# =-=-=-=-= omit NA data, by operate with it is imposible create a new data set with 
#           only precipitation in a window planting.
tidy_climate <- tidy_climate %>%
  na.omit %>% 
  rename(start = Wheat.Winter..Start.of.planting, end = Wheat.Winter..End.of.planting) %>% 
  #mutate(condition = ifelse(start > end, 'yes', 'no'), dif = end - start) %>% 
  mutate(yes = map2(.x = start, .y = end, .f = tibbleE)) %>%
  #dplyr::select(-start, -end) %>% 
  mutate(data.F = map2(.x = data, .y = yes, .f = filterD )) 




#tidy_climate %>% 
#    mutate(condition = ifelse(start > end, 'yes', 'no')) %>% 
#    filter( condition == 'yes') %>% 
#    filter(row_number() == 1) %>% 
#    dplyr::select(start, data.F) %>% 
#    unnest %>% 
#    filter(year == 2010) %>% View

# In this graph we know the difference between window dates
tidy_climate %>% 
  dplyr::select(id, start, end) %>% 
  mutate(condition = ifelse(start > end, 'yes', 'no'), dif = end - start) %>%
  #filter(condition == 'yes')
  count(dif) %>% 
  ggplot(aes(as.factor(dif),  n)) + geom_bar(stat = 'identity') + 
  theme_bw()


# length to the data in the window sowing 
tidy_climate %>% 
  dplyr::select(id, data.F) %>% 
  unnest %>% 
  filter(year == 2011) %>%
  nest(-id) %>% 
  mutate(leng = purrr::map(.x = data, .f = nrow)) %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  ggplot(aes(as.factor(leng))) + geom_bar() + 
  theme_bw()






# =-=-=-=-= This function count number of dry and wet days.
number_of_days <- function(.x){
  summ <- .x %>% 
    summarise(number_dry = sum(dry_days), number_wet = sum(wet_days))
  return(summ)
  }




# =-=-=-=-= It make a variable to clasificate a day dry or wet and after count the wet and dry days. 
test_days <- tidy_climate %>% 
  dplyr::select(id, lat, long,  data.F) %>% 
  unnest %>% filter(year < 2012) %>% 
  mutate(dry_days = ifelse(precip == 0, 1, 0), wet_days = ifelse(precip == 0, 0, 1) ) %>% 
  nest(-id, -year, -long, -lat) %>% #filter(row_number() == 1 )%>%
  mutate(number_days = purrr::map(.x = data, .f = number_of_days)) %>% 
  mutate(total_rows = purrr::map(.x = data, .f = nrow)) %>% 
  mutate(probability = purrr::map2(.x = number_days, .y = total_rows, .f = function(.x,.y){.x / as.numeric(.y)})) 



# This map is for represent number with for each year, and is only for describe the situation.
test_days %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  filter(year %in% 2008:2011) %>%
  ggplot(aes(long, lat, fill = number_wet)) + geom_tile() +
  scale_fill_gradientn(colours = rainbow(5)) +  
  geom_polygon(data = shp_colombia, aes(x=long, y = lat, group = group), color = "gray30", fill=NA) + 
  facet_wrap(~year, ncol = 2) + coord_fixed() +
  theme_bw()

# Here we calculate freq mean of dry and wet days... 
freq_mean_days <- test_days %>% 
  dplyr::select(id, lat, long, year, probability) %>% 
  unnest() %>% 
  group_by(id, lat,long) %>%
  summarise(dry_mean = mean(number_dry) * 100, wet_mean = mean(number_wet) * 100) 



# graphs 
a1 <- freq_mean_days %>% 
  ggplot(aes(x = long, y = lat, fill =  wet_mean)) + 
  geom_raster() + scale_fill_gradientn(colours = rev(heat.colors(10))) + 
  geom_polygon(data = shp_colombia, aes(x=long, y = lat, group = group), color = "gray30", fill=NA)  +
  coord_fixed() +
  theme_bw() + 
  theme(legend.position = 'bottom')


a2 <- freq_mean_days %>% 
  ggplot(aes(x = long, y = lat, fill =  dry_mean)) + 
  geom_raster() + scale_fill_gradientn(colours = rev(heat.colors(10))) + 
  geom_polygon(data = shp_colombia, aes(x=long, y = lat, group = group), color = "gray30", fill=NA)  +
  coord_fixed() +
  theme_bw() + 
  theme(legend.position = 'bottom')

gridExtra::grid.arrange(a1,a2, ncol = 2)






##### From here all script is a proof about different emphasis.

# =-=-=-=-=  With this function we count the number of consecutive days dry or wet. 
find_ocurrences <- function(.x, k){
  
  dataset.w <- setDT(.x)[, counter := seq_len(.N), by=rleid(wet_days)] %>% 
    filter(wet_days == 1) %>% 
    summarise(count = sum(counter >= k)) %>% 
    cbind(type = 'wet_days', .)
  
  dataset.d <- setDT(.x)[, counter := seq_len(.N), by=rleid( dry_days)] %>% 
    filter(dry_days == 1) %>% 
    summarise(count = sum(counter >= k)) %>% 
    cbind(type = 'dry_days', .)
  
  
  dataset <- bind_rows(dataset.w, dataset.d) %>% as.tibble
  
  return(dataset)}



# In this part, count the number of the consecutive days dry and wet, in k days (for this case is 4).
kc_days <- test_days %>% 
  mutate(ocurrences = purrr::map(.x = data, 
                                 .f = find_ocurrences, k = 4)) %>%
  dplyr::select(id, long, lat,  year, ocurrences) %>% unnest



# it is only for do a idea about the variables
#kc_days %>% filter(type ==  'wet_days') %>%  ggplot(aes(x = id, y = count, colour = as.factor(year))) + geom_point() + theme_bw()




# ............................................................................................

# It is a table with the count about when was the crop failure if the reference time is k

count_timeWet <- function(row_id){
  id_row <- row_id %>%
    filter(type == 'wet_days') %>% 
    mutate(crop_failure =  ifelse(count < 1 , 'failure', '-')) %>% # if it don't have... rainy days
    count(crop_failure) %>%
    mutate(freq = n /  sum(n))
  return(id_row)}

table <- kc_days %>% 
  nest(-id, -long, -lat) %>% 
  mutate(wet_count = purrr::map(.x = data, .f = count_timeWet)) %>% 
  dplyr::select(id, long, lat , wet_count) 


table %>% filter(row_number() == 1) %>% dplyr::select(wet_count) %>% unnest

#failure_freq <- round((table %>% filter(crop_failure == 1) %>% dplyr::select(n)) / sum(table %>% dplyr::select(n)), 2)
#data.frame(table, failure_freq) 

# It is a graph with the count about when was the crop failure if the reference time is k

table %>% 
  unnest %>% 
  # filter(crop_failure == 'failure') %>% 
  ggplot(aes(x = long, y = lat, fill = freq)) +
  geom_raster() + 
  scale_fill_gradientn(colours = rev(heat.colors(10))) + 
  geom_polygon(data = shp_colombia, aes(x=long, y = lat, group = group), color = "gray30", fill=NA)  +
  coord_fixed() + facet_grid(~crop_failure) + 
  theme_bw() + 
  theme(legend.position = 'bottom')



### other test =-=-=-=-=-=-=-=-=-=-=-=

proof_for_gumbel <- test_days %>% 
  dplyr::select(id, lat, long, year, data) %>% 
  unnest %>% group_by(id, lat, long, year) %>%
  summarise(prec_mean =  mean(precip)) %>% 
  ungroup()



a <- extRemes::fevd(proof_for_gumbel$prec_mean, type="Gumbel", threshold = 0, time.units = "days/year" )

# Parameters: location --- scale  ---  shape
a$initial.results$MOM$pars
plot(a)
plot(a, 'trace')

#1- extRemes::pevd(0, a$initial.results$MOM$pars[1], a$initial.results$MOM$pars[2], a$initial.results$MOM$pars[3] ,type="Gumbel" )
#extRemes::pevd(ajam$precip, a$initial.results$MOM$pars[1], a$initial.results$MOM$pars[2], a$initial.results$MOM$pars[3] ,type="Gumbel" ) %>% 
#cbind(prob = . , precip = ajam$precip) %>% data.frame() %>%   plot



# =-=-=-=-=-=-=-= 


# =-=-=-=-=  With this function we count the number of consecutive days dry or wet. 
max_consecutive_days <- function(.x){
  
  #.x <- test_days %>% filter(id == 40 & year == 1984) %>% dplyr::select(-number_days) %>% unnest
  
  dataset.w <- setDT(.x)[, counter := seq_len(.N), by=rleid(wet_days)] %>% 
    filter(wet_days == 1) %>% 
    summarise(count = max(counter)) %>% 
    cbind(type = 'Max_cd_wet', .)
  
  dataset.d <- setDT(.x)[, counter := seq_len(.N), by=rleid( dry_days)] %>% 
    filter(dry_days == 1) %>% 
    summarise(count = max(counter)) %>% 
    cbind(type = 'Max_cd_dry', .)
  
  
  dataset <- bind_rows(dataset.w, dataset.d) %>% as.tibble
  
  return(dataset)}


# In this part, count the number of the consecutive days dry and wet, in k days (for this case is 4).
Cdays_max <- test_days %>% 
  mutate(max = purrr::map(.x = data, 
                          .f = max_consecutive_days)) %>%
  dplyr::select(id, long, lat,  year, max) %>% 
  unnest %>% 
  mutate(count = ifelse( count == -Inf, 0 , count) )


Cdays_max %>% 
  ggplot(aes(x = as.factor(year), y = count, fill = type)) + 
  geom_boxplot() + 
  theme_bw() 




p <- Cdays_max %>% 
  filter(year == 2010 & type == 'Max_cd_dry') %>% 
  ggplot(aes(x = long, y = lat, fill = count)) + 
  geom_raster() + 
  geom_polygon(data = shp_colombia, aes(x=long, y = lat, group = group), color = "gray30", fill=NA) + 
  scale_fill_gradient2() +
  coord_equal() + 
  theme_bw() 


# that thing is only for lear to do zoom with ggplot 
p +
  ggforce::facet_zoom(x = long < -50 & long > -125, y = lat > 10, horizontal = FALSE, zoom.size = 1 )






total_rows  <- test_days %>% dplyr::select(id,  total_rows) %>% unnest
# 

Fdays_max <- Cdays_max  %>% 
  left_join(total_rows, ., by = 'id') %>% 
  mutate(freq =  count/ total_rows * 100)



Fdays_max %>% 
  group_by(id, long, lat, type) %>% 
  summarise(Freq_mean = mean(freq)) %>% 
  filter(type == 'Max_cd_wet') %>% 
  ggplot(aes(x = long, y = lat, fill = Freq_mean)) + 
  geom_raster() + 
  geom_polygon(data = shp_colombia, aes(x=long, y = lat, group = group), color = "gray30", fill=NA) + 
  scale_fill_gradient2() +
  coord_equal() + 
  theme_bw() 


