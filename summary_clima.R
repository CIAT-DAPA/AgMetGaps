
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
