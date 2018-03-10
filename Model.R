##### Climate relationship models with Gaps 


# dir_path <- '/mnt/workspace_cluster_9/AgMetGaps/' # for server linux
dir_path <- '//dapadfs/Workspace_cluster_9/AgMetGaps/' # for server windows
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
## Packages
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##


#### Temporarily
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
suppressMessages(if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)})
suppressMessages(if(!require(readr)){install.packages('readr'); library(readr)} else {library(readr)})
suppressMessages(if(!require(stringr)){install.packages('stringr'); library(stringr)} else {library(stringr)})
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(if(!require(mgcv)){install.packages('mgcv'); library(mgcv)} else {library(mgcv)})


# =-=-=-=-=-=-=-=-=-= Routes
crop <- 'Maize'
calendarp <-  '3_monthly_climate_variability/katia_calendar/'
#   rm(flor);  g <- gc();  rm(g);  removeTmpFiles( h = 24)

       
# =-=-=-=-=-=-=-=-=-= Calendar (raster)

calendar <- list.files(paste0(dir_path, calendarp, crop) , pattern = '.tif', full.names = TRUE) %>% 
  stack() %>% 
  crop(extent(-180, 180, -50, 50))



calendar2 <- list.files(paste0(dir_path, calendarp, crop) , full.names = TRUE) %>% 
  stack() %>% 
  crop(extent(-180, 180, -50, 50)) %>%  
  raster::rasterToPoints(x = .) %>% 
  as_tibble(.)  %>% 
  dplyr::mutate(., ID = 1:nrow(.) ) 


rasterToPoints(calendar) # 1. verificar si existen NA, ya que dependiendo del archivo pueden llamarse NaN, NA o -999


sum(is.na(calendar2$FloweringMonth))
sum(is.na(calendar2$HarvestMonth))
sum(is.na(calendar2$PlantingMonth))




# =-=-=-=-=-=-=-=-= read all data sets


# =-=-=-=-=-= read gap data
Iizumi <- '1_crop_gaps/iizumi_processed/'
crop1 <- 'maize_major' # rice_major - wheat_spring

iizumi <- list.files(paste0(dir_path, Iizumi, crop1), pattern = 'gap', full.names = TRUE) %>% 
  stack() %>% 
  crop(extent(-180, 180, -50, 50))




# =-=-=-=-=-= read climate data


i <- 1 # 1:6

climate <- '3_monthly_climate_variability/katia_climate/'  
names <- strsplit(list.files(paste0(  dir_path, climate, crop))[i], ".nc") %>%  unlist



climate <- stack(x = list.files(paste0(  dir_path, climate, crop), full.names=T)[1] , bands= 1:31) %>% 
  flip(., direction = 2) %>%
  t()

crop( x = climate,extent(-180, 180, -50, 50))
extent(climate) <- extent(-180, 180, -50, 50)
crs(climate) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


rasterToPoints(climate[[1]]) %>% head # El valor que aparezca en las primeras filas es el NA que se toma para esa variable





# =-=-=-=-=-=-= Stack with all information data


rasts <- raster::stack(climate,iizumi,calendar) %>% 
  rasterToPoints(x = .)  %>% 
  as_tibble(.)  %>%
  mutate(., ID = 1:nrow(.)) %>% 
  filter(layer.1 != -999 ) %>%  # aqui se cambia el valor del NA del clima
  filter( !is.na(yield_1981_gap ) )  %>% 
  filter( !is.na(FloweringMonth ) )  %>% # en las siguientes 3 lineas se cambia el valor del NA del calendario
  filter( !is.na(HarvestMonth ) ) %>% 
  filter( !is.na(PlantingMonth ) )




# =-=-=-=-=-=-=-= Split tables

climate <- rasts[, c(68, 1:33)] %>% 
  setNames( c("ID" ,"lon","lat", paste0("year.",1981:2011) )  )  %>% 
  gather( year , value, year.1981:year.2011 )

yield <- rasts[, c(68, 1:2, 34:64)]  %>% 
  setNames( c("ID", "lon","lat", paste0("year.",1981:2011) )  ) %>% 
  gather( year , yield, year.1981:year.2011 )


# Restrictions at year-month 
calend <- rasts[, c(68,  65:67)] %>% 
  mutate(HP = HarvestMonth- PlantingMonth, HF = HarvestMonth- FloweringMonth) %>% 
  mutate(year_start =  ifelse(HF > 0 | HP > 0 , 0, 1)) %>% 
  select(ID, year_start)


## =-=-=-= Join Yield Gap with climate
model_data <- bind_cols( climate, yield) %>% 
  select(-ID1,-lon1, -lat1, -year1)




# =-=-=-=-=-=-=  Agrupate by ID information and paste the restrictions

proof <- model_data %>% 
  group_by(ID) %>%
  nest(-ID) %>% 
  left_join(., calend)




# =-=-=-=-=-= Polynomial model -- Function 
model <- function(.x , .y){
  if(.x == 0){
    summary(lm(.y$yield ~ .y$value + I(.y$value^2)))$r.squared 
  }else{
    summary(lm(.y$yield[-1] ~.y$value[-31] + I(.y$value[-31]^2)))$r.squared 
  }
}



#### GAM
library(mgcv)

gam_model <- function(.x,.y){ 
  
  if(.x == 0){
    gm <- gam(.y$yield~ s(.y$value,fx = TRUE) )
    dev <- summary(gm)$dev.expl  
    
  }else{
    
    gm <- gam(.y$yield[-1]~ s(.y$value[-31],fx = TRUE) )
    dev <- summary(gm)$dev.expl 
    
  }
  return(dev)
}






# =-=-=-=-=-= Run one model 


rsquare <- proof %>%
  mutate(model = purrr::map2(.x = year_start, .y = data, .f = model)) %>%
  #mutate(GAM = purrr::map2(.x = year_start, .y = data, .f = gam_model))  %>%
  select(ID, model) %>% unnest

hist(rsquare$model)
boxplot(rsquare$model)
sum(rsquare$model > 0.5)



#hist(rsquare$GAM)
#boxplot(rsquare$GAM)
#sum(rsquare$GAM > 0.5)


write.csv(x = rsquare, file = paste0(dir_path, '3_monthly_climate_variabilit/Models/', crop, '/', names, '.csv'))
writeRaster(x = rsquare, file = paste0(dir_path, '3_monthly_climate_variabilit/Models/', crop, '/', names, '.tif'))



library(sf)    

shp <- read_sf(paste0(dir_path, '/0_general_inputs/shp/mapa_mundi.shp')) %>%
  as('Spatial') %>%
  crop(extent(-180, 180, -50, 50))


rsquare %>% 
  bind_cols(., rasts[,1:2]) %>%
  ggplot(aes(x = x, y = y))  +
  geom_tile(aes(fill = GAM)) + 
  geom_polygon(data = shp , aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  coord_equal() + theme_bw() +  scale_fill_distiller(palette = "Spectral") + 
  labs(x= 'Longitude', y = 'Latitude')


ggsave(paste0(dir_path, '3_monthly_climate_variabilit/Models/', crop, '/', names, '.png'))

