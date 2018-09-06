

## Librerias

library(glue)
library(sf)
library(raster)
library(tidyverse)
path <- "D:/agmetgaps/"

h_crops <- raster(glue("{path}crops.tif"))
h_climate <- raster(glue("{path}climate.tif"))

y <- read_sf(glue("{path}ne_50m_admin_0_countries.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')


countries <- c('BFA', 'NER','SEN', 'MLI', 'NGA', 'GHA')


area_by_polygon <- function(countries, r, shapefile, variable){
  
  z <- shapefile %>%
    filter(ADM0_A3 == countries) %>%
    as('Spatial')
  
 
  r <- crop(r, z) %>% 
    mask(z)
  
  pos <- which(r[] > 0.0)
  
  r[pos] <- 1
  
  mn <- tapply(area(r), r[], sum)
  df <- data.frame(categoria=names(mn),sum=mn) %>%
    rename(!!variable:=sum) %>%
    tbl_df() %>%
    filter(categoria ==1) %>%
    mutate(Country = countries)
    
  
  
  # names(r) <- countries
  return(df)
  
}


w_crops <- purrr::map(.x = countries, .f = area_by_polygon, r = h_crops, shapefile = y, variable = 'area_crop')
w_crops <- bind_rows(w_crops) 

w_climate <- purrr::map(.x = countries, .f = area_by_polygon, r = h_climate, shapefile = y, variable = 'area_climate')
w_climate <- bind_rows(w_climate) 

#w_climate <- purrr::map(.x= countries, .f = area_by_polygon, r = h_climate, shapefile = y)
#w_climate <- unlist(w_climate) 

a <- y %>% filter(ADM0_A3 %in% countries) %>%
  mutate(ADM0_A3 = as_factor(ADM0_A3)) %>%
  mutate(ADM0_A3 = fct_relevel(ADM0_A3, countries)) %>% 
  arrange(ADM0_A3) %>%
  st_area
  

a <- data_frame(area =a, Country = countries)


reduce(.x = list(w_crops, w_climate, a), full_join, by = 'Country') %>%
  dplyr::select(-categoria.x, -categoria.y) %>%
  dplyr::select(Country, area, area_crop, area_climate) %>%
  mutate(area = as.numeric(area)/1000000) %>%
  mutate(hotspots_crops = (area_crop/area),
         hotspots_climate = (area_climate/area))







