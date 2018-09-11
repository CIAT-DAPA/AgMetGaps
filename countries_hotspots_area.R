

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
  
  treshold_one <- which(r[] < 0.23)
  treshold_two <- which(r[] >= 0.23 & r[] < 0.46)
  treshold_three <- which(r[] >= 0.46)
  
  r[treshold_one] <- 1
  r[treshold_two] <- 2
  r[treshold_three] <- 3
  
  mn_one <- tapply(area(r), r[], sum)
  # mn_two <- tapply(area(r), r[], sum)
  # mn_three <- tapply(area(r), r[], sum)
  
  df <- data.frame(categoria=names(mn_one),sum_one=mn_one) %>%
    rename(!!variable:=sum_one) %>%
    tbl_df() %>%
    filter(categoria %in% c(1, 2, 3)) %>%
    mutate(Country = countries)  %>%
    spread(categoria, !!variable) %>%
    rename_if(is.numeric, funs(glue('{variable}_{.}')))
    
    
  
  
  # names(r) <- countries
  return(df)
  
}


w_crops <- purrr::map(.x = countries, .f = area_by_polygon, r = h_crops, shapefile = y, variable = 'crop')
w_crops <- bind_rows(w_crops)

w_climate <- purrr::map(.x = countries, .f = area_by_polygon, r = h_climate, shapefile = y, variable = 'climate')
w_climate <- bind_rows(w_climate) 

#w_climate <- purrr::map(.x= countries, .f = area_by_polygon, r = h_climate, shapefile = y)
#w_climate <- unlist(w_climate) 

a <- y %>% filter(ADM0_A3 %in% countries) %>%
  mutate(ADM0_A3 = as_factor(ADM0_A3)) %>%
  mutate(ADM0_A3 = fct_relevel(ADM0_A3, countries)) %>% 
  arrange(ADM0_A3) %>%
  st_area
  

a <- data_frame(area =a, Country = countries)


make_area <- function(df){
  
  crop <- df %>%
    dplyr::select(contains('crop')) %>%
    colnames() 
  
  climate <- df %>%
    dplyr::select(contains('climate')) %>%
    colnames()

  df %>%
    mutate_at(.funs = funs(crop = ./area), .vars = glue('{crop}')) %>%
    mutate_at(.funs = funs(climate = ./area), .vars = glue('{climate}')) 
  # mutate(!!!glue('crop_{crop}') := !!!sym(glue('crop_{crop}'))/ !!!sym(glue('area')))
}

data <- reduce(.x = list(w_crops, w_climate, a), full_join, by ='Country') %>%
  # dplyr::select(-categoria.x, -categoria.y) %>%
  dplyr::select(Country, area, contains('crop'), contains('climate')) %>%
  mutate(area = as.numeric(area)/1000000) %>%
  # mutate_at(.funs = funs(toAsset = ./area), .vars = glue("crop_1"))
  nest() %>%
  mutate(x = purrr::map(.x =data, .f = make_area)) %>%
  unnest(x)
  # mutate(!!(glue('hots_crop_1')) := !!sym(glue('crop_1'))/ !!sym(glue('area')))
  # mutate(hotspots_percentage_crops = (hotspot_area_crop/area),
  #        hotspots__percentage_climate = (hotspot_area_climate/area))

write.csv(data, file = glue("{path}hotspots.csv"))







