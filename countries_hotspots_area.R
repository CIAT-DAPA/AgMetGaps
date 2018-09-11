

## Librerias
rm(list = ls())
library(glue)
library(sf)
library(raster)
library(tidyverse)


path <- "D:/agmetgaps/"
hotspots <-c('crops', 'climate','precipitation_hotspots', 'temperature_hotspots',
             'maize_hotspots','wwheat_hotspots')

countries <- c('BFA', 'NER','SEN', 'MLI', 'NGA', 'GHA')

y <- read_sf(glue("{path}ne_50m_admin_0_countries.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')


file <- glue("{path}{hotspots}.tif")


ar <- function(file,  variable, shape, countries){
  
  
  h <- raster(glue("{file}"))
  
  
  area_by_polygon <- function(countries, r, shapefile, variable){
    
    z <- shapefile %>%
      filter(ADM0_A3 == countries) %>%
      as('Spatial')
    
    
    r <- crop(r, z) %>% 
      mask(z)
    
    treshold_one <- which(r[] > 0 & r[] < 0.23)
    treshold_two <- which(r[] >= 0.23 & r[] < 0.46)
    treshold_three <- which(r[] >= 0.46)
    
    r[treshold_one] <- 1
    r[treshold_two] <- 2
    r[treshold_three] <- 3
    
    mn <- tapply(area(r), r[], sum)
    
    # print(variable)
    df <- data.frame(categoria=names(mn),sum=mn) %>%
      rename(!!variable := sum) %>%
      tbl_df() %>%
      filter(categoria %in% c(1, 2, 3)) %>%
      mutate(Country = countries)  %>%
      spread(categoria, !!variable) %>%
      rename_if(is.numeric, funs(glue('{variable}_{.}')))
    
    
    
    
    # names(r) <- countries
    return(df)
    
  }
  
  # print(countries)
  # print(variable)
  w <- purrr::map(.x = countries, .f = area_by_polygon, r = h, shapefile = shape, variable = variable)
  
  w <- bind_rows(w)
  
  # w_climate <- purrr::map(.x = countries, .f = area_by_polygon, r = h_crops, shapefile = y, variable = 'climate')
  # w_climate <- bind_rows(w_climate)
  
  
  
  a <- shape %>% filter(ADM0_A3 %in% countries) %>%
    mutate(ADM0_A3 = as_factor(ADM0_A3)) %>%
    mutate(ADM0_A3 = fct_relevel(ADM0_A3, countries)) %>% 
    arrange(ADM0_A3) %>%
    st_area
  
  
  a <- data_frame(area =a, Country = countries)
  
  
  make_area <- function(df, hotspots){
    
    crop <- df %>%
      dplyr::select(contains(hotspots)) %>%
      colnames() 
    
    
    # climate <- df %>%
    #   dplyr::select(contains('climate')) %>%
    #   colnames()
    # print(df)
    df %>%
      mutate_at(.funs = funs(./area), .vars = glue('{crop}')) 
    # mutate_at(.funs = funs(climate = ./area), .vars = glue('{climate}')) 
  }
  
  data <- reduce(.x = list(w, a), full_join, by ='Country') %>%
    dplyr::select(Country, area, contains(variable)) %>%
    mutate(area = as.numeric(area)/1000000) %>%
    nest() %>%
    mutate(x = purrr::map(.x =data, .f = make_area, variable)) %>%
    unnest(x) %>%
    dplyr::select(-area)
  
  
  
  
  
  
}

z <- purrr::map2(.x = file, .y =  hotspots, .f = ar, shape = y, countries = countries) %>%
  reduce(.f = left_join, by = 'Country') %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


write.csv(z, file = glue("{path}hotspots.csv"))










