##### Climate relationship models with Gaps 


# dir_path <- '/mnt/workspace_cluster_9/AgMetGaps/' # for server linux
dir_path <- '//dapadfs/Workspace_cluster_9/AgMetGaps/' # for server windows
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
## Packages
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##


#### Temporarily
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
suppressMessages(if(!require(velox)){install.packages('velox'); library(velox)} else {library(velox)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
suppressMessages(if(!require(future)){install.packages('future'); library(future)} else {library(future)}) # parallel package
suppressMessages(if(!require(doFuture)){install.packages('doFuture'); library(doFuture)} else {library(doFuture)})
suppressMessages(if(!require(magrittr)){install.packages('magrittr'); library(magrittr)} else {library(magrittr)})
suppressMessages(if(!require(lubridate)){install.packages('lubridate'); library(lubridate)} else {library(lubridate)})
suppressMessages(if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)})
suppressMessages(if(!require(readr)){install.packages('readr'); library(readr)} else {library(readr)})
suppressMessages(if(!require(stringr)){install.packages('stringr'); library(stringr)} else {library(stringr)})
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(if(!require(chron)){install.packages('chron'); library(chron)} else {library(chron)})
suppressMessages(if(!require(geoR)){install.packages('geoR'); library(geoR)} else {library(geoR)})

###### Graphical parameters -----  to ggplot2

### Lectura del shp
shp <- shapefile(paste0(dir_path, '0_general_inputs/shp/mapa_mundi.shp',sep="")) %>% 
  crop(extent(-180, 180, -50, 50))
       
       #ewbrks <- c(seq(-180,0,45), seq(0, 180, 45))
       #nsbrks <- seq(-50,50,25)
       #ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(abs(x), "°W"), ifelse(x > 0, paste( abs(x), "°E"),x))))
       #nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(abs(x), "°S"), ifelse(x > 0, paste(abs(x), "°N"),x))))
       
       #Blues<-colorRampPalette(c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#045a8d','#023858','#233159'))
       
       
       
       
       
       
       
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       ## =-=-=-=-=-=   Gaps_resolution 
       
       ## the next directories depends of the main path
       ## Settings Iizumi paths
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       ## =-=-=-=-=-=   Gaps_resolution 
       
       ## the next directories depends of the main path
       ## Settings Iizumi paths
       
       
       crop <- 'wheat_spring' # 'rice_major'
       
       Iizumi <- '1_crop_gaps/iizumi_processed/wheat_spring/'
       Iizumi <- glue:: glue('{dir_path}{Iizumi}')
       
       
       
       iizumi_S <- list.files(Iizumi, pattern = 'gap',full.names = TRUE)[1] %>%  
         stack() %>% 
         crop(extent(-180, 180, -50, 50))  
       
       
       
       
       
       
       # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       # =-=-=-=-=-=-=-= Calendar Correction ...
       
       # date_c <- 'Inputs/05-Crop Calendar Sacks/'
       # //dapadfs/Workspace_cluster_9/AgMetGaps/calendar_5min
       
       date_c <- '0_general_inputs/calendar_5min'
       date_c <- glue:: glue('{dir_path}{date_c}')
       
       
       # glob2rx('*Wheat.crop*.nc$*') --- '*Wheat.crop*.nc$*'
       pdate <- list.files(path =  date_c, pattern = glob2rx('*Wheat.crop*.nc$*'), full.names = TRUE) %>%
         raster(. , varname = 'plant')  %>% 
         crop(extent(-180, 180, -50, 50)) 
       
       
       
       pdate_points <- rasterToPoints(pdate) %>%
         tbl_df() %>% 
         setNames(nm = c('x', 'y', 'julian.start')) %>%
         mutate(Planting =  month.day.year(julian.start)$month)
       
       
       hdate <- list.files(path =  date_c, pattern = glob2rx('*Wheat.crop*.nc$*'), full.names = TRUE) %>%
         raster(. , varname = 'harvest') %>% 
         crop(extent(-180, 180, -50, 50)) 
       
       hdate_points <- rasterToPoints(hdate) %>%
         tbl_df() %>% 
         setNames(nm = c('x', 'y', 'julian.h')) %>%
         mutate(harvest =  month.day.year(julian.h)$month) 
       
       
       
       
       
       
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       ## Functions for creation to the calendar with three seasons (harvest- floration - planting)
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       
       # Compute floration trimester
       compute_flor <- function(.x, .y){
         
         if(.x < .y){
           flor <- round(.x + (.y - .x)/2 , 0)
         } else if(.x > .y){
           flor <- round(.x +  ( (12 - .x) + .y)/2 , 0)
         }
         
         if(flor> 12){flor <- flor - 12} else if( flor <= 12){flor <- flor}
         
         return(flor)
       }
       
       # Compute the trimester
       month_below <- function(.x){
         if(.x > 1) { below <- .x - 1} else if(.x  == 1){ below <- 12 }
         return(below)}
       
       month_above <- function(.x){
         if(.x < 12) { above <- .x + 1} else if(.x  == 12){ above <- 1 }
         return(above)}
       
       
       
       
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       #######                   Crop calendar in a table                  ######## 
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       
       
       crop.time <- inner_join(pdate_points, hdate_points) %>% 
         select(-julian.start, -julian.h) %>% 
         mutate(flor = purrr::map2(.x = Planting, .y = harvest, .f = compute_flor)) %>% 
         unnest() %>% 
         select(x, y, Planting, flor, harvest) %>% 
         mutate(a.Planting1 = purrr::map(.x = Planting, .f = month_below), 
                a.Planting3 = purrr::map(.x = Planting, .f = month_above), 
                b.flor1 = purrr::map(.x = flor, .f = month_below), 
                b.flor3 = purrr::map(.x = flor, .f = month_above),
                c.harvest1= purrr::map(.x = harvest, .f = month_below), 
                c.harvest3 = purrr::map(.x = harvest, .f = month_above)) %>% 
         unnest  %>% 
         rename(a.Planting2 = Planting, b.flor2 = flor, c.harvest2 = harvest) %>% 
         select(x,y, a.Planting1, a.Planting2, a.Planting3, b.flor1, b.flor2, b.flor3, 
                c.harvest1, c.harvest2, c.harvest3) %>% 
         gather(key = phase.month, value = month, -x, -y)  %>% 
         separate(phase.month, c("phase", "type"), sep = "\\.") 
       
       
       
       
       
       
       
       # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
       # To use the spatial polygons information in the velox strack to points 
       # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
       sp_pdate <- rasterToPolygons(pdate,  dissolve = F) 
       
       
       
       
       
       
       
       
       
       
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       ##########      Functions to perform read raster,       ##########
       ##########    convert to velox objects, extract points, ##########
       ##########      convert to raster if it is necessary.   ########## 
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       ## Functions for management to Chirps information
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
       
       ## =-=-=-=-=-=-=-=-=-=-=##
       ###### function to read modificate raster to change band .y
       raster_mod <- function(.x, .y){
         raster(.x, band = .y)
       }
       ## =-=-=-=-=-=-=-=-=-=-=##
       
       
       ## =-=-=-=-=-=-=-=-=-=-=##
       #### Create a function to strack the points 
       extract_velox <- function(velox_Object, points){
         
         coords <- coordinates(points) %>%
           tbl_df() %>%
           rename(lat = V1, long = V2)
         
         velox_Object$extract(points, fun = function(x){ 
           x[x<0] <- NA
           x <- as.numeric(x)
           mean(x, na.rm = T)}) %>%
           tbl_df() %>%
           rename(values = V1) %>%
           bind_cols(coords) %>%
           dplyr::select(lat, long, values)
       }
       ## =-=-=-=-=-=-=-=-=-=-=##
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-= ##
       ##########        Precipitation: Chirps                 ########## 
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-= ##
       
       
       n_bands <- 372
       Chirps <- '0_general_inputs/chips_monthly'
       # nc_open(glue:: glue('{dir_path}{Chirps}/chirps-v2.0.monthly.nc'))$dim$time$len
       
       
       
       options(future.globals.maxSize= 1048576000)
       
       
       
       ## read monthly precipitation rasters with furure functions 
       plan(multisession, workers = availableCores() - 3)
       
       
       
       ## read monthly precipitation rasters 
       system.time(  
         dates_raster <-  "1981-1-1"  %>% 
           as.Date() %>%
           seq(by = "month", length.out = n_bands) %>%
           data_frame(date = .) %>%
           mutate(year = year(date), month = month(date)) %>%
           mutate(band = 1:n_bands, file = rep(paste0( dir_path, '0_general_inputs/chips_monthly/', 'chirps-v2.0.monthly.nc'), n_bands)) %>% 
           mutate(load_raster = purrr::map2(.x = file, .y = band, .f = ~future(raster_mod(.x,.y))))  %>%
           mutate(load_raster = purrr::map(.x = load_raster, .f = ~value(.x)))  %>% 
           mutate(raster_df = purrr::map(.x = load_raster, .f = ~future(velox(.x)))) %>%
           mutate(raster_df = purrr::map(raster_df, .f = ~value(.x))) 
       )
       
       
       
       future:::ClusterRegistry("stop")
       
       #### Extract points
       
       
       options(future.globals.maxSize= 1048576000)
       plan(multisession, workers = availableCores() - 4)
       
       system.time(  
         dates_raster2 <- dates_raster %>% 
           mutate(points = purrr::map(.x = raster_df, .f = ~extract_velox(velox_Object = .x, points = sp_pdate))) 
       )
       #rs <- dates_raster$raster_df[[1]]
       #object.size(sp_pdate)
       
       
       future:::ClusterRegistry("stop")
       
       
       
       
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-= ##
       
       # Understand the extract information for each pixel 
       # This part extact only the information for year, 
       # month and important points
       
       
       point_dates <-  dates_raster2 %>% 
         select( year, month, points) %>% 
         unnest %>% 
         group_by(lat, long) %>%
         nest() %>%
         mutate(id = 1:nrow(.))
       
       
       # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
       
       
       
       
       make_station <- function(x){
         
         weather_station <- x %>% 
           bind_rows() %>%
           unnest()
         
         coord <- weather_station %>%
           dplyr::select(lat, long) %>%
           distinct()
         
         lat <- dplyr::pull(coord, 'lat')
         long <- dplyr::pull(coord, 'long')
         
         
         return(weather_station)}
       
       
       
       
       
       
       point_extract <- function(x, y){
         
         proof <-  x  %>%
           inner_join(y , ., by = c('id', 'month')) %>%
           dplyr::select(-lat.y, -long.y) %>%
           group_by(phase, year,lat.x, long.x) %>%
           rename(lat = lat.x, long = long.x, precip =  values) %>% 
           summarise(prec_clim = sum(precip)) %>%
           ungroup()
         
         
         
         test1 <- inner_join(y , x, by = c('id', 'month')) %>%
           mutate(time = factor(type, labels = c(1, 2, 3))) %>% 
           filter(time == '2') %>% select(year , month)
         
         test <- left_join(proof, test1)
         
         return(test)
       }
       
       
       
       
       
       
       extract_months <- function(dates, atelier){
         
         test <-point_dates %>% 
           mutate(i = 1:nrow(.)) %>%
           nest(-i) %>% 
           mutate(stations = purrr::map(.x =  data , .f =  make_station)) %>% 
           mutate(each_Pclim =  purrr::map(.x =  stations, .f = point_extract, y = atelier))
         
         return(test) }
       
       
       
       
       
       
       
       
       
       # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
       
       # out1    ---  folder
       
       chirps.p <- paste0(dir_path, '3_monthly_climate_variability/filter_chirps/', crop)
       
       
       
       
       id_creation <- function(.x){
         .x %>% 
           group_by(type) %>%
           mutate(id = 1:length(type)) %>%
           ungroup() 
       }
       
       #### paralelizar ... 
       
       
       plan(multisession, workers = availableCores() - 3)
       
       system.time(
         month_prec <- crop.time %>% 
           rename(long = x,  lat  =  y) %>%
           mutate(control =  phase)  %>% 
           nest(-control) %>% 
           mutate(id_data = purrr::map(.x = data, .f = id_creation)) %>% 
           select(control, id_data) %>%  filter(row_number() == 1) %>% 
           mutate(ext.months = purrr::map(.x = id_data,  .f = ~future(extract_months(.x))) ) %>% #  dates = point_dates,
           mutate(ext.months =  purrr::map(.x = ext.months, .f = ~value(.x)))
       )
       
       
       future:::ClusterRegistry("stop")
       
       
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
       #Write out to netcdf format
       #writeRaster(month_prec, paste0(dir_path, '3_monthly_climate_variability/filter_chirps/wheat_spring/month_prec.nc'), overwrite=TRUE, format="CDF", varname="Temperature", varunit="degC", 
       #             longname="Precipitacion_trim", xname="X", yname="Y",zname="Year",
       #             zunit="numeric")
       
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
       
       
       
       
       
       
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-= ##
       ##                Mean Temperature: NCEP CPC GHCN_CAMS     
       ## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-= ##
       
       
       
       
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
       
       # read a one raster to GHCN_CAMS
       
       
       raster_mod_temp <- function(.x, .y){
         raster(.x, band = .y) %>% 
           rotate
       }
       
       
       
       extract_velox_temp <- function(velox_Object, points){
         coords <- coordinates(points) %>%
           tbl_df() %>%
           rename(lat = V1, long = V2)
         velox_Object$extract(points, fun = function(x){ 
           mean(x, na.rm = T)}) %>%
           tbl_df() %>%
           rename(values = V1) %>%
           bind_cols(coords) %>%
           dplyr::select(lat, long, values)
       }
       
       
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #        
       
       
       
       
       
       GHCN_CAMS <- '0_general_inputs/GHCN_CAMS/'
       
       
       
       
options(future.globals.maxSize= 1048576000)
       
## read monthly precipitation rasters with furure functions 
plan(multisession, workers = availableCores() - 3)
       
       

       
       system.time(  
         # read monthly temperature
         GHCN_CAMS <-   str_extract(nc_open(paste0('0_general_inputs/chips_monthly/', 'chirps-v2.0.monthly.nc'))$dim$time$units,
                                    "\\d{4}-\\d{1}-\\d{1}") %>%
           as.Date() %>%
           seq(by = "month", length.out = n_bands)  %>%
           data_frame(date = .) %>%
           mutate(year = year(date), month = month(date)) %>%
           mutate(band = 1:n_bands, file = rep(paste0(GHCN_CAMS, 'data.nc'), n_bands)) %>%
           mutate(load_raster = purrr::map2(.x = file, .y = band, .f = ~future(raster_mod_temp(.x,.y)))) %>%
           mutate(load_raster = purrr::map(.x = load_raster, .f = ~value(.x))) %>% 
           mutate(raster_df = purrr::map(.x = load_raster, .f = ~future(velox(.x)))) %>%
           mutate(raster_df = purrr::map(raster_df, .f = ~value(.x))) %>% 
           mutate(points = purrr::map(.x = raster_df, .f = ~future(extract_velox_temp(velox_Object = .x, points = sp_pdate)))) %>%
           mutate(points = purrr::map(points, ~value(.x))) 
       )
       
       
future:::ClusterRegistry("stop")
       
       
       