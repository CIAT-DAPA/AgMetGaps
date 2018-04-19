
############### modelos diferentes 
library(tidyverse)
library(raster)
library(rgdal)
library(ncdf4)
library(rgeos)
library(sf)
library(mgcv)


rootPath <- '//dapadfs/Workspace_cluster_9/AgMetGaps/'
calendarPath <-  paste0(rootPath, '3_monthly_climate_variability/katia_calendar/')
climatePath <- paste0(rootPath, '3_monthly_climate_variability/katia_climate/') 
IizumiPath <- paste0(rootPath, '1_crop_gaps/iizumi_processed/')
# modelsPath <- paste0(rootPath, '3_monthly_climate_variability/models_02/')
# modelPolyPath <- paste0(modelsPath, 'polynomial/')
# modelGAMpath <- paste0(modelsPath, 'GAM/')
shpPath <- paste0(rootPath, '0_general_inputs/shp/')

crop <- 'Maize'
seasonCrop <- 'maize_major'

# phenology <- c('Planting', 'Flowering', 'Harvest')

## loading calendar map
calendar <- list.files(paste0(calendarPath, crop), pattern = 'Int.tif$', full.names = TRUE) %>%
  raster::stack() %>% 
  raster::crop(extent(-180, 180, -50, 50))
## loading yield gap
gap_iizumi <- list.files(paste0(IizumiPath, seasonCrop), pattern = 'gap.tif$', full.names = TRUE) %>% 
  raster::stack() %>% 
  raster::crop(extent(-180, 180, -50, 50))



# knowing what is the number of year, we know that iizumi information it is from 1981 to 2011
number_years <- length(names(gap_iizumi))

# extract coordinates from iizumi yield gap
gaps_sf <- rasterToPoints(gap_iizumi)%>%
  tbl_df() %>%
  dplyr::rename(lat = y, long = x) %>%
  st_as_sf(coords = c("long", "lat"))


# values_gap <- rasterToPoints(gap_iizumi)[, 3] %>%
#   tbl_df() %>%
#   rename(gap = value)
## i = 1

## function to extract the trimester variable

extract_trimestre <- function(x){
  # list.files(paste0(climatePath, crop), full.names=T)
  
  x %>%
    basename() %>% 
    stringr::str_replace(pattern = "[0-9]+.nc$", replacement = '')
}

## function to read n bands from a raster

read_bands <- function(x, n_bands){
  
  stack(x, bands = 1:n_bands) %>%
    flip(direction = 2) %>%
    t()
  
}

trimester_files <- list.files(paste0(climatePath, crop), full.names = T)

## aplicar plan future
library(future)
library(future.apply)
library(velox)
plan(sequential)
plan(future::cluster, workers = 28)
options(future.globals.maxSize= 8912896000)

## reading number_years of band in trimester files make for Katia (average for each trimester in planting flowering and harvesting)
stk_vx <- future.apply::future_lapply(X = trimester_files, FUN = read_bands, number_years) %>%
  future.apply::future_lapply(FUN = velox) 

## es para averiguar que trimestre es

type <- list.files(paste0(climatePath, crop), full.names = T) %>%
  data_frame(path = .) %>%
  mutate(type = basename(path),
         trimestre = extract_trimestre(path)) %>%
  dplyr::select(trimestre, everything()) %>%
  pull(trimestre)


## crear funcion para extraer 

date_raster <- 1981:2011
# values_trimestre(stk_vx[[1]], type[1], spatial_points, date_raster)
values_trimestre <- function(stk_vx, variable, spatial_points, date_raster){
  
  # stk_vx[[1]]
  # variable <- type[1]
  # spatial_points <- gaps_sf
  
  values <-  stk_vx$extract_points(spatial_points) %>%
    tbl_df() %>%
    purrr::set_names(date_raster) 
  # mutate(id = 1:nrow(.))
  
  
  ## quiza meter estos objetos como inputs para no estarlos creando para cada variable
  coords <- spatial_points %>%
    st_coordinates() %>%
    as_tibble() %>%
    dplyr::select(X, Y) %>%
    rename(long = X, lat = Y)
  
  # gther_spatial_points <- spatial_points %>%
  # st_set_geometry(NULL) %>%
  # gather(year, gap)
  
  #######
  
  
  climate_gap <- bind_cols(coords, values) %>%
    # rename_at(vars(contains('V')), funs(c(!!date))) %>%
    mutate(id = 1:nrow(.)) %>%
    gather(year, !!variable, -lat, -long, -id)
  # bind_cols(gther_spatial_points) %>%
  # dplyr::select(-year1)
  # group_by(ID) %>%
  # nest(-id)
  
  
  
  return(climate_gap)
  
  rm(coords)
  # rm(gther_spatial_points)
}

climate <- purrr::map2(.x = stk_vx, .y = type, .f = ~future(values_trimestre(.x, .y, spatial_points, date_raster))) %>%
  future::values()
## id, lat, long, climate, year, trimestre, value

climate <- climate %>%
  purrr::reduce(left_join, by = c('id', 'lat', 'long', 'year'))

## agregar el gap antes de hacer el nest

gther_spatial_points <- gaps_sf %>%
  st_set_geometry(NULL) %>%
  mutate(id = 1:nrow(.)) %>%
  gather(year, gap, -id)

climate <- climate %>%
  nest(-id)



# Polynomial models in this case, they are linear regression models with quadratic terms in the variables:
# y = b0 + b1 * x1 + b2 * (x2)^2


##### Climate relationship models with Gaps 
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
## Packages
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
# suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
# suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
# suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
# suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
# suppressMessages(if(!require(rgeos)){install.packages('rgeos'); library(rgeos)} else {library(rgeos)})
# suppressMessages(if(!require(sf)){install.packages('sf'); library(sf)} else {library(sf)})
# suppressMessages(if(!require(mgcv)){install.packages('mgcv'); library(mgcv)} else {library(mgcv)})

library(tidyverse)
library(raster)
library(rgdal)
library(ncdf4)
library(rgeos)
library(sf)
library(mgcv)

# =-=-=-=-=-=-=-=-=-= Routes
rootPath <- '/mnt/workspace_cluster_9/AgMetGaps/'
calendarPath <-  paste0(rootPath, '3_monthly_climate_variability/katia_calendar/')
climatePath <- paste0(rootPath, '3_monthly_climate_variability/katia_climate/') 
IizumiPath <- paste0(rootPath, '1_crop_gaps/iizumi_processed/')
modelsPath <- paste0(rootPath, '3_monthly_climate_variability/models_02/')
modelPolyPath <- paste0(modelsPath, 'polynomial/')
modelGAMpath <- paste0(modelsPath, 'GAM/')
shpPath <- paste0(rootPath, '0_general_inputs/shp/')

# =-=-=-=-=-= Polynomial model -- Function 
polynomial <- function(.x , .y){
  if(.x == 0){
    summary(lm(.y$yield ~ .y$value + I(.y$value^2)))$r.squared 
  }else{
    summary(lm(.y$yield[-1] ~.y$value[-31] + I(.y$value[-31]^2)))$r.squared 
  }
}

### GAM
gam_model <- function(.x,.y){ 
  tryCatch( {
    
    if(.x == 0){
      gm <- gam(.y$yield~ s(.y$value, fx = TRUE), method="REML" )
      dev <- summary(gm)$dev.expl  
      
    }else{
      
      gm <- gam(.y$yield[-1]~ s(.y$value[-31], fx = TRUE) )
      dev <- summary(gm)$dev.expl 
      
    }
    return(dev) } 
    , error = function(e) {
       return(NA)
    } )
  }

calcModels <- function(crop, seasonCrop){
  # =-=-=-=-=-=-=-=-=-= Calendar (raster)
  # crop <- 'Maize'
  # seasonCrop <- 'maize_major'
  calendar <- list.files(paste0(calendarPath, crop), pattern = 'Int.tif$', full.names = TRUE) %>%
    raster::stack() %>% 
    raster::crop(extent(-180, 180, -50, 50))

  # =-=-=-=-=-=-=-=-= read all data sets
  
  # =-=-=-=-=-= read gap data
  iizumi <- list.files(paste0(IizumiPath, seasonCrop), pattern = 'gap.tif$', full.names = TRUE) %>% 
    raster::stack() %>% 
    raster::crop(extent(-180, 180, -50, 50))
  
  for(i in 1:6){
    # i <- 1
    # =-=-=-=-=-= read climate data
    print(i)
    names <- strsplit(list.files(paste0(climatePath, crop))[i], ".nc$") %>%  unlist
    
    print(paste0('Crop: ', crop, ' --- Season: ', seasonCrop, ' --- Variable: ', names))
    
    climate <- stack(x = list.files(paste0(climatePath, crop), full.names=T)[i] , bands= 1:31) %>% 
      flip(., direction = 2) %>%
      t()
    
    # crop(x = climate,extent(-180, 180, -50, 50))
    extent(climate) <- extent(-180, 180, -50, 50)
    crs(climate) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    
    # =-=-=-=-=-=-= Stack with all information data
    rasts <- raster::stack(climate,iizumi,calendar) %>% 
      rasterToPoints(x = .)  %>% 
      as_tibble(.)  %>%
      mutate(., ID = 1:nrow(.)) %>% 
      filter(layer.1 != -999 ) %>%  # aqui se cambia el valor del NA del clima
      filter( !is.na(yield_1981_gap ) )  %>% 
      filter( !is.na(FloweringMonthInt ) )  %>% # en las siguientes 3 lineas se cambia el valor del NA del calendario
      filter( !is.na(HarvestMonthInt ) ) %>% 
      filter( !is.na(PlantingMonthInt ) )
    
    # =-=-=-=-=-=-=-= Split tables
    climate <- rasts[, c(68, 1:33)] %>% 
      setNames( c("ID" ,"lon","lat", paste0("year.",1981:2011) )  )  %>% 
      gather( year , value, year.1981:year.2011 )
    
    yield <- rasts[, c(68, 1:2, 34:64)]  %>% 
      setNames( c("ID", "lon","lat", paste0("year.",1981:2011) )  ) %>% 
      gather( year , yield, year.1981:year.2011 )
    
    # Restrictions at year-month 
    calend <- rasts[, c(68,  65:67)] %>% 
      mutate(HP = HarvestMonthInt - PlantingMonthInt, HF = HarvestMonthInt - FloweringMonthInt) %>% 
      mutate(year_start =  ifelse(HF > 0 | HP > 0 , 0, 1)) %>% 
      dplyr::select(ID, year_start)
    
    ## =-=-=-= Join Yield Gap with climate
    model_data <- bind_cols( climate, yield) %>% 
      dplyr::select(-ID1,-lon1, -lat1, -year1)
    
    # =-=-=-=-=-=-=  Agrupate by ID information and paste the restrictions
    proof <- model_data %>% 
      group_by(ID) %>%
      nest(-ID) %>% 
      left_join(., calend)
    
    # =-=-=-=-=-= Run Models (linear (Polynomial)  and GAM)
    system.time(
    poly_GAM <- proof %>%
      mutate(polynomial = purrr::map2(.x = year_start, .y = data, .f = polynomial) ) %>%
      mutate(GAM = purrr::map2(.x = year_start, .y = data, .f = gam_model))  %>%
      dplyr::select(ID, polynomial, GAM) %>% unnest
    )

    # =-=-=-=-=-= Join ID with lat, lon y models values
    poly_GAM_table <- rasts[, c(68, 1:2)] %>%
      inner_join(.,poly_GAM) %>%
      dplyr::select(x,y,polynomial,GAM) %>%
      filter(polynomial != 'NA' ) %>%
      filter(GAM != 'NA' )

    # =-=-=-=-=-= Making map result and save in .png
    shp <- read_sf(paste0(shpPath, 'mapa_mundi.shp')) %>%
      as('Spatial') %>%
      crop(extent(-180, 180, -50, 50))

    poly_GAM %>% 
        bind_cols(., rasts[,c(1,2)]) %>%
        ggplot(aes(x = x, y = y))  +
        geom_tile(aes(fill = polynomial)) + 
        geom_polygon(data = shp , aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
        coord_equal() + theme_bw() +  scale_fill_distiller(palette = "Spectral") + 
        labs(x= 'Longitude', y = 'Latitude')
    
    print(paste0(modelPolyPath, crop, '/polynomial_', names, '.png'))
    ggsave(paste0(modelPolyPath, crop, '/polynomial_', names, '.png'))

    poly_GAM %>% 
      bind_cols(., rasts[,c(1,2)]) %>%
      ggplot(aes(x = x, y = y))  +
      geom_tile(aes(fill = GAM)) + 
      geom_polygon(data = shp , aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
      coord_equal() + theme_bw() +  scale_fill_distiller(palette = "Spectral") + 
      labs(x= 'Longitude', y = 'Latitude')
    
    print(paste0(modelGAMpath, crop, '/GAM_', names, '.png'))
    ggsave(paste0(modelGAMpath, crop, '/GAM_', names, '.png'))
    
    # =-=-=-=-=-=- Write result tables in csv
    print("cvs")
    write.csv(x = poly_GAM_table, file = paste0(modelsPath, crop, '_poly_GAM_', names, '.csv'))
    
    # =-=-=-=-=-=- Rasterize tables and write raster in tiff
    tmpRaster <- raster(nrow=1200,ncol=4320)
    extent(tmpRaster) <- extent(-180, 180, -50, 50)
    coordinates(poly_GAM_table) <- ~x+y

    resultRaster <- rasterize(poly_GAM_table, tmpRaster , poly_GAM_table$polynomial)
    writeRaster(x = resultRaster, file = paste0(modelPolyPath, crop, '/polynomial_', names, '.tif'), format="GTiff", overwrite=TRUE)
    
    resultRaster <- rasterize(poly_GAM_table, tmpRaster , poly_GAM_table$GAM)
    writeRaster(x = resultRaster, file = paste0(modelGAMpath, crop, '/GAM_', names, '.tif'), format="GTiff", overwrite=TRUE)
  }
}

calcModels('Maize', 'maize_major')
calcModels('Rice', 'rice_major')
calcModels('Wheat', 'wheat_spring')







# =-=-=-=-=-=-=-=-=-= results summary 


proof <- list.files(modelsPath, pattern = '.csv$', full.names = TRUE) %>% 
  as.tibble() %>% 
  mutate(only_names = list.files(modelsPath, pattern = '.csv$')) %>% 
  mutate(data =  purrr::map(.x = value, .f =  read.csv)) %>%
  select(-value) %>% 
  separate(only_names, c("Crop", "value"), '_poly_GAM_') %>% 
  separate( value , c("Var"), '.csv') %>% 
  unite('var', c('Crop', 'Var')) %>%  unnest() %>% 
  mutate(dif = ifelse((GAM - polynomial)>0, 1, 0)) 


proof %>%
  ggplot(aes(x = polynomial, y = GAM)) + geom_point(aes(colour = as.factor(dif))) +
  facet_grid(~var) + 
  theme_bw()

ggsave(paste0(modelsPath, 'test.png'), width = 15, height = 4)



proof %>% 
  select(var, polynomial, GAM) %>% 
  gather(models, value, -var) %>% 
  ggplot(aes(x =  var, y = value)) + geom_boxplot(aes(fill =  models)) +theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = '', y = 'R^2')

ggsave(paste0(modelsPath, 'Comparison.png'), width = 10, height = 7)





