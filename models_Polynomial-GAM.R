
############### modelos diferentes 
library(tidyverse)
library(raster)
library(rgdal)
library(ncdf4)
library(rgeos)
library(sf)
library(mgcv)


rootPath <- '/mnt/workspace_cluster_9/AgMetGaps/'
# rootPath <- '//dapadfs/workspace_cluster_9/AgMetGaps/'
calendarPath <-  paste0(rootPath, '3_monthly_climate_variability/katia_calendar/')
climatePath <- paste0(rootPath, '3_monthly_climate_variability/katia_climate/') 
IizumiPath <- paste0(rootPath, '1_crop_gaps/iizumi_processed/')
# modelsPath <- paste0(rootPath, '3_monthly_climate_variability/models_02/')
# modelPolyPath <- paste0(modelsPath, 'polynomial/')
# modelGAMpath <- paste0(modelsPath, 'GAM/')
shpPath <- paste0(rootPath, '0_general_inputs/shp/')
out_path <- paste0(rootPath, '3_monthly_climate_variability/Spatial_models/')
crop <- 'Maize'
seasonCrop <- 'maize_major'



# phenology <- c('Planting', 'Flowering', 'Harvest')

## loading calendar map
calendar <- list.files(paste0(calendarPath, crop), pattern = 'Int.tif$', full.names = TRUE)%>%
  raster::stack() %>%
  raster::crop(extent(-180, 180, -50, 50))

season_name <- names(calendar)

## debido a que la extraccion de raster con variables tipo string se hace mas lento
## se clasfica el calendario 0 como en NA (ya que no puede exisitir mes 0)

calendar_NA <- function(x){
  
  # x <- as.character(x)

  y <- if_else(is.na(x), 0, as.numeric(x))
  
  return(y)
}


# data <- calendar[] %>% tbl_df() %>%
#   mutate(FloweringMonthInt = ifelse(is.na(FloweringMonthInt), 0, FloweringMonthInt), 
#          HarvestMonthInt = ifelse(is.na(HarvestMonthInt), 0, HarvestMonthInt), 
#          PlantingMonthInt = ifelse(is.na(PlantingMonthInt), 0, PlantingMonthInt)) %>%
#   as.matrix()

data <- calendar[] %>%
  tbl_df() %>%
  mutate_all(funs(calendar_NA)) %>%
  as.matrix()

calendar[] <- data


## loading yield gap from Iizumi
gap_iizumi <- list.files(paste0(IizumiPath, seasonCrop), pattern = 'gap.tif$', full.names = TRUE)%>% 
  raster::stack() %>% 
  raster::crop(extent(-180, 180, -50, 50))

# knowing what is the number of year, we know that iizumi information it is from 1981 to 2011
number_years <- length(names(gap_iizumi))




# x %>% 
#   filter(!is.na(layer.1), layer.1 != 0 ) %>%
#   dplyr::select(PlantingMonthInt, FloweringMonthInt, HarvestMonthInt, yield_1981_gap, layer.2) %>%
#   mutate(pixel = 1:nrow(.)) %>%
#   filter(PlantingMonthInt > 10) %>%
#   filter(row_number() <= 2)
  # mutate(HP = HarvestMonthInt - PlantingMonthInt,
  #        HF = HarvestMonthInt - FloweringMonthInt,
  #        FP = FloweringMonthInt - PlantingMonthInt) %>% 
  # mutate(harvest_year = case_when( HarvestMonthInt <  PlantingMonthInt ~ "next_Harvest"))
  
  
  # dplyr::select(cond)

  # calendar <- calendar  %>% raster::crop(extent(-122.95833, 151.45833, -35.45833, 49.95833))
  # calendar <- velox(calendar)
  # calendar$extract_points(gaps_sf[1:100, ])
  # y$extract_points(gaps_sf[1:1000, ])


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
plan(future::cluster, workers = 19)
options(future.globals.maxSize= 8912896000)

## reading number_years of band in trimester files make for Katia (average for each trimester in planting flowering and harvesting)
stk_vx <- future.apply::future_lapply(X = trimester_files, FUN = read_bands, number_years) %>%
  future.apply::future_lapply(FUN = velox) 

gc(reset = T)
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
  
  values <-  stk_vx$extract_points(sp = spatial_points) %>%
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

season <- names(calendar)

calendar <- velox(calendar)
calendar_df <- calendar$extract_points(gaps_sf) %>%
  tbl_df() %>%
  purrr::set_names(season) %>%
  mutate(id = 1:nrow(.)) 
  

rm(calendar)

climate <- purrr::map2(.x = stk_vx, .y = type, .f = ~future(values_trimestre(.x, .y, gaps_sf, date_raster))) %>%
  future::values()
## id, lat, long, climate, year, trimestre, value

climate <- climate %>%
  purrr::reduce(left_join, by = c('id', 'lat', 'long', 'year'))

full_data <- left_join(climate, calendar_df, by = 'id')

## agregar el gap antes de hacer el nest

gther_spatial_points <- gaps_sf %>%
  st_set_geometry(NULL) %>%
  # mutate(id = 1:nrow(.)) %>%
  gather(year, gap) %>%
  dplyr::select(-year)

full_data <- bind_cols(full_data, gther_spatial_points)


library(fst)
install.packages('fst')
write_csv(full_data, paste0(out_path, seasonCrop, '.csv'))
fst::write_fst(full_data, paste0(out_path, seasonCrop, '.fst'))

# gaps_sf full_data
rm(list=setdiff(ls(), c('gaps_sf', 'full_data', 'type', 'season_name'))) 
gc(reset = T)
gc()

## funcion para organizar clima de acuerdo al planting y harvesting (recordar que se puede sembrar en diciembre y cosechar el otro año)

prueba <- full_data %>%
  filter(HarvestMonthInt <2)

# ... with 3,002,185 more rows, and 7 more variables:
  #   MaizeHarvestTrimTemp <dbl>, MaizePlantingTrimPrecip <dbl>,
  #   MaizePlantingTrimTemp <dbl>, FloweringMonthInt <dbl>,
  #   HarvestMonthInt <dbl>, PlantingMonthInt <dbl>, gap <dbl>


# Organizar esta parte que la haga automatico sin necesidad de ingresar las variables a cambiar



prueba <- prueba %>%
  nest(-id) %>%
  filter(row_number()<=1) %>%
  unnest()

# variables_trim <- c('PlantingTrimPrecip', 
#                     'FloweringTrimPrecip', 
#                     'HarvestTrimPrecip',
#                     'PlantingTrimTemp',
#                     'FloweringTrimTemp', 
#                     'HarvestTrimTemp')
# 
# variables_trim <-paste0('Maize', variables_trim)

## casi terminando 
prueba %>%
  dplyr::select(PlantingMonthInt, FloweringMonthInt, HarvestMonthInt, !!type, gap ) %>%
  mutate(gap_new = if_else(HarvestMonthInt <  PlantingMonthInt, 
                           lead(gap, 1), 
                           gap), 
         MaizeFloweringTrimPrecip_new = case_when(FloweringMonthInt < PlantingMonthInt ~ lead(MaizeFloweringTrimPrecip, 1),
                                                  HarvestMonthInt < FloweringMonthInt ~ MaizeFloweringTrimPrecip,
                                                  TRUE ~ MaizeFloweringTrimPrecip)) %>%
  filter(!is.na(MaizeHarvestTrimPrecip_new)) %>%
  dplyr::select(PlantingMonthInt, FloweringMonthInt, HarvestMonthInt, MaizeFloweringTrimPrecip, MaizeFloweringTrimPrecip_new, gap, gap_new) %>%
  as.data.frame() %>% head


  mutate(new = if_else(HarvestMonthInt <  PlantingMonthInt, lead(MaizePlantingTrimPrecip, 1), 
                       MaizePlantingTrimPrecip)) %>%
  dplyr::select(year, PlantingMonthInt, HarvestMonthInt, MaizePlantingTrimPrecip, new) %>%
  tail()




prueba %>%
  pull(HarvestMonthInt) %>%
  lead(1) %>%
  tail()

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





