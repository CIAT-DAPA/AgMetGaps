##### Climate relationship models with Gaps 
rm(list=ls())

## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
## Packages
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
# require(tidyverse)
require(raster)
require(rgdal)
require(ncdf4)
require(rgeos)
require(sf)
require(mgcv)

# =-=-=-=-=-=-=-=-=-= Routes
rootPath <- '//dapadfs/Workspace_cluster_9/AgMetGaps/'
calendarPath <-  paste0(rootPath, '3_monthly_climate_variability/katia_calendar/')
climatePath <- paste0(rootPath, '3_monthly_climate_variability/katia_climate/') 
IizumiPath <- paste0(rootPath, '1_crop_gaps/iizumi_processed/')
modelsPath <- paste0(rootPath, '3_monthly_climate_variability/models/')
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
      gm <- gam(.y$yield~ s(.y$value, fx = TRUE) )
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
  # crop <- 'maize'
  # seasonCrop <- 'maize_major'
  calendar <- list.files(paste0(calendarPath, crop), pattern = 'Int.tif$', full.names = TRUE) %>%
    stack() %>% 
    crop(extent(-180, 180, -50, 50))

  # =-=-=-=-=-=-=-=-= read all data sets
  
  # =-=-=-=-=-= read gap data
  iizumi <- list.files(paste0(IizumiPath, seasonCrop), pattern = 'gap.tif$', full.names = TRUE) %>% 
    stack() %>% 
    crop(extent(-180, 180, -50, 50))
  
  for(i in 1:6){
    # i <- 1
    # =-=-=-=-=-= read climate data
    names <- strsplit(list.files(paste0(climatePath, crop))[i], ".nc$") %>%  unlist
    
    print(paste0('Crop: ', crop, ' --- Season: ', seasonCrop, ' --- Variable: ', names))
    
    climate <- stack(x = list.files(paste0(climatePath, crop), full.names=T)[i] , bands= 1:31) %>% 
      flip(., direction = 2) %>%
      t()
    
    crop(x = climate,extent(-180, 180, -50, 50))
    extent(climate) <- extent(-180, 180, -50, 50)
    crs(climate) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # rasterToPoints(climate[[1]]) %>% head # El valor que aparezca en las primeras filas es el NA que se toma para esa variable
    
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
    
    # =-=-=-=-=-= Run polynomial and GAM model
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
    
    ggsave(paste0(modelPolyPath, crop, '/polynomial_', names, '.png'))

    poly_GAM %>% 
      bind_cols(., rasts[,c(1,2)]) %>%
      ggplot(aes(x = x, y = y))  +
      geom_tile(aes(fill = GAM)) + 
      geom_polygon(data = shp , aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
      coord_equal() + theme_bw() +  scale_fill_distiller(palette = "Spectral") + 
      labs(x= 'Longitude', y = 'Latitude')
    
    ggsave(paste0(modelGAMpath, crop, '/GAM_', names, '.png'))
    
    # =-=-=-=-=-=- Write result tables in csv
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
