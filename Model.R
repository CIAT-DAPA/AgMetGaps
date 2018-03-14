##### Climate relationship models with Gaps 

## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
## Packages
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
require(tidyverse)
require(raster)
require(rgdal)
require(ncdf4)
require(rgeos)
require(sf)
require(mgcv)
# require(future)
# require(doFuture)

# =-=-=-=-=-=-=-=-=-= Routes
rootPath <- '//dapadfs/Workspace_cluster_9/AgMetGaps/'
calendarPath <-  paste0(rootPath, '3_monthly_climate_variability/katia_calendar/')
climatePath <- paste0(rootPath, '3_monthly_climate_variability/katia_climate/') 
IizumiPath <- paste0(rootPath, '1_crop_gaps/iizumi_processed/')
modelsPath <- paste0(rootPath, '3_monthly_climate_variability/models/')
modelPolynomial <- paste0(modelsPath, 'polynomial/')
modelGAM <- paste0(modelsPath, 'GAM/')
shpPath <- paste0(rootPath, '0_general_inputs/shp/')

# =-=-=-=-=-= Polynomial model -- Function 
model <- function(.x , .y){
  if(.x == 0){
    summary(lm(.y$yield ~ .y$value + I(.y$value^2)))$r.squared 
  }else{
    summary(lm(.y$yield[-1] ~.y$value[-31] + I(.y$value[-31]^2)))$r.squared 
  }
}

### GAM
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

calcModels <- function(crop, seasonCrop){
  # =-=-=-=-=-=-=-=-=-= Calendar (raster)
  calendar <- list.files(paste0(calendarPath, crop), pattern = 'Int.tif$', full.names = TRUE) %>%
    stack() %>% 
    crop(extent(-180, 180, -50, 50))

  # =-=-=-=-=-=-=-=-= read all data sets
  
  # =-=-=-=-=-= read gap data
  iizumi <- list.files(paste0(IizumiPath, seasonCrop), pattern = 'gap.tif$', full.names = TRUE) %>% 
    stack() %>% 
    crop(extent(-180, 180, -50, 50))
  
  for(i in 1:6){
    # =-=-=-=-=-= read climate data
    names <- strsplit(list.files(paste0(climatePath, crop))[i], ".nc$") %>%  unlist
    
    print(paste0('Crop: ', crop, ' - Season: ', seasonCrop, ' - Variable: ', names))
    
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
    
    # =-=-=-=-=-= Run one model 
    system.time(
    rsquare <- proof %>%
      mutate(model = purrr::map2(.x = year_start, .y = data, .f = model) ) %>%
      dplyr::select(ID, model) %>% unnest
      # mutate(GAM = purrr::map2(.x = year_start, .y = data, .f = gam_model))  %>%
      # dplyr::select(ID, model, GAM) %>% unnest
    )

    rsquareTable <- rasts[, c(68, 1:2)] %>%
      inner_join(.,rsquare) %>%
      dplyr::select(x,y,model)

    shp <- read_sf(paste0(shpPath, 'mapa_mundi.shp')) %>%
      as('Spatial') %>%
      crop(extent(-180, 180, -50, 50))
    
    rsquare %>% 
        bind_cols(., rasts[,1:2]) %>%
        ggplot(aes(x = x, y = y))  +
        geom_tile(aes(fill = model)) + 
        geom_polygon(data = shp , aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
        coord_equal() + theme_bw() +  scale_fill_distiller(palette = "Spectral") + 
        labs(x= 'Longitude', y = 'Latitude')
    
    ggsave(paste0(modelPolynomial, crop, '/', names, '.png'))
    
    write.csv(x = rsquareTable, file = paste0(modelPolynomial, crop, '/',  names, '.csv'))
    
    tmpRaster <- raster(nrow=1200,ncol=4320)
    extent(tmpRaster) <- extent(-180, 180, -50, 50)
    coordinates(rsquareTable) <- ~x+y
    resultRaster <- rasterize(rsquareTable, tmpRaster , rsquareTable$model)
    
    writeRaster(x = resultRaster, file = paste0(modelPolynomial, crop, '/', names, '.tif'), format="GTiff", overwrite=TRUE)
  }
}

calcModels('Maize', 'maize_major')
calcModels('Rice', 'rice_major')
calcModels('Wheat', 'wheat_spring')
