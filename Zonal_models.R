# =-=-=-=  2.2. Yield gap vs Clima -- Climate relationship models with Gaps.
#               (Spatial models with geographical poderation).

rm(list=ls())

## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
## Packages
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
suppressMessages(if(!require(rgeos)){install.packages('rgeos'); library(rgeos)} else {library(rgeos)})
suppressMessages(if(!require(sf)){install.packages('sf'); library(sf)} else {library(sf)})
# Exploring Spatial Heterogeneity using Geographically Weighted Models
suppressMessages(if(!require(GWmodel)){install.packages('GWmodel'); library(GWmodel)} else {library(GWmodel)}) # 




# =-=-=-=-=-=-=-=-=-= Routes
rootPath <- '/mnt/workspace_cluster_9/AgMetGaps/'
calendarPath <-  paste0(rootPath, '3_monthly_climate_variability/katia_calendar/')
climatePath <- paste0(rootPath, '3_monthly_climate_variability/katia_climate/') 
IizumiPath <- paste0(rootPath, '1_crop_gaps/iizumi_processed/')
modelsPath <- paste0(rootPath, '3_monthly_climate_variability/Spatial_models/')
shpPath <- paste0(rootPath, '0_general_inputs/shp/')


# =-=-=-= Preliminar parameters 
crop <- 'Wheat'
seasonCrop <- 'wheat_spring'


i <- 1

names <- names <- strsplit(list.files(paste0(climatePath, crop))[i], ".nc$") %>%  unlist
print(paste0('Crop: ', crop, ' --- Season: ', seasonCrop, ' --- Variable: ', names))



# Other package for explore: library(spgwr)

# =-=- on this part we need to use the calendar for to determine what years to use  in the modelation


iizumi <- list.files(paste0(IizumiPath, seasonCrop), pattern = 'gap.tif$', full.names = TRUE)[i] %>% 
  stack() %>% 
  crop(extent(-180, 180, -50, 50))



climate <- stack(x = list.files(paste0(climatePath, crop), full.names=T)[i] , bands= i) %>% 
  flip(., direction = 2) %>%
  t()

extent(climate) <- extent(-180, 180, -50, 50)
crs(climate) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"





# =-=-=-=-=-=- stack(climate, iizumi) %>% plot



# Temporarily 
shp_colombia <- st_read(dsn = paste0(shpPath, 'mapa_mundi.shp')) %>%
  as('Spatial') %>% crop(extent(-180, 180, -50, 50))


library(maptools)

adm <- getData('GADM', country='AUS', level=1)
adm %>% plot 	

plot(iizumi) 
plot(adm , add = TRUE)


# proof <- stack(climate, iizumi) %>% rasterToPoints() 
# names(proof) <-  c('long', 'lat', 'climate', 'yield')

# proof <- proof[-which( proof[, 4] %>%  is.na()), ]
# proof <-  proof[-which( proof[, 3] %>%  is.na()), ]

system.time(
  Rp1 <-  stack(climate, iizumi) %>% crop(., adm) %>%  rasterToPolygons(., na.rm	= TRUE)  
)

Data.scaled <- scale(as.data.frame(Rp1)) # sd


Rp2 <- stack(climate, iizumi) %>% crop(., adm) %>% rasterToPoints()
Rp2 <- Rp2[-which(Rp2[,4] %>% is.na()), ] %>% data.frame()
Rp2 <- SpatialPointsDataFrame(coords = Rp2[,1:2], data = Rp2[,3:4])


# =-=-=-= Basic GW regression

linmod <- lm(yield_1981_gap~layer,data=Rp2) # Store the regression model to use in a plot later
summary(linmod)$adj.r.squared



plot(yield_1981_gap~layer,data=Rp2,col = 'red', pch = 19)
abline(linmod )



library(mgcv)
gam_mod <- gam(yield_1981_gap~s( layer, fx = TRUE),data=Rp2) # Store the regression model to use in a plot later
summary(gam_mod)$dev.expl  





# =-=-=-=

plot(adm)
plot(Rp2, pch=16, col=adjustcolor('navyblue',alpha.f=0.5),add=TRUE)

DM <- gw.dist(dp.locat=coordinates(Rp2))



grd <- SpatialGrid(GridTopology(c(112,160), c(-56,-8), c(20,20)))

system.time(gwr.res <- gwr.basic(yield_1981_gap~layer, data=Rp2,  bw=100, dMat=DM,kernel='gaussian'))


