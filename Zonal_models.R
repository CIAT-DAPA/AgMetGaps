# =-=-=-=  2.2. Yield gap vs Clima -- Climate relationship models with Gaps.
#               (Spatial models with geographical poderation).


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
crop <- 'Maize'
seasonCrop <- 'maize_major'



i <- 1

names <- names <- strsplit(list.files(paste0(climatePath, crop))[i], ".nc$") %>%  unlist
print(paste0('Crop: ', crop, ' --- Season: ', seasonCrop, ' --- Variable: ', names))




# Temporarily st_read(dsn = paste0(shpPath, 'mapa_mundi.shp'))
shp_colombia <- st_read(dsn = paste0(shpPath, 'all_countries.shp')) %>%
  as('Spatial') %>% crop(extent(-180, 180, -50, 50)) 

# shp_colombia$UNREG2 %>% unique %>% as.tibble() %>% View
# adm <- shp_colombia[shp_colombia$NAME %in%  c('Mexico'),]
adm <- shp_colombia[shp_colombia$UNREG1 %in%  c('Northern America'),]


plot(adm)
ext1 <- c(extent(adm)[1:2], extent(adm)[3], 40)
adm <- crop(adm, ext1)



# Other package for explore: library(spgwr)

# =-=- on this part we need to use the calendar for to determine what years to use  in the modelation


iizumi <- list.files(paste0(IizumiPath, seasonCrop), pattern = 'gap.tif$', full.names = TRUE)[30] %>% 
  stack() %>% 
  crop(extent(-180, 180, -50, 50))



climate <- stack(x = list.files(paste0(climatePath, crop), full.names=T)[i] , bands= 30) %>% 
  flip(., direction = 2) %>%
  t()

extent(climate) <- extent(-180, 180, -50, 50)
crs(climate) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


test <- rasterToPoints(climate)
test %>% summary
sum(test[,3] == -999)




# =-=-=-=-=-=- stack(climate, iizumi) %>% plot

Rp2 <- stack(climate, iizumi) %>% crop(., adm)  %>% mask(., adm) %>% rasterToPoints()  # 
Rp2 <- Rp2[-which(Rp2[,4] %>% is.na(.)) , ] %>% data.frame()
Rp2 <- Rp2[-which(Rp2[,3] == -999) , ] %>% data.frame()

Rp2 <- SpatialPointsDataFrame(coords = Rp2[,1:2], data = Rp2[,3:4])



# =-=-=-= Basic GW regression

linmod <- lm(yield_1981_gap~layer,data=Rp2) # Store the regression model to use in a plot later
summary(linmod)$adj.r.squared
#  0.004196895



library(mgcv)
gam_mod <- gam(yield_1981_gap~s( layer, fx = TRUE),data=Rp2) # Store the regression model to use in a plot later
summary(gam_mod)$dev.expl  
#0.1011333


(76723 / 540900) * 100




# =-=-=-=

#plot(adm)
#plot(Rp2, pch=16, col=adjustcolor('navyblue',alpha.f=0.5),add=TRUE)


# compute the distances between the points on the grid where βj(u,v) 
DM <- gw.dist(dp.locat=coordinates(Rp2), p =  3)


# conectar con el resto
bw.gwr.1 <- bw.gwr(yield_2010_gap ~ layer,
                   data = Rp2, 
                   kernel = "bisquare", dMat = DM)
print(bw.gwr.1)
# 20
#bw.gwr.1 <- 25
system.time(gwr.res <- gwr.basic(  formula = yield_2010_gap~layer, data = Rp2,
                                   bw = bw.gwr.1, dMat = DM,  kernel = "bisquare"))


gwr.res$GW.diagnostic$gwR2.adj

gwr.res$SDF$Local_R2 %>% boxplot


Coords <- coordinates(gwr.res$SDF) %>%
  as_tibble %>% 
  rename(long = x, lat = y)


df_data <- bind_cols(as_tibble(gwr.res$SDF), Coords)

rasterize_masa <-  function(var, data, raster){
  points <- data %>%
    dplyr::select(long, lat) %>%
    data.frame 
  
  
  vals <- data %>%
    dplyr::select(!!var) %>%
    magrittr::extract2(1)
  
  y <- rasterize(points, raster, vals, fun = sum)
  return(y)}


p <- rasterize_masa('residual', df_data , iizumi %>% crop(., adm))

#plot(p)
#plot(adm, add = T)
ggplot(df_data, aes(long, lat, fill = Local_R2)) + 
  geom_tile() +   
  scale_fill_gradientn(colours = rev(terrain.colors(10)),  limits=c(0,1)) +
  geom_polygon(data = adm, aes(x=long, y = lat, group = group), color = "gray", fill=NA) + 
  #geom_polygon(data = shp_colombia, aes(x=long, y = lat, group = group), color = "black", fill=NA) + 
  theme_bw() + coord_fixed()


write.csv(df_data, paste0(modelsPath, 'USA_S.csv'))




print(paste0('Crop: ', crop, ' --- Season: ', seasonCrop, ' --- Variable: ', names))
print(bw.gwr.1)

ggsave(paste0(modelsPath, 'Wheat_FP_SSA_16.png'))
df_data %>% write.csv(paste0(modelsPath, 'Wheat_FP_SSA_16.csv'))


