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


















# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Models with panel data

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=




## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
## Packages
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ##
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
suppressMessages(if(!require(rgeos)){install.packages('rgeos'); library(rgeos)} else {library(rgeos)})
suppressMessages(if(!require(sf)){install.packages('sf'); library(sf)} else {library(sf)})
# Geographically Weighted Regression
suppressMessages(if(!require(GWmodel)){install.packages('GWmodel'); library(GWmodel)} else {library(GWmodel)}) # 
suppressMessages(if(!require(plm)){install.packages('plm'); library(plm)} else {library(plm)}) # 







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
names <- strsplit(list.files(paste0(climatePath, crop))[i], ".nc$") %>%  unlist
print(paste0('Crop: ', crop, ' --- Season: ', seasonCrop, ' --- Variable: ', names))



# Temporarily st_read(dsn = paste0(shpPath, 'mapa_mundi.shp'))
shp_colombia <- st_read(dsn = paste0(shpPath, 'all_countries.shp')) %>%
  as('Spatial') %>% crop(extent(-180, 180, -50, 50)) 

adm <- shp_colombia[shp_colombia$NAME == 'Colombia', ]

# first idea: wheat automatization - i believe this crop run with all continent


# shp_colombia$UNREG2 %>% unique %>% as.tibble() %>% View
# adm <- shp_colombia[shp_colombia$UNREG2 %in%  c('Americas'),]


iizumi <- list.files(paste0(IizumiPath, seasonCrop), pattern = 'gap.tif$', full.names = TRUE)[-1] %>% 
  stack() %>% 
  crop(extent(-180, 180, -50, 50))



climate <- stack(x = list.files(paste0(climatePath, crop), full.names=T)[i] , bands= 2:31) %>% 
  flip(., direction = 2) %>%
  t()

extent(climate) <- extent(-180, 180, -50, 50)
crs(climate) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"





#test <- rasterToPoints(climate)
#test %>% summary
#sum(test[,3] == -999)


ad <- raster::getData('GADM', country='COL', level=1)

ad$NAME_1


idea <- rasterize(ad, iizumi[[1]]  %>% crop(., ad)  %>% mask(., ad), field = 'OBJECTID' )


# =-=-=-=-=-=-=
Rp1 <- stack(climate, iizumi) %>% crop(., ad)  %>% mask(., ad) 
Rp <- stack(idea, Rp1) %>% rasterToPoints() 

#Rp %>% as.tibble()
#Rp_climate <- Rp[[1:30]] %>% setNames(paste0('y',1982:2011))
#Rp_iizumi <- Rp[[31:60]] %>% setNames(paste0('y', 1982:2011))

# plot(Rp_iizumi)


test1  <- Rp %>% 
  as.tibble() %>% 
  mutate(id= 1:length(x)) %>%  
  na.omit() %>% 
  .[, c(ncol(.), 3 ,  1:2, 4:33)] %>%  setNames(c('id', 'polygon','x','y', paste0('y',1982:2011)) )  %>% 
  gather(year, climate, 'y1982':'y2011') %>% 
  separate(year,  c("key", "year"), "y") %>% 
  dplyr::select(-key)

test <-   Rp %>% as.tibble() %>% 
  mutate(id= 1:length(layer.1)) %>%  
  na.omit() %>%
  .[, c(ncol(.), 3, 1:2, 34:(ncol(.)-1))] %>% 
  setNames(c('id','polygon', 'x','y', paste0('y',1982:2011)) )%>% 
  gather(year, iizumi, 'y1982':'y2011') %>% 
  separate(year,  c("key", "year"), "y") %>% 
  dplyr::select(-key)


sp_data <- ad


coor <-  coordinates(ad) %>% as.tibble() %>% setNames(c('x', 'y')) %>% mutate(polygon = 1:length(x))

# sp_data <- rasterToPolygons(Rp_iizumi)

# test1 <- Rp_climate %>% rasterToPoints() 
# test <- Rp_iizumi %>% rasterToPoints() 


#test <- test %>% 
#  as.tibble %>%
#  mutate(id = 1:length(x)) %>% 
#  gather(year, iizumi, 'y1982':'y2011') %>% 
#  separate(year,  c("key", "year"), "y") %>% 
#  dplyr::select(-key)

#test1 <- test1 %>% 
#  as.tibble %>%
#  mutate(id = 1:length(x)) %>% 
#  gather(year, climate, 'y1982':'y2011') %>% 
#  separate(year,  c("key", "year"), "y") %>% 
#  dplyr::select(-key)


# proof <- inner_join(test, test1 %>% dplyr::select(-x,-y), by = c('id', 'polygon','year'))


proof <- inner_join(test, test1 %>% dplyr::select(-x,-y), by = c('id', 'polygon','year')) %>% 
  group_by(polygon, year) %>% summarise(iizumi = mean(iizumi), climate =  mean(climate)) %>% 
  ungroup() 







proof <- proof %>% nest(-polygon) %>% 
  inner_join(., coor, by = 'polygon') %>% unnest








states.m <- list(x= proof%>% dplyr::select(x),   y =  proof%>% dplyr::select(y),  
                 range = extent(ad) %>% as.vector(), names = proof %>% dplyr::select(polygon) )

yrs <- 1982:2011
time <- as.POSIXct(paste(yrs, "-01-01", sep = ""))




### 

tst <- proof %>% dplyr::select(polygon) %>% unique  %>% pull(1)
sp_data <- ad[ad@data$OBJECTID %in% tst,]



plot(sp_data)



Produc.st <-spacetime::STFDF(sp = sp_data,  time = time, data = proof)


zz <- plm(log(iizumi) ~ log(climate),
          data = as.data.frame(Produc.st), index = c("polygon", "year"))

dim(as.data.frame(Produc.st)) # 253890     42

summary(zz)$r.squared

























