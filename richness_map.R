require(raster)
require(rgdal)

rasterDummy <- raster(nrow=12000, ncol=43200)

res <- 0.008333333333

xMin = -180
yMin = -50

xMax <- xMin + (rasterDummy@ncols * res)
yMax <- yMin + (rasterDummy@nrows * res)

rasExt <- extent(xMin,xMax,yMin,yMax)

rasterDummy@extent <- rasExt

rasterDummy[]<- 1:ncell(rasterDummy)

plot(rasterDummy)

