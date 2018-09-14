## Librerias
rm(list = ls())
library(glue)
library(sf)
library(raster)
library(tidyverse)



vars <- c("crops", "climate", "temperature", "precipitation","maize","rice", "swheat","wwheat")


path <- "D:/agmetgaps/" 
input <- glue("{path}{vars}.tif")
h <- purrr::map(.x = input, .f = raster)
output <- glue("{path}{vars}.png")




make_histograms <- function(r, out){
 
  
  name <- names(r)
  name <- glue("Histogram of {name}")
  r <- r[]
  png(file=out)
  hist(r, col="steelblue", breaks=10, main = name)
  dev.off()
  
}

purrr::map2(.x = h, .y = output, .f = make_histograms)


spdf <-rasterToPoints(h[[5]])
df <- as.data.frame(spdf)

(cl <- kmeans(spdf[, 3], 3))

df <- data.frame(df, cl$cluster)
ggplot() +  
  geom_tile(data=df, aes(x=x, y=y, fill= as.factor(cl.cluster))) + 
  scale_fill_viridis(discrete = T) +
  coord_equal() +
  theme_bw()
