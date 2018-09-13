## Librerias
rm(list = ls())
library(glue)
library(sf)
library(raster)
library(tidyverse)



var <- c("crops", "temperature", "precipitation","maize","rice", "swheat","wwheat")
var <- var[1]

#make_histograms <- function(var){
  path <- "D:/agmetgaps/"
  input <- glue("{path}{var}.tif")
  output <- glue("histogram_{var}.png")
  name <- glue("Histogram of {var}")
  h <- raster(input)
  h <- h[]
  png(file=output)
  hist(h, col="steelblue", breaks=10, main=name)
  dev.off()
#}
