## Librerias
rm(list = ls())
library(glue)
library(sf)
library(raster)
library(tidyverse)



vars <- c("crops", "temperature", "precipitation","maize","rice", "swheat","wwheat")


path <- "D:/agmetgaps/" 
input <- glue("{path}{vars}.tif")
h <- purrr::map(.x = input, .f = raster)
output <- glue("{path}histogram_{vars}.png")




make_histograms <- function(r, out){
 
  
  name <- names(r)
  name <- glue("Histogram of {name}")
  r <- r[]
  png(file=out)
  hist(r, col="steelblue", breaks=10, main = name)
  dev.off()
  
}

purrr::map2(.x = h, .y = output, .f = make_histograms)