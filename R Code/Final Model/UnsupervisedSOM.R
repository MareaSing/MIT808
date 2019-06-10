# Load libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(texreg)
library(tidyr)
library(lubridate)
library(tidyverse)
library(grid)
library(gridExtra)
library(kohonen)
library(caret)
library(wbstats)
library(RColorBrewer)

setwd("C:/Users/Marea Sing/Desktop/MIT808")
rm(list = ls())

root = ""

db <- read.csv(paste0(root, "Full Data.csv"), stringsAsFactors = F)
varList <- read.csv(paste0(root, "VarList.csv"), stringsAsFactors = F)

for (year in unique(db$Year)) {
  y <- db %>% 
    filter(Year == year) %>% 
    select(Crisis)
  x <-  db %>% 
    filter(Year == year) %>% 
    select(c(6:ncol(db)))
  
  rownames(x) <- unique(db$Country)
  
  # remove variables that have too many missing values
  fracNA <- colSums(is.na(x))/nrow(x)
  xpart <- x[fracNA < 0.5]
  
  preProc_cs <- preProcess(xpart, method = c("center", "scale", "zv"))
  x_cs <- predict(preProc_cs, xpart)
  
  numvars <- ncol(x_cs)
  N = nrow(x_cs)
  gridsize <- 3*sqrt(numvars)
  
  som_grid <- somgrid(xdim = ceiling(sqrt(gridsize)), 
                      ydim = ceiling(sqrt(gridsize)), 
                      topo="hexagonal")
  
  set.seed(1234)
  som_model <- som(as.matrix(x_cs), 
                   grid = som_grid,
                   maxNA.fraction = 0.5,
                   rlen = 500,
                   keep.data = T)
  
  coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
  
  png(filename = paste0(root, "Unsupervised/Changes/Changes_", year,".png"))
  plot(som_model, type = "changes",
       main = paste0("Changes ",year))
  dev.off()
  
  som.hc <- cutree(hclust(object.distances(som_model, "codes")), 5)
  
  png(filename = paste0(root, "Unsupervised/Distance/DistPlot_", year,".png"))
  Umat <- plot(som_model, 
               type= "dist.neighbours", 
               main = paste0("SOM neighbour distances ", year),
               shape = "straight")
  add.cluster.boundaries(som_model, som.hc)
  dev.off()
  
  png(filename = paste0(root, "Unsupervised/Quality/QualityPlot_", year,".png"))
  similarities <- plot(som_model, 
                       type="quality", 
                       palette.name = coolBlueHotRed,
                       shape = "straight",
                       main = paste0("Quality ", year ,"_", mean(som_model$distances)))
  add.cluster.boundaries(som_model, som.hc)
  dev.off()
  
  png(filename = paste0(root, "Unsupervised/Mapping/Mapping_", year,".png"))
  mapping <- plot(som_model, 
                  type="mapping", 
                  bgcol = brewer.pal((max(som.hc)), "Pastel1")[som.hc],
                  shape = "straight",
                  labels = db %>% 
                    filter(Year == year) %>% 
                    mutate(lab = ifelse(Crisis == 1, Country, "x")) %>% 
                    select(lab) %>% 
                    as.matrix(),
                  main = paste0("Mapping ",year))
  add.cluster.boundaries(som_model, som.hc)
  dev.off()
  
  # change this to reflect var names
  
  numplots <- ceiling(numvars/9)
  
  for (plot in 1:numplots){
    png(filename = paste0(root, "Unsupervised/Variables/ ",year,"_Plot",plot,".png"),
        width = 1440, height = 1440, units = "px",)
    par(mfrow = c(3,3), oma = c(0, 0.1, 0.1, 0.1), mar = c(0.1,0.1,0.1,0.1),
        cex.main = 2.75)
    
    for(var in ((plot-1)*9+1):min(9*plot,numvars)){
      plot(som_model, type = "property",
         property = getCodes(som_model)[,var],
         main = varList$V2[varList$V1 ==colnames(x_cs)[var]],
         palette.name = coolBlueHotRed,
         shape = "straight")
      add.cluster.boundaries(som_model, som.hc)
    }
    dev.off()  

  }
  

  
}

