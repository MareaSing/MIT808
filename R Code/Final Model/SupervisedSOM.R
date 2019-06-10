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

set.seed(1234)
inTrain <- createDataPartition(db$Crisis,
                                   p = 0.7,
                                   list = FALSE)
y_train <- db[inTrain,] %>% 
  select(Crisis)
x_train <-  db[inTrain,]  %>% 
  select(c(6:ncol(db)))

#rownames(x) <- db$Year

# remove variables that have too many missing values
fracNA <- colSums(is.na(x_train))/nrow(x_train)
xpart <- x_train[,fracNA < 0.5]

preProc_cs <- preProcess(xpart, method = c("center", "scale", "zv"))
x_train_cs <- predict(preProc_cs, xpart)

numvars <- ncol(x_train_cs)
N = nrow(x_train_cs)
gridsize <- 5*sqrt(numvars)

som_grid <- somgrid(xdim = ceiling(sqrt(gridsize)), 
                    ydim = ceiling(sqrt(gridsize)), 
                    topo="hexagonal")

set.seed(1234)
xyf_model <- xyf(X = as.matrix(x_train_cs), Y = as.matrix(y_train), 
                 grid=som_grid,
                 maxNA.fraction = 0.5,
                 rlen = 500,
                 keep.data = T)

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

png(filename = paste0(root, "Supervised/Changes_.png"))
plot(xyf_model, type = "changes",
     main = paste0("Changes"))
dev.off()

som.cc <- as.data.frame(round(getCodes(xyf_model,2),1)) %>% 
  mutate(predictions = case_when(
    Crisis == 1 ~ "Crisis",
    Crisis == 0 ~ "Normal",
    Crisis < 1 & Crisis > 0 ~ "Above-Normal"),
    predictions = as.factor(predictions)
  )

png(filename = paste0(root, "Supervised/DistPlot_.png"))
Umat <- plot(xyf_model, 
             type= "dist.neighbours", 
             main = paste0("SOM neighbour distances "),
             shape = "straight")
add.cluster.boundaries(xyf_model, as.integer(som.cc$predictions))
dev.off()

png(filename = paste0(root, "Supervised/QualityPlot_.png"))
similarities <- plot(xyf_model, 
                     type="quality", 
                     palette.name = coolBlueHotRed,
                     shape = "straight",
                     main = paste0("Quality "))
add.cluster.boundaries(xyf_model, as.integer(som.cc$predictions))
dev.off()

png(filename = paste0(root, "Supervised/Mapping_.png"))
mapping <- plot(xyf_model, 
                type="mapping", 
                bgcol = c("orange","red","lightgreen")[as.integer(som.cc$predictions)],
                shape = "straight",
                labels = db %>% 
                  mutate(lab = ifelse(Crisis == 1 & Year %in% c(2005,2009), 
                                      paste0(Country," ",Year), ".")) %>% 
                  select(lab) %>% 
                  as.matrix(),
                main = paste0("Mapping "))
add.cluster.boundaries(xyf_model, as.integer(som.cc$predictions))
dev.off()

# change this to reflect var names

numplots <- ceiling(numvars/9)

for (plot in 1:numplots){
  png(filename = paste0(root, "Supervised/Plot",plot,"_.png"),
      width = 1440, height = 1440, units = "px",)
  par(mfrow = c(3,3), oma = c(0, 0.1, 0.1, 0.1), mar = c(0.1,0.1,0.1,0.1),
      cex.main = 2.75)
  
  for(var in ((plot-1)*9+1):min(9*plot,numvars)){
    plot(xyf_model, type = "property",
         property = getCodes(xyf_model,1)[,var],
         main = varList$V2[varList$V1 ==colnames(x_train_cs)[var]],
         palette.name = coolBlueHotRed,
         shape = "straight")
    add.cluster.boundaries(xyf_model,  as.integer(som.cc$predictions))
  }
  dev.off()  
  
}

# Train & Test fit ----------------------------------------------------------------
xyf_pred_train <- predict(xyf_model)
predictions_train <- as.data.frame(xyf_pred_train$predictions[[2]]) %>%
  mutate(predictions = case_when( 
    Crisis >= 0.8  ~ "1",
    Crisis < 0.8 ~ "0",
    is.na(Crisis) ~ "0")) %>% 
  select(predictions) 

# compare predicted outcome and true outcome
confusionMatrix(as.factor(as.matrix(predictions_train)),
                as.factor(y_train[as.numeric(rownames(xyf_model$data[[1]])),]))

# test data

x_test <-  db[-inTrain,]  %>% 
  select(c(6:ncol(db)))

fracNA <- colSums(is.na(x_test))/nrow(x_test)
xpart <- x_test[,fracNA < 0.5]

preProc_cs <- preProcess(xpart, method = c("center", "scale", "zv"))
x_test_cs <- predict(preProc_cs, xpart)

tempTest <- cbind(db[-inTrain, "Crisis"], x_test_cs) 

# predict the outcome on a test set
xyf_pred_test <- predict(xyf_model, 
                    newdata = list(as.matrix(x_test_cs)),
                    whatmap = 1)

# predictions to factor:
predictions_test <- as.data.frame(xyf_pred_test$predictions[[2]]) %>%
  mutate(predictions = case_when( 
    Crisis >= 0.8  ~ "1",
    Crisis < 0.8 ~ "0",
    is.na(Crisis) ~ "0")) %>% 
    select(predictions) 

# compare predicted outcome and true outcome
confusionMatrix(as.factor(as.matrix(predictions_test)),
                as.factor(tempTest[,1]))


# Check with RF VIF -------------------------------------------------------
x_full <-  db %>% 
  select(c(6:ncol(db)))

fracNA <- colSums(is.na(x_full))/nrow(x_full)
x_full <- x_full[,fracNA < 0.5]

preProc_cs <- preProcess(x_full, method = c("center", "scale", "zv"))
x_full_cs <- predict(preProc_cs, x_full)

tempDB <- cbind(Crisis =db$Crisis, x_full) %>% 
  na.omit()

rf_model <- train(as.factor(Crisis) ~.,
                  tuneLength = 10,
                  data = na.omit(as.matrix(tempDB)), 
                  method = "ranger",
                  importance = 'impurity'
)

vImp <- varImp(rf_model)$importance %>%
  mutate("variable" = rownames(varImp(rf_model)$importance)) %>%
  arrange(.,desc(Overall)) %>% 
  left_join(varList, by = c("variable" = "V1"))

