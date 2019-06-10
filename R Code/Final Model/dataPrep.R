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

setwd("C:/Users/Marea Sing/Desktop/MIT808")

root = ""

# Load WB Indicators --------------------------------------------------------------------
euCountries <- read_excel("loadData.xlsx", sheet = "Country", col_names = F) %>% 
  as.matrix()

new_cache <- wbcache()

euList <- new_cache$countries %>% 
  filter(iso2c %in% euCountries) %>% 
  select(iso2c, country) %>% 
  as.data.frame()

startDate <- 1970
endDate <- 2016

vars <- read_excel("loadData.xlsx", sheet = "Vars", col_names = F) %>% 
  as.matrix() %>% 
  gsub("(", "\\(", ., fixed = T) %>% 
  gsub(")", "\\)", .,  fixed = T) %>% 
  gsub("$","\\$", ., fixed = T)

varList <- lapply(vars, function (x) wbsearch(x)[1,]) %>% 
  unlist(.) %>% 
  matrix(ncol = 2, byrow = T) %>% 
  as.data.frame(stringsAsFactors = F)

# adjusted for archived series:
varList <- varList %>% 
  mutate(V1 = ifelse(V1 == "GB.DOD.TOTL.GDP.ZS", "GC.DOD.TOTL.GD.ZS", V1),
         V1 = ifelse(V1 == "GB.TAX.TOTL.GDP.ZS", "GC.TAX.TOTL.GD.ZS", V1),
         V1 = ifelse(V1 == "BN.CAB.XOKA.GDP.ZS", "BN.CAB.XOKA.GD.ZS", V1),
         V1 = ifelse(V1 == "NY.GDP.MKTP.KN.87.ZG", "NY.GDP.MKTP.KD.ZG", V1))

rawIndicatorData <- wb(country = euList$iso2c,
                       indicator = varList$V1, 
                       startdate = startDate, 
                       enddate = endDate,
                       return_wide = TRUE)
rawIndicatorData$date <- as.numeric(rawIndicatorData$date)

write.csv(varList, paste0(root, "VarList.csv"),
          row.names = F)
write.csv(rawIndicatorData, paste0(root, "Raw WB Indicator Data.csv"),
          row.names = F)

# Log differences only required for Official exchange rate:
IndicatorData <- rawIndicatorData %>% 
  group_by(country) %>% 
  mutate(PA.NUS.FCRF = c(0,diff(log(PA.NUS.FCRF)))) %>% 
  as.data.frame()

# Load ECB crisis series --------------------------------------------------
crisis <- read_excel("loadData.xlsx", sheet = "ECB", col_names = T) %>% 
  mutate(YrStart = as.numeric(substr(Start,1,4)),
         YrNorm = ifelse(ReturnToNormality == "ongoing", 
                          endDate, 
                          as.numeric(substr(ReturnToNormality,1,4))+1)) %>% 
  arrange(Country, YrStart) %>% 
  as.data.frame()
  
numcrises <- table(crisis$Country)
crTemp <- data.frame()

for (i in 1:length(euCountries)) {
  cntry <- euCountries[i]
  temp <- cbind.data.frame(cntry, as.numeric(startDate:endDate), 0)
  colnames(temp) <- c("Country","Year","Crisis")
  count <- numcrises[names(numcrises) == cntry]
  crDates <- crisis %>% filter(Country == cntry)
  for (j in 1:count){
    temp <- temp %>% 
      mutate(Crisis = ifelse(Year <= crDates$YrNorm[j] & Year >= crDates$YrStart[j],1,Crisis))
  }
  crTemp <- rbind(crTemp, temp)
}

fullDB <- left_join(crTemp, IndicatorData, by = c("Year" = "date","Country" = "iso2c")) %>% 
  mutate(Crisis = as.factor(Crisis))

write.csv(fullDB, paste0(root, "Full Data.csv"),
          row.names = F)

