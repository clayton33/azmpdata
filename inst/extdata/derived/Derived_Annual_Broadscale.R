# code to prepare `Derived_Annual_Broadscale` dataset
cat('Sourcing Derived_Annual_Broadscale.R', sep = '\n')
library(dplyr)
library(tidyr)
library(readr)
library(usethis)
library(RCurl)
source('inst/extdata/read_physical.R')
# load physical data ----
## sea_surface_temperature_from_satellite ----
cat('    reading in satellite SST data', sep = '\n')
url_name <- "ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/physical/SSTsatellite/"
result <- getURL(url_name,
                 verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
filenames <- unlist(strsplit(result, "\r\n"))
### create dataframe list ----
d <- list()
for(i in 1:length(filenames)){
  cat(paste("Reading file", filenames[[i]]), sep = '\n')
  con <- url(paste0(url_name, filenames[[i]]))
  d[[i]] <- read.physical(con)
}
vardat <- unlist(lapply(d, function(k) as.numeric(k[['data']][['anomaly']]) + as.numeric(k[['climatologicalMean']])))
areaName <- unlist(lapply(d, function(k) rep(k[['regionName']], dim(k[['data']])[1])))
year <- unlist(lapply(d, function(k) k[['data']][['year']]))
df <- data.frame(year = year,
                 area = areaName,
                 sea_surface_temperature_from_satellite = vardat) # verify this is correct variable name
sstSatellite <- df
## north_atlantic_oscillation ----
cat('    reading in nao data', sep = '\n')

url_name <- "ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/physical/nao/"
result <- getURL(url_name,
                 verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
filenames <- unlist(strsplit(result, "\r\n"))
filenames <- filenames[grepl("nao_*", filenames)]
### create dataframe list ----
naodf <- list()
for(i in 1:length(filenames)){
  cat(paste("Reading file", filenames[[i]]), sep = '\n')
  con <- url(paste0(url_name, filenames[i]))
  k <- read.physical(con)
  naoType <- gsub('nao_(.*)_en.dat', '\\1', filenames[i])
  vardat <- as.numeric(k[['data']][['nao']]) + as.numeric(k[['climatologicalMean']])
  areaName <- NA
  year <- k[['data']][['year']]
  df <- data.frame(year = year,
                   area = areaName,
                   data = vardat)
  dataname <- paste("north_atlantic_oscillation", tolower(naoType), sep = '_')
  names(df)[names(df) == "data"] <- dataname
  naodf[[i]] <- df
}
### merge naodf together by year ----
for(i in 2:length(naodf)){
  if(i == 2){
    nao <- merge(x = naodf[[1]],
                 y = naodf[[i]],
                 by = c('year', 'area'),
                 all = TRUE)
  } else {
    nao <- merge(x = nao,
                 y = naodf[[i]],
                 by = c('year', 'area'),
                 all = TRUE)
  }
}
## temperature_at_sea_floor ----
cat('    reading in areas bottom temperature data', sep = '\n')
url_name <- "ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/physical/areas/"
result <- getURL(url_name,
                 verbose = TRUE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
filenames <- unlist(strsplit(result, "\r\n"))
### get relevant files ----
fn <- grep(filenames, pattern = 'areasTemperatureAnnualAnomaly\\w+\\.dat', value = TRUE)
### create dataframe list ----
d <- list()
for(i in 1:length(fn)){
  cat(paste("Reading file", fn[[i]]), sep = '\n')
  con <- url(paste0(url_name, fn[[i]]))
  d[[i]] <- read.physical(con)
}
vardat <- unlist(lapply(d, function(k) as.numeric(k[['data']][['temperatureAnomaly']]) + as.numeric(k[['climatologicalMean']])))
areaName <- unlist(lapply(d, function(k) rep(k[['areaName']], dim(k[['data']])[1])))
year <- unlist(lapply(d, function(k) k[['data']][['year']]))
df <- data.frame(year = year,
                 area = areaName,
                 temperature_at_sea_floor = vardat)
areasTemperature <- df
## density_gradient_0_50 ----
cat('    reading in areas data, density gradient', sep = '\n')
### find other files ----
cat('    reading in areas data', sep = '\n')
otherFiles <- filenames[!filenames %in% fn]
### define official names ----
official_names <- c('Densitygradient' = 'density_gradient_0_50',
                    'PracticalSalinity0m' = 'salinity_0',
                    'PracticalSalinity50m' = 'salinity_50',
                    'salinityGradient' = 'salinity_gradient_0_50',
                    'Temperature0m' = 'sea_temperature_0',
                    'Temperature50m' = 'sea_temperature_50',
                    'SigmaTheta0m' = 'sigmaTheta_0',
                    'SigmaTheta50m' = 'sigmaTheta_50')
areadfall <- list()
for(i in 1:length(otherFiles)){
  cat(paste("Reading file", otherFiles[[i]]), sep = '\n')
  con <- url(paste0(url_name, otherFiles[[i]]))
  k <- read.physical(con)
  vardat <- as.numeric(k[['data']][['anomaly']]) + as.numeric(k[['climatologicalMean']])
  variableName <- k[['variable']]
  variableName <- gsub('\\s', '', variableName) # remove any space in variable name
  variableDepth <- ifelse('depth' %in% names(k), k[['depth']], " ")
  variableDepth <- gsub('NA', "", variableDepth)
  variableNameFull <- official_names[match(paste0(variableName, variableDepth), names(official_names))]
  year <- k[['data']][['year']]
  areaName <- k[['areaName']]
  areaName <- gsub(x = areaName, pattern = 'Scotian Shelf', replacement = 'scotian_shelf_box')
  df <- data.frame(year = year,
                   area = areaName,
                   data = vardat)
  names(df)[names(df) == "data"] <- variableNameFull
  areadfall[[i]] <- df
}
### merge areadf together by year and area ----
for(i in 2:length(areadfall)){
  if(i == 2){
    areadf <- merge(x = areadfall[[1]],
                    y = areadfall[[i]],
                    by = c('year', 'area'),
                    all = TRUE)
  } else {
    areadf <- merge(x = areadf,
                    y = areadfall[[i]],
                    by = c('year', 'area'),
                    all = TRUE)
  }
}
### keep density gradient and surface variables ----
areadf <- areadf %>% select(year, area, density_gradient_0_50, sea_temperature_0, salinity_0)
areasOther <- areadf
## cold_intermediate_layer_volume & minimum_temperature_in_cold_intermediate_layer ----
cat('    reading in cold intermediate layer data', sep = '\n')
url_name <- "ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/physical/coldIntermediateLayer/"
result <- getURL(url_name,
                 verbose = TRUE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
filenames <- unlist(strsplit(result, "\r\n"))
# create dataframe list ----
d <- list()
for(i in 1:length(filenames)){
  cat(paste("Reading file", filenames[[i]]), sep = '\n')
  con <- url(paste0(url_name, filenames[[i]]))
  d[[i]] <- read.physical(con)
}
### extract data ----
vardat1 <- unlist(lapply(d, function(k) as.numeric(k[['data']][['volume']])))
vardat2 <- unlist(lapply(d, function(k) as.numeric(k[['data']][['minimumTemperature']])))
areaName <- unlist(lapply(d, function(k) rep(k[['areaName']], dim(k[['data']])[1])))
year <- unlist(lapply(d, function(k) k[['data']][['year']]))
### update areaName ----
areaName <- gsub(x = areaName, pattern = 'Scotian Shelf', replacement = 'scotian_shelf_grid')
### construct dataframe ----
df <- data.frame(year = year,
                 area = areaName,
                 cold_intermediate_layer_volume = vardat1,
                 minimum_temperature_in_cold_intermediate_layer = vardat2)
coldIntermediateLayer <- df
## temperature_at_sea_floor ----
cat('    reading in summer bottom temperature data', sep = '\n')

url_name <- "ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/physical/summerBottomTemperature/"
result <- getURL(url_name,
                 verbose = TRUE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
filenames <- unlist(strsplit(result, "\r\n"))
### create dataframe list ----
d <- list()
for(i in 1:length(filenames)){
  cat(paste("Reading file", filenames[[i]]), sep = '\n')
  con <- url(paste0(url_name, filenames[[i]]))
  d[[i]] <- read.physical(con)
}
### extract data
tasf <- unlist(lapply(d, function(k) as.numeric(k[['data']][['anomaly']]) + as.numeric(k[['climatologicalMean']])))
areaName <- unlist(lapply(d, function(k) rep(k[['divisionName']], dim(k[['data']])[1])))
year <- unlist(lapply(d, function(k) k[['data']][['year']]))

df <- data.frame(year = year,
                 area = areaName,
                 temperature_at_sea_floor = tasf)
summerBottomTemperature <- df
# assemble data ----
Derived_Annual_Broadscale <- dplyr::bind_rows(areasOther,
                                              areasTemperature,
                                              coldIntermediateLayer,
                                              summerBottomTemperature,
                                              nao, sstSatellite)
# save data ----
# save data to csv ----
readr::write_csv(Derived_Annual_Broadscale, "inst/extdata/csv/Derived_Annual_Broadscale.csv")
# save data to rda ----
usethis::use_data(Derived_Annual_Broadscale, overwrite = TRUE)
