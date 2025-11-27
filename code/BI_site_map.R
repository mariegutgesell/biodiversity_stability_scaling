##site map 


library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

#remotes::install_github("velofrog/readxlsb")
library(readxlsb)

library(sf)
library(mapview)
library(dplyr)
library(terra)
library(tools)
library(stringr)
library(leaflet)
library(leafem)
library(tidyr)


##set working directory 

##Read in master file with dataset info 
master <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "overview")


##Macroinvertebrate site information - lat, long etc. 
bi_site_df <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_MZB") 

##Start by just plotting out a map of all the sites
sites_coord <- st_as_sf(bi_site_df, coords = c("Longitude_X", "Latitude_Y"), crs = 4326)
site_map <- mapview(sites_coord, map.types = "Esri.WorldTopoMap", legend = TRUE,  alpha = 1, alpha.regions = 1, cex = 4)
site_map


##Looking at some of the datafiles that have potentially overlapping data
ger_018_coords <- sites_coord %>%
  filter(Dataset.ID %in% c("GER_018_MZB_LO")) %>%
  filter(Site_ID_original %in% ger_018_lo1$Site.ID)
mapview(ger_018_coords, map.types = "Esri.WorldTopoMap", legend = TRUE,  alpha = 1, alpha.regions = 1, cex = 4)
