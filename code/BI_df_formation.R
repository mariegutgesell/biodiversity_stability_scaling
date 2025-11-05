##Benthic Invertebrate - Dataframe Formation

library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

#remotes::install_github("velofrog/readxlsb")
library(readxlsb)

##Read in master file with dataset info 
master <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "overview")


##Macroinvertebrate site information - lat, long etc. 
bi_site_df <- read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_MZB")

##what are all of the different sampling methods used?
sampling_methods <- bi_site_df %>%
  select(sampling.method) %>%
  unique()

##do 
