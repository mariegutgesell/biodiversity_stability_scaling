#this code creates unique site and dataset IDs for the Master table site list and checks for any missing data 

###---Load libraries a
library(Hmisc)
#remotes::install_github("velofrog/readxlsb")
library(readxlsb)
library(tidyverse)

###1) ---Load the overview, site list, and country codes from the master table -----------------
#Site list
sites<-read_xlsb("data/raw/mastertable/1_MASTERTABLE.xlsb", sheet = "samples1_MZB") %>%#load the list for your taxonomic group
  filter(ecosystem == "lotic") #%>% #uncomment if you want to filter to specific ecosystem here 
  #filter(fulfills.requirement == "yes") 
#Overview
over<-read_xlsb("data/raw/mastertable/1_MASTERTABLE.xlsb", sheet = "overview") %>%
  filter(Taxa.group == "macroinvertebrates", ecosystem == "lotic") ##filter to desired taxonomic group and ecosystem

#Country codes
code<-read_xlsb("data/raw/mastertable/1_MASTERTABLE.xlsb", sheet = "Countrycodes") %>%
  select(countrycode:X3.letter.Code) %>%
  rename(Country = "country")

###---Load all dataset csvs, set working directory to the directory with the unprocessed data for your taxonomic group
file.list <- list.files(path = paste0(getwd(), "/data/raw/unprocessed_csv"), pattern='\\.csv$', full.names = TRUE)
file.list <- file.list[!grepl("LE", basename(file.list))] ##filter out lentic sites
taxa.files <- sapply(file.list, read.csv, simplify = FALSE, sep = ";", fileEncoding="latin1")
taxa.files <- setNames(taxa.files, basename(file.list))

##2) Source helper functions and tibbles -------------
source("code/data_processing/helper_functions.R")

##3) Generic cleaning ---------------- 
sites_clean <- sites %>%
  { if (!exists("owner_aliases")) . else fix_data_owner_names(., owner_aliases) } %>%##make sure the data owner names match in the overview and site list 
  infer_ecosystem_from_dataset_id() %>% ##make sure ecosystem is correctly indicated based on the dataset id 
  add_provider_and_country(over, code) %>% ##add provider number and country codes 
  mutate(Site_ID_original = trimws(Site_ID_original))  #trim any blanks/white spaces from site names 

##4) Manual dataset specific fixes (if present) -----------
##these are generated following diagnostics to try and resolve issues/mismatches between raw csv data and site list 
sites_clean <- sites_clean 

##5) Attach filenames and IDs -----------
taxa_lookup <- build_taxa_lookup(taxa.files, over)

##Join file name to site df by original site id from the raw unprocessed data (should theoretically be the same)
sites_clean_2 <- left_join(sites_clean, taxa_lookup, by = c("Site_ID_original", "Provider.Number", "ecosystem", "Country"))

##add new unique ids and dataset ids 
sites_clean_3 <- make_ids(sites_clean_2) %>%
  select(file_name, Dataset.ID, Unique.ID, Dataset.ID_marie, Unique.ID_marie, Site.ID_unprocessed, origin:Country, countrycode, X3.letter.Code, Latitude_Y:ecosystem, TG_short, ecosys_short, River.lake:Provider.Number)
str(sites_clean_3)

##6) Run Diagnostics to look for missing files, sites, duplicates etc. ---------
diag <- run_diagnostics(sites_clean_3, over, taxa_lookup, strict = FALSE)
##see step 8 to investigate diagnostics

##7) Save processed site list  ---------
##save this to a processed data folder, so don't need to re-run this code 
write.csv(sites_clean_3, "data/processed/step2_site_ids/Step2_MZB_sites_lotic.csv", row.names = FALSE)


##8) Investigate Diagnostics --------------------------
missing_files <- diag[["missing_files"]]
##FRA_098_MZB_LO.csv, FIN_0101_MZB_LO.csv, PRT_106_MZB_LO.csv -- csvs not ready year 

duplicate_sites <- diag[["dups_sites"]]
##there are duplicates here, where for 4 datasets there are the same Site IDs  GER_018_MZB_LO1.csv , GER_018_MZB_LO2.csv, (same site id in master table and raw csv) ESP_036_MZB_LO1.csv, ESP_036_MZB_LO2.csv (same site ids in raw data, but not in master table)
ger_018_lo1 <- taxa.files[["GER_018_MZB_LO1.csv"]] %>%
  filter(Site.ID %in% duplicate_sites$Site_ID_original)
ger_018_lo2 <- taxa.files[["GER_018_MZB_LO2.csv"]]%>%
  filter(Site.ID %in% duplicate_sites$Site_ID_original)

ger_018_overlap <- anti_join(ger_018_lo1, ger_018_lo2, by = c("Site.ID", "Sampling.date", "Taxon.name", "Taxon.ID", "Abundance"))
##okay, so my interpretation of this is that ger_018_lo2 contains the same data as ger_018_lo1, as there are no unique data. lo2 just does not contain a sample ID, so retain lo1 
##so for this one would be okay to just keep one, but i think maybe want to figure out a systematic way to deal with this .. 

esp_036_lo1 <- taxa.files[["ESP_036_MZB_LO1.csv"]] %>%
  filter(Site.ID %in% duplicate_sites$Site_ID_original)
esp_036_lo2 <- taxa.files[["ESP_036_MZB_LO2.csv"]] %>%
  filter(Site.ID %in% duplicate_sites$Site_ID_original)

esp_036_overlap <- anti_join(esp_036_lo1, esp_036_lo2, by = c("Site.ID", "Sampling.date", "Taxon.name", "Taxon.ID", "Abundance"))
##so for this spain dataset, these have the same site ID but have different data - wouldn't we want to keep them both? at least at this stage? 
##i think the issue for this one, is that in the raw csv for LO2 the site names have been changed and are not listed as the site_id_original as in the master table samples 
##this would also fix why those sites that are listed in the master table do not have associated raw data 

##my solution - i think need to talk to james/nathalie .. 

missing_sites <- diag[["filename_NA"]]
##879 sites missing file names - so  there are 878 sites where the original sample ID in the sites df does not match exist in the raw csv files 
##so 4 datasets where site ids do not match .. 
#IRL_051_MZB_LO, AUT_001_MZB_LO,  ESP_034_MZB_LO, ESP_036_MZB_LO 

##ESP_034_MZB_LO --> these sites exist in the master site list, because this data set was coming from 3 different csv, only keep most recent csv that does not have all sites 
##AUT_001_MZB_LO --> that missing site does not exist 



