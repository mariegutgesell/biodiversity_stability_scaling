#this code creates unique site and dataset IDs for the Master table site list

###---Load libraries a
library(Hmisc)
#remotes::install_github("velofrog/readxlsb")
library(readxlsb)
library(tidyverse)


###---Load the overview, site list, and country codes from the master table
#Site list
sites<-read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_MZB") %>%#load the list for your taxonomic group
  filter(ecosystem == "lotic") #%>%
  #filter(fulfills.requirement == "yes") 
#Overview
over<-read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "overview") %>%
  filter(Taxa.group == "macroinvertebrates", ecosystem == "lotic") ##filter to desired taxonomic group and ecosystem

#Country codes
code<-read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "Countrycodes") %>%
  select(countrycode:X3.letter.Code) %>%
  rename(Country = "country")

##trying my own approach to data prep 
###---Load all taxa files, set working directory to the directory with the unprocessed data for your taxonomic group
file.list <- list.files(path = paste0(getwd(), "/data/Unprocessed_datasets_MZB"), pattern='\\.csv$', full.names = TRUE)
file.list <- file.list[!grepl("LE", basename(file.list))] ##filter out lentic files
taxa.files <- sapply(file.list, read.csv, simplify = FALSE, sep = ";", fileEncoding="latin1")
taxa.files <- setNames(taxa.files, basename(file.list))
#df <- rbindlist(taxa.files) ##if want to bind all dfs together

##So, effectively I want to add the unique provider number, country code, file name, dataset id and unique id to the site list

##So, add provision number and country to the site list
##then, would start from the raw taxa data and join to the site list (without dataset ID and unique ID) - 
##generate datasetID and uniqueID 
##add datafiles from overview to the list, but these are missing in the raw csv files 


###---Add owner numbers to the site list
##create df of unique codes for each provider, and add to site 
prov_map <- over %>%
  mutate(  Provider.Number = str_pad(Provider.Number, width = 3, pad = "0")) %>%
  select(Data_owner, Provider.Number) %>%
  distinct()
str(prov_map)

sites_clean <- left_join(sites, prov_map, by = "Data_owner") %>%##add unique provider.number 
  left_join(code, by = "Country") %>% ##add country code
  mutate(Site_ID_original = trimws(Site_ID_original)) 
##correct the format of the provider in the overview sheet and select columns that you want to retain 
over_clean <- over %>%
  mutate(  Provider.Number = str_pad(Provider.Number, width = 3, pad = "0")) %>%##add provider number 
  select(Provider.Number, ecosystem, file_name, Country, TG_short, ecosys_short) %>%
  distinct()

##match site ID original in sites df to site id in taxa.files, then attach the file name from the file that contains that site id to the sites clean df. 
##make lookup table that has all file names and site ids (unique)
taxa_lookup <- imap_dfr(taxa.files, ~tibble(file_name = .y, Site_ID_original = as.character(.x$Site.ID))) %>%
  distinct() %>%
  mutate(Site_ID_original = trimws(Site_ID_original)) %>% ##trim any white space from the site names 
  left_join(over_clean, by = "file_name") %>% ##add file name, provider number, country and ecosystem from overview df
  mutate(Site.ID_unprocessed = Site_ID_original) 


##Join file name to site df by original site id from the raw unprocessed data (should theoretically be the same)
sites_clean_2 <- left_join(sites_clean, taxa_lookup, by = c("Site_ID_original", "Provider.Number", "ecosystem", "Country"))


##Create dataset and unique IDs
make_ids <- function(df) {
  df %>%
    mutate(
      Dataset.ID_marie = tools::file_path_sans_ext(file_name),
      Unique.ID_marie  = paste(Dataset.ID_marie, SGN_Site_ID, sep = "_")
    )
}

sites_clean_3 <- make_ids(sites_clean_2) %>%
  select(file_name, Dataset.ID, Unique.ID, Dataset.ID_marie, Unique.ID_marie, Site.ID_unprocessed, origin:Country, countrycode, X3.letter.Code, Latitude_Y:ecosystem, TG_short, ecosys_short, River.lake:Provider.Number)
str(sites_clean_3)

##save this to a processed data folder, so don't need to re-run this code 
write.csv(sites_clean_3, "data/data_processing/Step2_MZB_sites_lotic.csv", row.names = FALSE)

readLines("data/data_processing/Step2_MZB_sites_lotic.csv", n = 10)




###CODE BELOW -- looking into missing filenames and trying to figure out why file names are missing - perhaps these are removed in later steps? 
##why different from james? keep going and see where this comes up again 

##1) Missing files in raw data that exist in the master overview spreadsheet
##overview says there are 83 datafiles (88 total rows, where 4 files have 2-3 rows as the datafile contains multiple year periods) for lotic data, but in the folder there are only 80, 
missing_files <- anti_join(over, taxa_lookup, by = "file_name")
##so there are 3 datafiles in overview that are not in the raw data - why?
##FRA_098_MZB_LO.csv, FIN_0101_MZB_LO.csv, PRT_106_MZB_LO.csv

##2) Why am I getting more sites when joining sites_clean to taxa lookup? 
##check if any duplicate site names or file names (shouldn't have any duplicates)
dups_taxa_lookup <- taxa_lookup %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

##check for duplicates in the cleaned site list 
dups_sites_clean_2 <- sites_clean_2 %>%
  group_by(Site_ID_original, Provider.Number, ecosystem, Country) %>%
  filter(n() > 1) %>%
  ungroup()
##there are duplicates here, where for 4 datasets there are the same Site IDs  GER_018_MZB_LO1.csv , GER_018_MZB_LO2.csv, (same site id in master table and raw csv) ESP_036_MZB_LO1.csv, ESP_036_MZB_LO2.csv (same site ids in raw data, but not in master table)
ger_018_lo1 <- taxa.files[["GER_018_MZB_LO1.csv"]] %>%
  filter(Site.ID %in% dups_sites_clean_2$Site_ID_original)
ger_018_lo2 <- taxa.files[["GER_018_MZB_LO2.csv"]]%>%
  filter(Site.ID %in% dups_sites_clean_2$Site_ID_original)

ger_018_overlap <- anti_join(ger_018_lo1, ger_018_lo2, by = c("Site.ID", "Sampling.date", "Taxon.name", "Taxon.ID", "Abundance"))
##okay, so my interpretation of this is that ger_018_lo2 contains the same data as ger_018_lo1, as there are no unique data. lo2 just does not contain a sample ID, so retain lo1 
##so for this one would be okay to just keep one, but i think maybe want to figure out a systematic way to deal with this .. 

esp_036_lo1 <- taxa.files[["ESP_036_MZB_LO1.csv"]] %>%
  filter(Site.ID %in% dups_sites_clean_2$Site_ID_original)
esp_036_lo2 <- taxa.files[["ESP_036_MZB_LO2.csv"]] %>%
  filter(Site.ID %in% dups_sites_clean_2$Site_ID_original)

esp_036_overlap <- anti_join(esp_036_lo1, esp_036_lo2, by = c("Site.ID", "Sampling.date", "Taxon.name", "Taxon.ID", "Abundance"))
##so for this spain dataset, these have the same site ID but have different data - wouldn't we want to keep them both? at least at this stage? 
##i think the issue for this one, is that in the raw csv for LO2 the site names have been changed and are not listed as the site_id_original as in the master table samples 
##this would also fix why those sites that are listed in the master table do not have associated raw data 

##my solution - i think need to talk to james/nathalie .. 


##3) Filename NAs - sites that are listed in the Mastertable site list, but do not exist in the raw data 
filename_NA <- sites_clean_3 %>%
  filter(is.na(file_name)) 
##879 sites missing file names - so I think this means that there are 887 sites where the original sample ID in the sites df does not match the site ID in the unprocessed data, why? 

missing_filenames <- filename_NA %>%
  select(Dataset.ID, Dataset.ID_marie,  Country) %>%
  distinct()
##so 4 datasets where site ids do not match .. 
#IRL_051_MZB_LO, AUT_001_MZB_LO,  ESP_034_MZB_LO, ESP_036_MZB_LO 


##Look to see which sites are missing where -- why? 
raw_ids <- taxa_lookup %>% distinct()
site_ids <- sites %>% select(Dataset.ID, Unique.ID, Site_ID_original) %>% distinct()

ids_in_raw_not_in_sites <- raw_ids %>%
  anti_join(site_ids, by = "Site_ID_original") ##2sites that are in raw data that are not in site df 
ids_in_sites_not_in_raw <- site_ids %>%
  anti_join(raw_ids, by = "Site_ID_original") ##879 sites in site df that are not in the raw unproccessed data 

##are these removed at some point later? or is there something else about the original site ids that i am missing? 


##Checking that the same NAs come up in my dataset and james
james_mzb_df <- read.csv("EU macroinvert database processing/Step 2 - Master table site and dataset IDs/MZB sites_18.11.2025.csv") %>%
  filter(ecosystem == "lotic") #%>%
  filter(fulfills.requirement == "yes") ##this is the most recent one that comes from dropbox 

test_j <- james_mzb_df %>%
  filter(is.na(Filename))
##has 879 w/ missing file names, and dataset ids, and unique ids  - the discrepancy in numbers is because in james don't have issues from ESP_036_MZB_LO,


my_na <- filename_NA %>%
  select(Site_ID_original, my_file_name = file_name) %>%
  distinct()

james_na <- test_j %>%
  select(Site_ID_original, james_file_name = Filename) %>%
  distinct()

compare <- my_na %>%
  full_join(james_na, by = c("Site_ID_original"))

##I think we have the same NAs 




###Integrating checks to make sure joining/filtering etc. is all working correctly

##Check filter on LE files, only filtered out LE (no LO)
file.list_le <- list.files(path = paste0(getwd(), "/data/Unprocessed_datasets_MZB"), pattern='\\.csv$', full.names = TRUE)
file.list_le <- file.list_le[grepl("LE", basename(file.list_le))] 
taxa.files_le <- sapply(file.list_le, read.csv, simplify = FALSE, sep = ";", fileEncoding="latin1")
taxa.files_le <- setNames(taxa.files_le, basename(file.list_le)) ##set the name of each file to be the base name rather than the whole directory string 



