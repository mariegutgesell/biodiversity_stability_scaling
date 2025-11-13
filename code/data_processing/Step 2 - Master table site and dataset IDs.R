#this code creates unique site and dataset IDs for the Master table site list

###---Load libraries a
library(Hmisc)
#remotes::install_github("velofrog/readxlsb")
library(readxlsb)
library(tidyverse)


###---Load the overview, site list, and country codes from the master table
#Site list
sites<-read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_MZB") %>%#load the list for your taxonomic group
  filter(ecosystem == "lotic") %>%
  filter(fulfills.requirement == "yes") 
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

##test
file.list_le <- list.files(path = paste0(getwd(), "/data/Unprocessed_datasets_MZB"), pattern='\\.csv$', full.names = TRUE)
file.list_le <- file.list_le[grepl("LE", basename(file.list_le))] 
taxa.files_le <- sapply(file.list_le, read.csv, simplify = FALSE, sep = ";", fileEncoding="latin1")
taxa.files_le <- setNames(taxa.files_le, basename(file.list_le)) ##set the name of each file to be the base name rather than the whole directory string 


##So, effectively I want to add the unique provider number, country code, file name, dataset id and unique id to the site list
##now, the site list does already have a dataset id and unique id... 
test <- sites %>%
  filter(is.na(Unique.ID))

###---Add owner numbers to the site list
##create df of unique codes for each provider, and add to site 
prov_map <- over %>%
  mutate(Provider.Number= sprintf("%03d", as.integer(Provider.Number))) %>%
  select(Data_owner, Provider.Number) %>%
  distinct()

sites_clean <- left_join(sites, prov_map, by = "Data_owner") %>%##add unique provider.number 
  left_join(code, by = "Country") ##add country code

##correct the format of the provider in the overview sheet and select columns that you want to retain 
over_clean <- over %>%
  mutate(Provider.Number= sprintf("%03d", as.integer(Provider.Number))) %>%
  select(Provider.Number, ecosystem, file_name, Country, TG_short, ecosys_short) %>%
  distinct()

##so there are 5 duplicate provider number and file name .. but are from different sampling years, but these are in the same datafile, so don't want to retain the sampling year (gives duplicates when joining further down)
##check duplicates:
dups_all <- over_clean %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

testdf <- taxa.files[["ESP_034_MZB_LO.csv"]]

##match site ID original in sites df to site id in taxa.files, then attach the file name from the file that contains that site id to the sites clean df. 
##make lookup table that has all file names and site ids (unique)
taxa_lookup <- imap_dfr(taxa.files, ~tibble(file_name = .y, Site_ID_original = as.character(.x$Site.ID))) %>%
  distinct() %>%
  left_join(over_clean, by = "file_name") %>% ##add file name, provider number, country and ecosystem from overview df
  mutate(Site.ID_unprocessed = Site_ID_original)

##check if any duplicate site names or file names (shouldn't have any duplicates)
dups_all <- taxa_lookup %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

##check if any NAs in original site ids or file names
test_1 <- taxa_lookup %>%
  filter(is.na(Site_ID_original))
##one file contains an NA site ID: ESP_036_MZB_LO2.csv 
testdf <- taxa.files[["ESP_036_MZB_LO2.csv"]] %>%
  filter(is.na(Site.ID))
##Looks like it is just a whole bunch (4305 rows) of all NAs - will likely want to clean this up at somepoint 

##check if any NA files names 
test_2 <- taxa_lookup %>%
  filter(is.na(file_name))
##no 


##Join file name to site df by original site id from the raw unprocessed data (should theoretically be the same)
sites_clean_2 <- left_join(sites_clean, taxa_lookup, by = c("Site_ID_original", "Provider.Number", "ecosystem", "Country"))
##so there are 14 rows that are causing a multijoin

dups_sites <- sites_clean %>%
  group_by(Site_ID_original, Provider.Number, ecosystem, Country) %>%
  filter(n() > 1) %>%
  ungroup()

dups_taxa <- taxa_lookup %>%
  group_by(Site_ID_original, Provider.Number, ecosystem, Country) %>%
  filter(n() > 1) %>%
  ungroup() ##there are duplicates here because data comes from two different csvs (LO1 or LO2) but that is not preserved or differentiated in Site_ID_original or in the Dataset ID, so not sure which goes with what 

dup_rows <- sites_clean_2 %>%
  count(Site_ID_original, Provider.Number, ecosystem, Country) %>%
  filter(n > 1)

problem_rows <- sites_clean_2 %>%
  semi_join(dup_rows,
            by = c("Site_ID_original", "Provider.Number", "ecosystem", "Country")) ##based on james df, the duplicate ones from germany come from LO2 - this is coded implicitly in his for loop by overwriting and choosing the order it appears in overview, is this right?

taxa_lookup %>%
  semi_join(dup_rows,
            by = c("Site_ID_original", "Provider.Number", "ecosystem", "Country"))

##check for any NAs in site IDs
test_3 <- sites_clean_2 %>%
  filter(is.na(Site.ID_unprocessed))

test_4 <- sites_clean_2 %>%
  filter(is.na(file_name))
##887 missing file names - so I think this means that there are 887 sites where the original sample ID in the sites df does not match the site ID in the unprocessed data, why? 

missing_filenames <- sites_clean_2 %>%
  filter(is.na(file_name)) %>%
  select(Dataset.ID, Country) %>%
  distinct()
##so 6 datasets where site ids do not match .. 
#IRL_051_MZB_LO, AUT_001_MZB_LO, NLD_028_MZB_LO, ESP_034_MZB_LO, ESP_036_MZB_LO, CHE_075_MZB_LO

##Look to see which sites are missing where -- why? 
raw_ids <- taxa_lookup %>% distinct()
site_ids <- sites %>% select(Dataset.ID, Unique.ID, Site_ID_original) %>% distinct()

ids_in_raw_not_in_sites <- raw_ids %>%
  anti_join(site_ids, by = "Site_ID_original") ##2268 sites that are in raw data that are not in site df - i think these are ones that were determined as not suitable (i.e., do not fulfill requirements)
ids_in_sites_not_in_raw <- site_ids %>%
  anti_join(raw_ids, by = "Site_ID_original") ##886 sites in site df that are not in the raw unproccessed data 

##are these removed at some point later? or is there something else about the original site ids that i am missing? 



james_mzb_df <- read.csv("EU macroinvert database processing/Step 2 - Master table site and dataset IDs/MZB sites.csv") %>%
  filter(ecosystem == "lotic")

test_j <- james_mzb_df %>%
  filter(is.na(Filename))
##has 879 w/ missing file names, and dataset ids, and unique ids  -



###---Add owner numbers to the site list
sites$Provider.Number<-NA
prov<-unique(sites$Data_owner)
for (i in 1:length(prov)) {
  nums<-which(sites$Data_owner==prov[i])
  num<-over$Provider.Number[which(over$Data_owner==prov[i])[1]]
  
  if (nchar(num)==1) {
    num<-paste("00",num, sep="")
  }
  if (nchar(num)==2) {
    num<-paste("0",num, sep="")
  }
  
  sites$Provider.Number[nums]<-num
}

###---Add Filenames and dataset IDs
sites$Filename<-NA
sites$Dataset.ID<-NA
prov<-unique(sites$Provider.Number)
for (i in 1:length(prov)) {
  sites.nums<-which(sites$Provider.Number==prov[i])
  sub<-sites[sites.nums,]
  sub2<-subset(over, over$Provider.Number==as.numeric(prov[i]))
  for (j in 1:length(sub2$file_name)) {
    num<-which(taxa.files==sub2$file_name[j])
    dat<-taxa.files[[num]]
    sub3<-sub[which(sub$ecosystem==sub2$ecosystem[j]),]
    nums<-which(sites$Site_ID_original %in% dat$Site.ID & sites$Provider.Number %in% sub3$Provider.Number & sites$ecosystem %in% sub3$ecosystem)
    if (length(nums)>0) {
      cty.num<-which(code$country==sub2$Country[j])
      sites$Filename[nums]<-sub2$file_name[j]
      sites$Dataset.ID[nums]<-paste(code$X3.letter.Code[cty.num], prov[i], sub2$TG_short[j], sub2$ecosys_short[j], sep="_")
    }
  }
}

###---Add unique site IDs
sites$Unique.ID<-paste(sites$Dataset.ID, sites$SGN_Site_ID, sep="_")

write.csv(sites, "MZB sites.csv", row.names=FALSE, fileEncoding = "latin1")
