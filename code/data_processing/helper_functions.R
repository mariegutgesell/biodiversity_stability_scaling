##HELPER FUNCTIONS TO HELP WITH DATA PROCESSING

##FUNCTIONS FOR STEP 2 -------------------
##function to fix owner names in site df so that it matches the overview sheet, 
fix_data_owner_names <- function(df, aliases) {
  df %>%
    rowwise() %>%
    mutate(
      Data_owner = {
        hit <- which(startsWith(Data_owner, aliases$pattern))[1]
        if (is.na(hit)) Data_owner else aliases$clean_name[hit]
      }
    ) %>%
    ungroup()
}

##function to fix the lentic/lotic ecosystem as can be mislabeled in the site df
infer_ecosystem_from_dataset_id <- function(df) {
  df %>%
    mutate(
      ecosystem = case_when(
        str_detect(Dataset.ID, "LO[0-9]*$") ~ "lotic",
        str_detect(Dataset.ID, "LE[0-9]*$") ~ "lentic",
        TRUE ~ ecosystem  # keep existing if not matched
      )
    )
}

##function to add provider number and country codes to site df
add_provider_and_country <- function(df, over, code){
  ##create df of unique codes for each provider, and add to site 
  prov_map <- over %>%
    mutate(Provider.Number = str_pad(Provider.Number, width = 3, pad = "0")) %>%
    select(Data_owner, Provider.Number) %>%
    distinct()
  df <- df %>%
    left_join(prov_map, by = "Data_owner") %>%##add unique provider.number 
    left_join(code, by = "Country") ##add country code
}

build_taxa_lookup <- function(taxa.files, over) {
  ##correct the format of the provider in the overview sheet and select columns that you want to retain 
  over_clean <- over %>%
    mutate(Provider.Number = str_pad(Provider.Number, width = 3, pad = "0")) %>%##add provider number 
    select(Provider.Number, ecosystem, file_name, Country, TG_short, ecosys_short) %>%
    distinct()
  
  ##match site ID original in sites df to site id in taxa.files, then attach the file name from the file that contains that site id to the sites clean df. 
  ##make lookup table that has all file names and site ids (unique)
  taxa_lookup <- imap_dfr(taxa.files, ~tibble(file_name = .y, Site_ID_original = as.character(.x$Site.ID))) %>%
    distinct() %>%
    mutate(Site_ID_original = trimws(Site_ID_original)) %>% ##trim any white space from the site names 
    left_join(over_clean, by = "file_name") %>% ##add file name, provider number, country and ecosystem from overview df
    mutate(Site.ID_unprocessed = Site_ID_original) 
}

##function to create dataset and unique IDs
make_ids <- function(df) {
  df %>%
    mutate(
      Dataset.ID_marie = tools::file_path_sans_ext(file_name),
      Unique.ID_marie  = paste(Dataset.ID_marie, SGN_Site_ID, sep = "_")
    )
}

##function to run diagnostics to test for and detect missing files, duplicate site names 
run_diagnostics <- function(sites_clean_3, over, taxa_lookup, strict = FALSE) {
  missing_files <- anti_join(over, taxa_lookup, by = "file_name")
  dups_sites <- sites_clean_3 %>%
    group_by(Site_ID_original, Provider.Number, ecosystem, Country) %>%
    filter(n() > 1) %>% ungroup()
  filename_NA <- sites_clean_3 %>% filter(is.na(file_name))
  
  # print a short summary
  message("Diagnostics:")
  message("  missing_files: ", nrow(missing_files))
  message("  duplicate sites: ", nrow(dups_sites))
  message("  sites with NA file_name: ", nrow(filename_NA))
  
  if (strict) {
    if (nrow(missing_files) > 0) stop("Missing files in raw data.")
    if (nrow(dups_sites) > 0) stop("Duplicate site IDs after join.")
    if (nrow(filename_NA) > 0) stop("Sites with NA file_name.")
  }
  
  invisible(list(
    missing_files = missing_files,
    dups_sites    = dups_sites,
    filename_NA   = filename_NA
  ))
}
