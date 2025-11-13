#this code identifies sites that lie within a certain threshold distance from each other within each dataset
#the purpose being to find sites that are close together in the same river section and identify which to keep and which to remove

###---Load libraries
library(sf)
library(geosphere)
library(Hmisc)
library(readxlsb)

#######################################################################################################
#
#                                       ASSIGN DATASET NAMES
#
#######################################################################################################

#the first step is to assign the dataset and site names
#we need to identify which sites belong to which datasets to check site overlap within datasets and to label the overlapping sites

###---Load the overview, desired site list, and country codes from the master table, must be saved as separate CSVs
#Site list
sites<-read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "samples1_MZB") %>%#load the list for your taxonomic group
  filter(ecosystem == "lotic")  %>%
  filter(fulfills.requirement == "yes") #filter to sites that meet the long-term data call requirements

#Overview
over<-read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "overview") %>%
  filter(Taxa.group == "macroinvertebrates", ecosystem == "lotic") ##filter to desired taxonomic group and ecosystem

#Country codes
code<-read_xlsb("data/1_MASTERTABLE.xlsb", sheet = "Countrycodes") %>%
  select(countrycode:X3.letter.Code) #%>%
  #rename(Country = "country")

###---Add owner numbers to the site list (used to produce dataset and site IDs)
options(warn=2)

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

###---Add unique dataset and site IDs
sites$Dataset.ID<-NA
sites$Unique.ID<-NA
for (i in 1:length(sites$SGN_Site_ID)) {
  cty.num<-which(code$country==sites$Country[i])
  sites$Dataset.ID[i]<-paste(code$X3.letter.Code[cty.num], sites$Provider.Number[i], "MZB_LO", sep="_")
  sites$Unique.ID[i]<-paste(code$X3.letter.Code[cty.num], sites$Provider.Number[i], "MZB_LO", sites$SGN_Site_ID[i], sep="_")
}

###---Order sites
sites2<-sites[order(sites$Country, sites$Dataset.ID, sites$origin, sites$SGN_Site_ID),]

#######################################################################################################
#
#                                   REMOVE ALREADY CHECKED DATASETS
#
#######################################################################################################

#we already have a list of datasets that have been checked, so these can be removed from the site list to
#avoid repeating effort

#file is called "Datasets already checked.csv"
rem<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

sites3<-subset(sites2, sites2$Dataset.ID %nin% rem$checked)

#######################################################################################################
#
#                                   CHECK OVERLAPPING SITES WITHIN DATASETS
#
#######################################################################################################

#overlap is checked within 100m and 1km, but lists must be examined manually
#within 100m is manually checked first because these are almost always overlapping so the checking is faster
#within 1km = >100m but <=1km, these are typically slower to get through

###---Overlapping sites
within.100m<-data.frame(country=NA, prov=NA, origin=NA, dataset.id=NA, group=NA, org.site=NA, unique.id=NA, river=NA, latitude=NA, longitude=NA, st.yr=NA, ed.yr=NA, yr.ln=NA, yr.num=NA, distance=NA)
within.1km<-data.frame(country=NA, prov=NA, origin=NA, dataset.id=NA, group=NA, org.site=NA, unique.id=NA, river=NA, latitude=NA, longitude=NA, st.yr=NA, ed.yr=NA, yr.ln=NA, yr.num=NA, distance=NA)

##--within 100m (<100m)
loop.num<-0.1
count<-2
prov<-unique(sites3$Dataset.ID)
for (i in 1:length(prov)) {
  sub<-subset(sites3, sites3$Dataset.ID==prov[i])
  
  wgs84<-st_as_sf(sub, coords = c(12,11), crs = 4326)
  etrs89<-st_transform(wgs84, 3034)
  mat<-as.data.frame(st_distance(etrs89))
  
  group<-1
  for (k in 1:length(sub$Unique.ID)) {
    dists<-as.vector(mat[,k])
    nums<-which(dists>0 & dists<=100)
    if (length(nums)>0) {
      nums2<-c(count:(count+(length(nums))))
      
      within.100m<-rbind(within.100m, within.100m[rep(1, times=(length(nums2))),])
      
      within.100m$country[nums2]<-sub$Country[1]
      within.100m$prov[nums2]<-sub$Data_owner[1]
      within.100m$origin[nums2[1]]<-sub$origin[k]
      within.100m$origin[nums2[2:length(nums2)]]<-sub$origin[nums]
      within.100m$dataset.id[nums2]<-sub$Dataset.ID[1]
      within.100m$group[nums2]<-group
      within.100m$org.site[nums2[1]]<-sub$Site_ID_original[k]
      within.100m$org.site[nums2[2:length(nums2)]]<-sub$Site_ID_original[nums]
      within.100m$unique.id[nums2[1]]<-sub$Unique.ID[k]
      within.100m$unique.id[nums2[2:length(nums2)]]<-sub$Unique.ID[nums]
      within.100m$river[nums2[1]]<-sub$River.lake[k]
      within.100m$river[nums2[2:length(nums2)]]<-sub$River.lake[nums]
      within.100m$latitude[nums2[1]]<-sub$Latitude_Y[k]
      within.100m$latitude[nums2[2:length(nums2)]]<-sub$Latitude_Y[nums]
      within.100m$longitude[nums2[1]]<-sub$Longitude_X[k]
      within.100m$longitude[nums2[2:length(nums2)]]<-sub$Longitude_X[nums]
      within.100m$st.yr[nums2[1]]<-sub$Starting_year[k]
      within.100m$st.yr[nums2[2:length(nums2)]]<-sub$Starting_year[nums]
      within.100m$ed.yr[nums2[1]]<-sub$Ending_year[k]
      within.100m$ed.yr[nums2[2:length(nums2)]]<-sub$Ending_year[nums]
      within.100m$yr.ln[nums2[1]]<-sub$Year_count[k]
      within.100m$yr.ln[nums2[2:length(nums2)]]<-sub$Year_count[nums]
      within.100m$yr.num[nums2[1]]<-sub$Sampling_years[k]
      within.100m$yr.num[nums2[2:length(nums2)]]<-sub$Sampling_years[nums]
      within.100m$distance[nums2[1]]<-0
      within.100m$distance[nums2[2:length(nums2)]]<-mat[nums,k]
      
      group<-group+1
      count<-count+length(nums2)
    }
  }
  
  if (i/length(prov)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }
}

#remove the dummy first row
within.100m<-within.100m[-1,]

###---Remove mirrored groups
#groups will be duplicated, for example Site A is within 100m of Site B so these will be grouped, but Site B is also within 100m of Site A, so those will be grouped also
#this code removes those duplicated groups so the sites only have to be checked once
within.100m.2<-within.100m[1,]
datasets<-unique(within.100m$dataset.id)

for (i in 1:length(datasets)) {
  sub<-subset(within.100m, within.100m$dataset.id %in% datasets[i])
  groups<-0
  for (j in 1:length(unique(sub$group))) {
    sub1<-subset(sub, sub$group==unique(sub$group)[j])
    if (all(groups %nin% unique(sub$group)[j])) {
      within.100m.2<-rbind(within.100m.2, sub1)
      groups<-append(groups, unique(sub$group)[j])
    }
    sub2<-subset(sub, sub$group!=unique(sub$group)[j])
    for (k in 1:length(unique(sub2$group))) {
      sub3<-subset(sub2, sub2$group==unique(sub2$group)[k])
      if (all(sub3$unique.id %in% sub1$unique.id)) {
        groups<-append(groups, unique(sub2$group)[k])
      }
    }
  }
}

#remove the dummy first row
within.100m.2<-within.100m.2[-1,]


##--within 1km (note this is >100m & <=1000m)
loop.num<-0.1
count<-2
prov<-unique(sites3$Dataset.ID)
for (i in 1:length(prov)) {
  sub<-subset(sites3, sites3$Dataset.ID==prov[i])
  
  wgs84<-st_as_sf(sub, coords = c(12,11), crs = 4326)
  etrs89<-st_transform(wgs84, 3034)
  mat<-as.data.frame(st_distance(etrs89))
  
  group<-1
  for (k in 1:length(sub$Unique.ID)) {
    dists<-as.vector(mat[,k])
    nums<-which(dists>100 & dists<=1000)
    if (length(nums)>0) {
      nums2<-c(count:(count+(length(nums))))
      
      within.1km<-rbind(within.1km, within.1km[rep(1, times=(length(nums2))),])
      
      within.1km$country[nums2]<-sub$Country[1]
      within.1km$prov[nums2]<-sub$Data_owner[1]
      within.1km$origin[nums2[1]]<-sub$origin[k]
      within.1km$origin[nums2[2:length(nums2)]]<-sub$origin[nums]
      within.1km$dataset.id[nums2]<-sub$Dataset.ID[1]
      within.1km$group[nums2]<-group
      within.1km$org.site[nums2[1]]<-sub$Site_ID_original[k]
      within.1km$org.site[nums2[2:length(nums2)]]<-sub$Site_ID_original[nums]
      within.1km$unique.id[nums2[1]]<-sub$Unique.ID[k]
      within.1km$unique.id[nums2[2:length(nums2)]]<-sub$Unique.ID[nums]
      within.1km$river[nums2[1]]<-sub$River.lake[k]
      within.1km$river[nums2[2:length(nums2)]]<-sub$River.lake[nums]
      within.1km$latitude[nums2[1]]<-sub$Latitude_Y[k]
      within.1km$latitude[nums2[2:length(nums2)]]<-sub$Latitude_Y[nums]
      within.1km$longitude[nums2[1]]<-sub$Longitude_X[k]
      within.1km$longitude[nums2[2:length(nums2)]]<-sub$Longitude_X[nums]
      within.1km$st.yr[nums2[1]]<-sub$Starting_year[k]
      within.1km$st.yr[nums2[2:length(nums2)]]<-sub$Starting_year[nums]
      within.1km$ed.yr[nums2[1]]<-sub$Ending_year[k]
      within.1km$ed.yr[nums2[2:length(nums2)]]<-sub$Ending_year[nums]
      within.1km$yr.ln[nums2[1]]<-sub$Year_count[k]
      within.1km$yr.ln[nums2[2:length(nums2)]]<-sub$Year_count[nums]
      within.1km$yr.num[nums2[1]]<-sub$Sampling_years[k]
      within.1km$yr.num[nums2[2:length(nums2)]]<-sub$Sampling_years[nums]
      within.1km$distance[nums2[1]]<-0
      within.1km$distance[nums2[2:length(nums2)]]<-mat[nums,k]
      
      group<-group+1
      count<-count+length(nums2)
    }
  }
  
  if (i/length(prov)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }
}

#remove the dummy first row
within.1km<-within.1km[-1,]

###---Remove mirrored groups
within.1km.2<-within.1km[1,]
datasets<-unique(within.1km$dataset.id)

loop.num<-0.1
for (i in 1:length(datasets)) {
  sub<-subset(within.1km, within.1km$dataset.id %in% datasets[i])
  groups<-0
  for (j in 1:length(unique(sub$group))) {
    sub1<-subset(sub, sub$group==unique(sub$group)[j])
    if (all(groups %nin% unique(sub$group)[j])) {
      within.1km.2<-rbind(within.1km.2, sub1)
      groups<-append(groups, unique(sub$group)[j])
    }
    sub2<-subset(sub, sub$group!=unique(sub$group)[j])
    for (k in 1:length(unique(sub2$group))) {
      sub3<-subset(sub2, sub2$group==unique(sub2$group)[k])
      if (all(sub3$unique.id %in% sub1$unique.id)) {
        groups<-append(groups, unique(sub2$group)[k])
      }
    }
  }
  
  if (i/length(datasets)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }
}

#remove the dummy first row
within.1km.2<-within.1km.2[-1,]

#######################################################################################################
#
#                                      SAVE THE DATASETS
#
#######################################################################################################

write.csv(within.100m.2, "Lotic macroinvert - within less than 100m.csv", row.names=FALSE, fileEncoding = "latin1")
write.csv(within.1km.2, "Lotic macroinvert - within 101-1000m.csv", row.names=FALSE, fileEncoding = "latin1")

#next, load the site list into qGIS and manually examine by group, specifically focusing on groups with the same river names
#note do not overwrite the Excel files with the CSVs, only add from the CSVs to what is already in the Excel sheets