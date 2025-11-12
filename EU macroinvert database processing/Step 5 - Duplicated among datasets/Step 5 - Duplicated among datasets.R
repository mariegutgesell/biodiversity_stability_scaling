#this code identifies sites that are within a certain distance threshold between different datasets
#the purpose being to identify duplicated sites among datasets and choose which to keep and which to remove

###---Load libraries
library(sf)
library(geosphere)
library(Hmisc)
library(raster)

#######################################################################################################
#
#                                       ASSIGN DATASET NAMES
#
#######################################################################################################

###---Load the overview, desired site list, and country codes from the master table, must be saved as separate CSVs
#Site list
sites<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
sites<-subset(sites, sites$ecosystem=="lotic") #filter to desired ecosystem type
sites<-subset(sites, sites$fulfills.requirement=="yes") #filter to sites that meet the long-term data call requirements

#Overview
over<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
over<-subset(over, over$Taxa.group=="macroinvertebrates") #filter to desired taxonomic group
over<-subset(over, over$ecosystem=="lotic") #filter to desired ecosystem type

#Country codes
code<-read.csv(file.choose(), header=TRUE)

####---Load the sites that were listed for removal in Steps 3 & 4 ("Step 4 - duplicated within removed.csv")
rem<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

###---Add owner numbers to the site list
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

###---Add site IDs
sites$Dataset.ID<-NA
sites$Unique.ID<-NA
for (i in 1:length(sites$SGN_Site_ID)) {
  cty.num<-which(code$country==sites$Country[i])
  sites$Dataset.ID[i]<-paste(code$X3.letter.Code[cty.num], sites$Provider.Number[i], "MZB_LO", sep="_")
  sites$Unique.ID[i]<-paste(code$X3.letter.Code[cty.num], sites$Provider.Number[i], "MZB_LO", sites$SGN_Site_ID[i], sep="_")
}

###---Order sites
sites2<-sites[order(sites$Country, sites$Dataset.ID, sites$origin, sites$SGN_Site_ID),]

###--Remove sites already listed for removal in Steps 3 & 4
sites2<-subset(sites2, sites2$Unique.ID %nin% rem$unique.id)

#######################################################################################################
#
#                                   CHECK OVERLAPPING SITES AT 100M
#
#######################################################################################################

among.100m<-data.frame(country=NA, prov=NA, origin=NA, dataset=NA, group=NA, org.site=NA, unique.id=NA, river=NA, latitude=NA, longitude=NA, yr.st=NA, yr.ed=NA, yr.ln=NA, yr.num=NA, distance=NA)

##--among 100m
loop.num<-0.1
count<-2
prov<-unique(sites2$Dataset.ID)

for (i in 1:length(prov)) {
  sub<-subset(sites2, sites2$Dataset.ID==prov[i])
  sub1<-subset(sites2, sites2$Country==sub$Country[1] & sites2$Dataset.ID %nin% sub$Dataset.ID[1])
  
  if (length(sub1[,1])>0) {
    wgs84<-st_as_sf(sub, coords = c(12,11), crs = 4326)
    wgs84.1<-st_as_sf(sub1, coords = c(12,11), crs = 4326)
    etrs89<-st_transform(wgs84, 3034)
    etrs89.1<-st_transform(wgs84.1, 3034)
    coords<-as.data.frame(st_coordinates(etrs89))
    coords.1<-as.data.frame(st_coordinates(etrs89.1))
    
    group<-1
    for (j in 1:length(sub$SGN_Site_ID)) {
      dists<-as.vector(pointDistance(p1=coords[j,], p2=coords.1, lonlat=FALSE))
      nums<-which(dists>0 & dists<=100)
      
      if (length(nums)>0) {
        nums2<-c(count:(count+(length(nums))))
        
        among.100m<-rbind(among.100m, among.100m[rep(1, times=(length(nums2))),])
        
        among.100m$country[nums2[1]]<-sub$Country[j]
        among.100m$country[nums2[2:length(nums2)]]<-sub1$Country[nums]
        among.100m$prov[nums2[1]]<-sub$Data_owner[j]
        among.100m$prov[nums2[2:length(nums2)]]<-sub1$Data_owner[nums]
        among.100m$origin[nums2[1]]<-sub$origin[j]
        among.100m$origin[nums2[2:length(nums2)]]<-sub1$origin[nums]
        among.100m$dataset[nums2[1]]<-sub$Dataset.ID[j]
        among.100m$dataset[nums2[2:length(nums2)]]<-sub1$Dataset.ID[nums]
        among.100m$group[nums2]<-group
        among.100m$org.site[nums2[1]]<-sub$Site_ID_original[j]
        among.100m$org.site[nums2[2:length(nums2)]]<-sub1$Site_ID_original[nums]
        among.100m$unique.id[nums2[1]]<-sub$Unique.ID[j]
        among.100m$unique.id[nums2[2:length(nums2)]]<-sub1$Unique.ID[nums]
        among.100m$river[nums2[1]]<-sub$River.lake[j]
        among.100m$river[nums2[2:length(nums2)]]<-sub1$River.lake[nums]
        among.100m$latitude[nums2[1]]<-sub$Latitude_Y[j]
        among.100m$latitude[nums2[2:length(nums2)]]<-sub1$Latitude_Y[nums]
        among.100m$longitude[nums2[1]]<-sub$Longitude_X[j]
        among.100m$longitude[nums2[2:length(nums2)]]<-sub1$Longitude_X[nums]
        among.100m$yr.st[nums2[1]]<-sub$Starting_year[j]
        among.100m$yr.st[nums2[2:length(nums2)]]<-sub1$Starting_year[nums]
        among.100m$yr.ed[nums2[1]]<-sub$Ending_year[j]
        among.100m$yr.ed[nums2[2:length(nums2)]]<-sub1$Ending_year[nums]
        among.100m$yr.ln[nums2[1]]<-sub$Year_count[j]
        among.100m$yr.ln[nums2[2:length(nums2)]]<-sub1$Year_count[nums]
        among.100m$yr.num[nums2[1]]<-sub$Sampling_years[j]
        among.100m$yr.num[nums2[2:length(nums2)]]<-sub1$Sampling_years[nums]
        among.100m$distance[nums2[1]]<-0
        among.100m$distance[nums2[2:length(nums2)]]<-dists[nums]
        
        group<-group+1
        count<-count+length(nums2)
      }
    }
  }
  
  if (i/length(prov)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }
}
among.100m<-among.100m[-1,]


###---Remove mirrored groups
among.100m.2<-among.100m[1,]
ctys<-unique(among.100m$country)

for (i in 1:length(ctys)) {
  sub<-subset(among.100m, among.100m$country %in% ctys[i])
  groups<-1
  nums<-1
  for (m in 2:length(sub$unique.id)) {
    if (sub$group[m]==sub$group[m-1] & m != length(sub$unique.id)) {
      nums<-append(nums, m)
    }
    if (sub$group[m]!=sub$group[m-1] & m != length(sub$unique.id)) {
      sub$group[nums]<-groups
      groups<-groups+1
      nums<-m
    }
    if (m == length(sub$unique.id)) {
      nums<-append(nums, m)
      sub$group[nums]<-groups
    }
  }
  
  groups<-0
  for (j in 1:length(unique(sub$group))) {
    sub1<-subset(sub, sub$group==unique(sub$group)[j])
    if (all(groups %nin% unique(sub$group)[j])) {
      among.100m.2<-rbind(among.100m.2, sub1)
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
among.100m.2<-among.100m.2[-1,]

write.csv(among.100m.2, "Lotic macroinvert - among less than 100m.csv", row.names=FALSE, fileEncoding = "latin1")
#check these sites now and save those to be removed in "Step 5 - duplicated within & among removed part 1.csv"

#######################################################################################################
#
#                                   CHECK OVERLAPPING SITES AT 1KM
#
#######################################################################################################

#Load those sites removed in the previous step ("Step 5 - duplicated within & among removed part 1.csv")
rem2<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
sites3<-subset(sites2, sites2$Unique.ID %nin% rem2$unique.id)

among.1km<-data.frame(country=NA, prov=NA, origin=NA, dataset=NA, group=NA, org.site=NA, unique.id=NA, river=NA, latitude=NA, longitude=NA, yr.st=NA, yr.ed=NA, yr.ln=NA, yr.num=NA, distance=NA)

loop.num<-0.1
count<-2
prov<-unique(sites3$Dataset.ID)

for (i in 1:length(prov)) {
  sub<-subset(sites3, sites3$Dataset.ID==prov[i])
  sub1<-subset(sites3, sites3$Country==sub$Country[1] & sites3$Dataset.ID %nin% sub$Dataset.ID[1])
  
  if (length(sub1[,1])>0) {
    wgs84<-st_as_sf(sub, coords = c(12,11), crs = 4326)
    wgs84.1<-st_as_sf(sub1, coords = c(12,11), crs = 4326)
    etrs89<-st_transform(wgs84, 3034)
    etrs89.1<-st_transform(wgs84.1, 3034)
    coords<-as.data.frame(st_coordinates(etrs89))
    coords.1<-as.data.frame(st_coordinates(etrs89.1))
    
    group<-1
    for (j in 1:length(sub$SGN_Site_ID)) {
      dists<-as.vector(pointDistance(p1=coords[j,], p2=coords.1, lonlat=FALSE))
      nums<-which(dists>100 & dists<=1000)
      
      if (length(nums)>0) {
        nums2<-c(count:(count+(length(nums))))
        
        among.1km<-rbind(among.1km, among.1km[rep(1, times=(length(nums2))),])
        
        among.1km$country[nums2[1]]<-sub$Country[j]
        among.1km$country[nums2[2:length(nums2)]]<-sub1$Country[nums]
        among.1km$prov[nums2[1]]<-sub$Data_owner[j]
        among.1km$prov[nums2[2:length(nums2)]]<-sub1$Data_owner[nums]
        among.1km$origin[nums2[1]]<-sub$origin[j]
        among.1km$origin[nums2[2:length(nums2)]]<-sub1$origin[nums]
        among.1km$dataset[nums2[1]]<-sub$Dataset.ID[j]
        among.1km$dataset[nums2[2:length(nums2)]]<-sub1$Dataset.ID[nums]
        among.1km$group[nums2]<-group
        among.1km$org.site[nums2[1]]<-sub$Site_ID_original[j]
        among.1km$org.site[nums2[2:length(nums2)]]<-sub1$Site_ID_original[nums]
        among.1km$unique.id[nums2[1]]<-sub$Unique.ID[j]
        among.1km$unique.id[nums2[2:length(nums2)]]<-sub1$Unique.ID[nums]
        among.1km$river[nums2[1]]<-sub$River.lake[j]
        among.1km$river[nums2[2:length(nums2)]]<-sub1$River.lake[nums]
        among.1km$latitude[nums2[1]]<-sub$Latitude_Y[j]
        among.1km$latitude[nums2[2:length(nums2)]]<-sub1$Latitude_Y[nums]
        among.1km$longitude[nums2[1]]<-sub$Longitude_X[j]
        among.1km$longitude[nums2[2:length(nums2)]]<-sub1$Longitude_X[nums]
        among.1km$yr.st[nums2[1]]<-sub$Starting_year[j]
        among.1km$yr.st[nums2[2:length(nums2)]]<-sub1$Starting_year[nums]
        among.1km$yr.ed[nums2[1]]<-sub$Ending_year[j]
        among.1km$yr.ed[nums2[2:length(nums2)]]<-sub1$Ending_year[nums]
        among.1km$yr.ln[nums2[1]]<-sub$Year_count[j]
        among.1km$yr.ln[nums2[2:length(nums2)]]<-sub1$Year_count[nums]
        among.1km$yr.num[nums2[1]]<-sub$Sampling_years[j]
        among.1km$yr.num[nums2[2:length(nums2)]]<-sub1$Sampling_years[nums]
        among.1km$distance[nums2[1]]<-0
        among.1km$distance[nums2[2:length(nums2)]]<-dists[nums]
        
        group<-group+1
        count<-count+length(nums2)
      }
    }
  }
  
  if (i/length(prov)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }
}
among.1km<-among.1km[-1,]


###---Remove mirrored groups
among.1km.2<-among.1km[1,]
ctys<-unique(among.1km$country)

for (i in 1:length(ctys)) {
  sub<-subset(among.1km, among.1km$country %in% ctys[i])
  groups<-1
  nums<-1
  for (m in 2:length(sub$unique.id)) {
    if (sub$group[m]==sub$group[m-1] & m != length(sub$unique.id)) {
      nums<-append(nums, m)
    }
    if (sub$group[m]!=sub$group[m-1] & m != length(sub$unique.id)) {
      sub$group[nums]<-groups
      groups<-groups+1
      nums<-m
    }
    if (m == length(sub$unique.id)) {
      nums<-append(nums, m)
      sub$group[nums]<-groups
    }
  }
  
  groups<-0
  for (j in 1:length(unique(sub$group))) {
    sub1<-subset(sub, sub$group==unique(sub$group)[j])
    if (all(groups %nin% unique(sub$group)[j])) {
      among.1km.2<-rbind(among.1km.2, sub1)
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
among.1km.2<-among.1km.2[-1,]

write.csv(among.1km.2, "Lotic macroinvert - among 101-1000m.csv", row.names=FALSE, fileEncoding = "latin1")
