#checks whether sites listed for removal also present in other datasets using a 100m threshold

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

###---Load the sites that were listed for removal in Step 3 ("Step 3 - duplicated within removed.csv")
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
#                 SITES LISTED FOR REMOVAL THAT ALSO OCCUR IN OTHER DATASETS
#
#######################################################################################################

rem.100m<-data.frame(country=NA, prov=NA, origin=NA, dataset.id=NA, group=NA, org.site=NA, unique.id=NA, river=NA, latitude=NA, longitude=NA, st.yr=NA, ed.yr=NA, yr.ln=NA, yr.num=NA, distance=NA)

loop.num<-0.1
count<-2
prov<-unique(rem$dataset.id)

for (i in 1:length(prov)) {
  sub<-rem[rem$dataset.id==prov[i],]
  sub1<-subset(sites2, sites2$Country==sub$country[1] & sites2$Dataset.ID %nin% sub$dataset.id[1])
  
  if (length(sub1[,1])>0) {
    wgs84<-st_as_sf(sub, coords = c(12,11), crs = 4326)
    wgs84.1<-st_as_sf(sub1, coords = c(12,11), crs = 4326)
    etrs89<-st_transform(wgs84, 3034)
    etrs89.1<-st_transform(wgs84.1, 3034)
    coords<-as.data.frame(st_coordinates(etrs89))
    coords.1<-as.data.frame(st_coordinates(etrs89.1))
    
    group<-1
    for (j in 1:length(sub$unique.id)) {
      dists<-as.vector(pointDistance(p1=coords[j,], p2=coords.1, lonlat=FALSE))
      nums<-which(dists<=100)
      
      if (length(nums)>0) {
        nums2<-c(count:(count+(length(nums))))
        
        rem.100m<-rbind(rem.100m, rem.100m[rep(1, times=(length(nums2))),])
        
        rem.100m$country[nums2[1]]<-sub$country[j]
        rem.100m$country[nums2[2:length(nums2)]]<-sub1$Country[nums]
        rem.100m$prov[nums2[1]]<-sub$prov[j]
        rem.100m$prov[nums2[2:length(nums2)]]<-sub1$Data_owner[nums]
        rem.100m$origin[nums2[1]]<-sub$origin[j]
        rem.100m$origin[nums2[2:length(nums2)]]<-sub1$origin[nums]
        rem.100m$dataset.id[nums2[1]]<-sub$dataset.id[j]
        rem.100m$dataset.id[nums2[2:length(nums2)]]<-sub1$Dataset.ID[nums]
        rem.100m$group[nums2]<-group
        rem.100m$org.site[nums2[1]]<-sub$org.site[j]
        rem.100m$org.site[nums2[2:length(nums2)]]<-sub1$Site_ID_original[nums]
        rem.100m$unique.id[nums2[1]]<-sub$unique.id[j]
        rem.100m$unique.id[nums2[2:length(nums2)]]<-sub1$Unique.ID[nums]
        rem.100m$river[nums2[1]]<-sub$river[j]
        rem.100m$river[nums2[2:length(nums2)]]<-sub1$River.lake[nums]
        rem.100m$latitude[nums2[1]]<-sub$latitude[j]
        rem.100m$latitude[nums2[2:length(nums2)]]<-sub1$Latitude_Y[nums]
        rem.100m$longitude[nums2[1]]<-sub$longitude[j]
        rem.100m$longitude[nums2[2:length(nums2)]]<-sub1$Longitude_X[nums]
        rem.100m$st.yr[nums2[1]]<-sub$st.yr[j]
        rem.100m$st.yr[nums2[2:length(nums2)]]<-sub1$Starting_year[nums]
        rem.100m$ed.yr[nums2[1]]<-sub$ed.yr[j]
        rem.100m$ed.yr[nums2[2:length(nums2)]]<-sub1$Ending_year[nums]
        rem.100m$yr.ln[nums2[1]]<-sub$yr.ln[j]
        rem.100m$yr.ln[nums2[2:length(nums2)]]<-sub1$Year_count[nums]
        rem.100m$yr.num[nums2[1]]<-sub$yr.num[j]
        rem.100m$yr.num[nums2[2:length(nums2)]]<-sub1$Sampling_years[nums]
        rem.100m$distance[nums2[1]]<-0
        rem.100m$distance[nums2[2:length(nums2)]]<-dists[nums]
        
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
rem.100m<-rem.100m[-1,]

###---Remove mirrored groups
rem.100m.2<-rem.100m[1,]
ctys<-unique(rem.100m$country)

for (i in 1:length(ctys)) {
  sub<-subset(rem.100m, rem.100m$country %in% ctys[i])
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
      rem.100m.2<-rbind(rem.100m.2, sub1)
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
rem.100m.2<-rem.100m.2[-1,]

#######################################################################################################
#
#                                      SAVE THE DATASET
#
#######################################################################################################

write.csv(rem.100m.2, "Lotic macroinvert - removed less than 100m.csv", row.names=FALSE, fileEncoding = "latin1")
#next, load the site list into qGIS and manually examine by group