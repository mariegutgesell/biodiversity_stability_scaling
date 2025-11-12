#this code creates unique site and dataset IDs for the Master table site list

###---Load libraries and set directory for saving files
library(Hmisc)
site.dir<-getwd()

###---Load the overview, site list, and country codes from the master table, must be saved as separate CSVs
#Site list
sites<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #load the list for your taxonomic group

#Overview
over<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
over<-subset(over, over$Taxa.group=="macroinvertebrates") #filter to desired taxonomic group

#Country codes
code<-read.csv(file.choose(), header=TRUE)

###---Load all taxa files, set working directory to the directory with the unprocessed data for your taxonomic group
(taxa.names<-list.files(pattern="*.csv"))
taxa.names<-taxa.names[grep(eco.type, taxa.names)] #refine to desired ecosystem type

taxa.files<-list() #empty list
for (i in 1:length(taxa.names)) { #for however many filenames you need
  taxa.files[[i]]<-read.csv(taxa.names[i], header=TRUE, fileEncoding="latin1", sep=";") #load the data into the ith list position 
}

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
    num<-which(taxa.names==sub2$file_name[j])
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
