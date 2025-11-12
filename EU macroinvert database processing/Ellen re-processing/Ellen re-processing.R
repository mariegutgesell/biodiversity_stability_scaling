library(Hmisc)

master.over<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #master table overview sheet
master.sites<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #master table samples sheet (it's the sites sheet really)
cty.code<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #master table country code sheet

tream.sites<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #TREAM site file
tream.yrs<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #TREAM siteyears file

dat<-subset(master.sites, master.sites$Ellens_ID %in% tream.sites$site_id) #only sites from the master table that match the TREAM site list

lux<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #fix for the Luxembourg dataset, I can send this if you want it
ita<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #fix for the Italian dataset, I can send this if you want it
be.wrong<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #Belgian sites with incorrect dates
be.right<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #Belgian dataset with the correct dates

###---Add owner numbers
options(warn=2)

dat$Provider.Number<-NA
prov<-unique(dat$Data_owner)
for (i in 1:length(prov)) {
  nums<-which(dat$Data_owner==prov[i])
  num<-master.over$Provider.Number[which(master.over$Data_owner==prov[i])[1]]
  
  if (nchar(num)==1) {
    num<-paste("00",num, sep="")
  }
  if (nchar(num)==2) {
    num<-paste("0",num, sep="")
  }
  
  dat$Provider.Number[nums]<-num
}

###---Add site IDs
dat$Dataset.ID<-NA
dat$Unique.ID<-NA
for (i in 1:length(dat$SGN_Site_ID)) {
  cty.num<-which(cty.code$country==dat$Country[i])
  if (dat$ecosystem[i]=="lotic") {
    dat$Dataset.ID[i]<-paste(cty.code$letters[cty.num], dat$Provider.Number[i], "MZB_LO", sep="_")
    dat$Unique.ID[i]<-paste(cty.code$letters[cty.num], dat$Provider.Number[i], "MZB_LO", dat$SGN_Site_ID[i], sep="_")
  }
  if (dat$ecosystem[i]=="lentic") {
    dat$Dataset.ID[i]<-paste(cty.code$letters[cty.num], dat$Provider.Number[i], "MZB_LE", sep="_")
    dat$Unique.ID[i]<-paste(cty.code$letters[cty.num], dat$Provider.Number[i], "MZB_LE", dat$SGN_Site_ID[i], sep="_")
  }
}

tream.sites2<-tream.sites
tream.sites2$Dataset.ID<-NA
tream.sites2$Unique.ID<-NA
tream.sites2$Longitude_X2<-NA
tream.sites2$Latitude_Y2<-NA

###---Update coordinates and add IDs
for (i in 1:length(tream.sites2$site_id)) {
  num<-which(dat$Ellens_ID==tream.sites2$site_id[i])
  tream.sites2$Longitude_X2[i]<-dat$Longitude_X[num]
  tream.sites2$Latitude_Y2[i]<-dat$Latitude_Y[num]
  tream.sites2$Dataset.ID[i]<-dat$Dataset.ID[num]
  tream.sites2$Unique.ID[i]<-dat$Unique.ID[num]
  tream.sites2$Site_ID_original[i]<-dat$Site_ID_original[num]
}
which(tream.sites2$Unique.ID %in% NA)
#found all sites

grep("LE", tream.sites2$Dataset.ID)
#48 is lentic

###---Get the data for each site
filenames <- list.files(pattern="*.csv") #load all file names in the "Community data" directory
filenames

#load the files that match the required datasets
file.nums<-numeric()
for (i in 1:length(unique(tream.sites2$Dataset.ID))) {
  nums2<-grep(unique(tream.sites2$Dataset.ID)[i], filenames)
  if (length(nums2)>0) {
    file.nums<-append(file.nums, nums2)
  }
}
file.nums<-sort(unique(file.nums))
filenames[file.nums]

myfiles<-list() #empty list
for (i in 1:length(file.nums)) { #for however many filenames you need
  myfiles[[i]]<-read.csv(filenames[file.nums[i]], header=TRUE, fileEncoding="latin1", sep=";") #load the data into the ith list position 
}

##--extract the data for each site into dat2

#first get a dataset with the right structure
dat2<-data.frame(Site.ID=NA, Sample.ID=NA, Sampling.date=NA, Taxon.name=NA, Taxon.ID=NA, Abundance=NA, Definition.of.abundance.class=NA, Abundance.class=NA, Site_ID_Ellen=NA, year=NA, month=NA, day=NA, sample_id=NA)

loop.num<-0.1
for (i in 1:length(tream.sites2$site_id)) {
  #get the datasets with the site in it
  nums<-grep(tream.sites2$Dataset.ID[i], filenames[file.nums])
  
  #loop though the datasets until you find the site, then break the loop
  for (j in 1:length(nums)) {
    sub<-myfiles[[nums[j]]]
    if (length(grep("X", colnames(sub)))>0) { #remove extra columns
      sub<-sub[,-grep("X", colnames(sub))]
    }
    
    #fix datasets
    if (tream.sites2$Dataset.ID[i]=="CHE_040_MZB_LO") {
      sub$Site.ID[which(sub$Site_ID_Ellen==118000005)]<-"Glatt_1_fall"
      sub$Site_ID_Ellen[which(sub$Site_ID_Ellen==118000005)]<-118000004
    }
    if (tream.sites2$Dataset.ID[i]=="LUX_027_MZB_LO") {
      sub$Abundance<-lux$abundance
    }
    if (tream.sites2$Dataset.ID[i]=="ESP_036_MZB_LO") {
      sub$Sampling.date[which(sub$Sampling.date=="27.11.2015" & sub$Site_ID_Ellen==103000714)]<-"27.08.2015"
    }
    if (tream.sites2$Dataset.ID[i]=="FRA_014_MZB_LO") {
      svd.nums<-which(sub$Site.ID=="svd")
      svg.nums<-which(sub$Site.ID=="svg")
      sub$Site_ID_Ellen[svd.nums]<-100000001
      sub$Site_ID_Ellen[svg.nums]<-100000002
    }
    if (tream.sites2$Dataset.ID[i]=="ITA_024_MZB_LO") {
      sub<-ita
    }
    if (tream.sites2$Dataset.ID[i]=="NLD_005_MZB_LO") {
      for (k in 1:length(sub$Sampling.date)) {
        sub$Sampling.date[k]<-strsplit(sub$Sample.ID[k], split="_")[[1]][4]
      }
    }
    if (tream.sites2$Dataset.ID[i]=="NLD_028_MZB_LO" & length(which(sub$Site_ID_Ellen==117000036))>0) {
      sub$Site.ID[which(sub$Site_ID_Ellen==117000036)]<-"oDIEZE_900"
    }
    
    #fix blank sampling dates
    if (sub$Sampling.date[1] %in% NA) {
      for (k in 1:length(sub$Sampling.date)) {
        sub$Sampling.date[k]<-strsplit(sub$Sample.ID[k], split="_")[[1]][2]
      }
    }
    
    sub2<-sub[which(sub$Site.ID==tream.sites2$Site_ID_original[i]),]
    if (length(sub2[,1])==0) { #some site IDs in the CSVs use Ellen's site IDs, e.g., Ralf's German data
      sub2<-sub[which(sub$Site.ID==tream.sites2$site_id[i]),]
    }
    if (length(sub2[,1])>0) {
      break
    }
  }
  
  #add site ID and sampling date info
  sub2$Site_ID_Ellen<-NA
  sub2$Site_ID_Ellen<-tream.sites2$site_id[i]
  sub2$year<-NA
  sub2$month<-NA
  sub2$day<-NA
  sub2$sample_id<-NA
   
  dates<-unique(sub2$Sampling.date) #add dates
  for (k in 1:length(dates)) {
    date.nums<-which(sub2$Sampling.date==dates[k])
    name<-strsplit(sub2$Sampling.date[date.nums[1]], split="\\.")
    sub2$year[date.nums]<-as.numeric(name[[1]][3])
    sub2$month[date.nums]<-as.numeric(name[[1]][2])
    
    if (name[[1]][1]=="NA" | tream.sites2$Dataset.ID[i]=="ITA_024_MZB_LO") {
      sub2$day[date.nums]<-NA
    } else {
      sub2$day[date.nums]<-as.numeric(name[[1]][1])
    }
    
    date<-as.POSIXlt(paste(sub2$day[date.nums[1]], sub2$month[date.nums[1]], sub2$year[date.nums[1]], sep="."), format="%d.%m.%Y")
    sub2$sample_id[date.nums]<-paste(sub2$Site_ID_Ellen[date.nums[1]], format(date, "%d.%m.%Y"), sep="_")
    
    if (name[[1]][1]=="NA") {
      date<-as.POSIXlt(paste("01", sub2$month[date.nums[1]], sub2$year[date.nums[1]], sep="."), format="%d.%m.%Y")
      sub2$sample_id[date.nums]<-paste(sub2$Site_ID_Ellen[date.nums[1]], "_NA.", format(date, "%m.%Y"), sep="")
    }
    
    if (tream.sites2$Dataset.ID[i]=="ITA_024_MZB_LO") {
      sub2$year[date.nums]<-as.numeric(strsplit(sub2$Sampling.date[date.nums[1]], split="_")[[1]][3])
      sub2$month[date.nums]<-"6to8"
      date<-paste("6to8", strsplit(sub2$Sampling.date[date.nums[1]], split="_")[[1]][3], sep=".")
      sub2$sample_id[date.nums]<-paste(sub2$Site_ID_Ellen[date.nums[1]], date, sep="_")
    }
  }
  
  #fix abundance
  sub2$Abundance<-as.numeric(gsub(",", replacement="\\.", sub2$Abundance))
  sub2<-subset(sub2, sub2$Abundance != 0)
  
  #fix Belgian methods
  if (length(which(sub2$sampling.method=="Artificieel substraat"))>0) {
    sub2$sampling.method[which(sub2$sampling.method=="Artificieel substraat")]<-"Artificial substrate"
  }
  
  #extract data for each sampling date for this site from tream.yrs and save it in dat2
  sub.yrs<-subset(tream.yrs, tream.yrs$site_id==sub2$Site_ID_Ellen[1])

  for (k in 1:length(sub.yrs$sample_id)) {
    if (sub.yrs$site_id_wMissing[k] %nin% NA & all(be.wrong$ellen_id %nin% tream.sites2$site_id[i])) {
      nums2<-which(sub2$year==sub.yrs$year[k] & sub2$month==sub.yrs$month[k] & sub2$day %in% sub.yrs$day[k])
      if (length(which(sub2$sampling.method %in% "Artificial substrate"))>0) {
        nums2<-which(sub2$year==sub.yrs$year[k] & sub2$month==sub.yrs$month[k] & sub2$day==sub.yrs$day[k] & sub2$sampling.method==sub.yrs$sampling_method_long[k])
      }
      sub3<-cbind(sub2[nums2,c(1:9)], sub2$year[nums2], sub2$month[nums2], sub2$day[nums2], sub2$sample_id[nums2])
      colnames(sub3)[1:13]<-colnames(dat2)[1:13]
      dat2<-rbind(dat2, sub3[,c(1:13)])
    }
  }
  
  #For Belgian sites w incorrect dates
  if (any(be.wrong$ellen_id %in% tream.sites2$site_id[i])) {
    sub2<-subset(be.right, be.right$Site_ID_Ellen==tream.sites2$site_id[i])
    if (length(sub2[,1])>0) {
      date<-as.POSIXlt(paste(sub2$day, sub2$month, sub2$year, sep="."), format="%d.%m.%Y")
      sub2$sample_id<-paste(sub2$Site_ID_Ellen, format(date, "%d.%m.%Y"), sep="_")
      sub3<-cbind(sub2[,c(1:9)], sub2$year, sub2$month, sub2$day, sub2$sample_id)
      colnames(sub3)[1:13]<-colnames(dat2)[1:13]
      dat2<-rbind(dat2, sub3[,c(1:13)])
    }
  }
  
  if (i/length(tream.sites2$site_id)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  } 
}
dat2<-dat2[-1,]

length(unique(dat2$Site_ID_Ellen))
#4 sites removed from Belgium

length(unique(tream.yrs$code_wMissing))
length(unique(dat2$sample_id))

samp.nums<-numeric()
samps<-unique(tream.yrs$code_wMissing)
for (i in 1:length(samps)) {
  num<-which(tream.yrs$code_wMissing==samps[i])
  if(length(which(dat2$Site_ID_Ellen==tream.yrs$site_id[num] & dat2$year==tream.yrs$year[num]))>0) {
    samp.nums[i]<-1
  } else {
    samp.nums[i]<-0
  }
}

samps[which(samp.nums==0)]


write.csv(tream.sites2, "Ellen sites re-processed.csv", row.names=FALSE, fileEncoding = "latin1")
write.csv(dat2, "Ellen taxa re-processed.csv", row.names=FALSE, fileEncoding = "latin1")

#1) Remove extra right-side columns from CSVs, typically 2
#2) BGR_007_MZB_LO, CZE_010_MZB_LO sampling dates
#3) Don Monteith coordinates
#4) LUX_027_MZB_LO abundance w decimal (one 3.2, replace w 3)
#5) NLD_028_MZB_LO2 some of oDIEZE_550 is oDIEZE_900 (117000036)
#6) svd/svg and Ellen's site IDs in Maxence Forcellini's data (FRA_014_MZB_LO) are reversed
#7) NLD_005_MZB_LO sampling dates for second site
#8) CYP_009_MZB_LO1 & 2 merge into one dataset
#9) ITA_024_MZB_LO switch to combined data
#10) CHE_040_MZB_LO site 118000005 is 118000004, spring and fall were both included
#11) ESP_036_MZB_LO site 4 correct sampling date in 2015 from 11/27/2015 to 8/27/2015
#12) Provide new Belgian data with missing methods

#For Ellen
#1) Code error 100000145_2011 should be 100000145_2012 and vice versa
#2) Cannot find 106000004_28.6.2012 in original data (also looked for example taxon Atherix sp. at 28.8 and cant find it)

#remove any unnecessary columns, remove double whitespaces, and re-load
dat3<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

###---Taxonomic adjustment
dict<-read.csv(file.choose(), header=TRUE, encoding="latin1")

##--Check for complete dictlist
dat3$Taxon.name[which(dat3$Taxon.name==5605)]<-"Hydropsyche sp." #number in the taxon name
dat3$Taxon.name<-trimws(dat3$Taxon.name) #remove trailing whitespaces
#dat2[which(dat2$Taxon.name==5605),] #103000713 & 103000718
#tream.sites2[which(tream.sites2$site_id==103000713),] #ESP_036_MZB_LO

loop.num<-0.1
num<-numeric()
names<-unique(dat3$Taxon.name)
names2<-toupper(names)
for (i in 1:length(names2)) {
  num2<-which(dict$provided_upper==names2[i])
  if (length(num2)==0) {
    num<-append(num, i)
  }
  
  if (i/length(names2)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  } 
}
num
names[num]

#have to manually fix some issues
dat3$Taxon.name[which(dat3$Taxon.name==names[num][1])]<-"Nematomorpha Gen. sp."
dat3$Taxon.name[which(dat3$Taxon.name==names[num][2])]<-"Cordulegaster boltonii"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][3])]<-"Rhyacophila mocsaryi Klapalek"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][4])]<-"Einfeldia longipes (Staeger)"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][5])]<-"Gyraulus laevis (Alder)"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][6])]<-"Protonemura brevistyla (Ris)"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][7])]<-"Centroptilum pennulatum Eaton"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][8])]<-"Protonemura montana Kimmins"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][9])]<-"Ecdyonurus fascioculata Sowa"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][10])]<-"Aeolosoma hemprichii Ehrenberg"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][11])]<-"Psychomyia pusilla (Fabricius)"
dat3$Taxon.name[which(dat3$Taxon.name==names[num][12])]<-"Austropotamobius torrentium (Schrank)"


##--Adjustment
dat3$Taxon.name<-toupper(dat3$Taxon.name)
dat3$Taxon.name2<-NA
dat3$Taxon.ID2<-NA
trait<-dat3

taxa.nums<-numeric()
loop.num<-0.1
names<-unique(dat3$Taxon.name)
for (i in 1:length(names)) {
  num<-which(dict$final_name==names[i])[1]
  nums<-which(dat3$Taxon.name==names[i])
  if (num %nin% NA) {
    taxa.nums[i]<-NA
    dat3$Taxon.name2[nums]<-dict$final_name[num]
    dat3$Taxon.ID2[nums]<-dict$final_aqem[num]
    trait$Taxon.name2[nums]<-dict$trait_name[num]
    trait$Taxon.ID2[nums]<-dict$final_aqem[num]
  } else {
    num<-which(dict$provided_upper==names[i])[1]
    if (dict$final_name[num]=="REMOVE") {
      taxa.nums[i]<-0
      dat3$Taxon.name2[nums]<-"REMOVE"
      trait$Taxon.name2[nums]<-"REMOVE"
    } else {
      taxa.nums[i]<-1
      dat3$Taxon.name2[nums]<-dict$final_name[num]
      dat3$Taxon.ID2[nums]<-dict$final_aqem[num]
      trait$Taxon.name2[nums]<-dict$trait_name[num]
      trait$Taxon.ID2[nums]<-dict$final_aqem[num]
    }
  }
  if (i/length(names)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  } 
}
names[which(taxa.nums==0)]
dat3<-subset(dat3, dat3$Taxon.name2 %nin% "REMOVE")
trait<-subset(trait, trait$Taxon.name2 %nin% "REMOVE")

dat3$Taxon.name<-dat3$Taxon.name2
dat3$Taxon.ID<-dat3$Taxon.ID2
dat3<-dat3[,c(1:11)]

trait$Taxon.name<-trait$Taxon.name2
trait$Taxon.ID<-trait$Taxon.ID2
trait<-trait[,c(1:11)]

###---Re-sum abundances
dates<-unique(dat3$sample_id)

dat4<-dat3[1,]
loop.num<-0.1
for (i in 1:length(dates)) {
  sub<-subset(dat3, dat3$sample_id==dates[i])
  if(any(as.numeric(table(sub$Taxon.name))>1)) {
    sub2<-sub[1,]
    names<-sort(unique(sub$Taxon.name))
    loop.num1<-2
    for (j in 1:length(names)) {
      sub2<-rbind(sub2, sub[1,])
      nums<-which(sub$Taxon.name==names[j])
      sub2$Taxon.name[loop.num1]<-sub$Taxon.name[nums[1]]
      sub2$Taxon.ID[loop.num1]<-sub$Taxon.ID[nums[1]]
      sub2$Abundance[loop.num1]<-sum(sub$Abundance[nums])
      loop.num1<-loop.num1+1
    }
    sub2<-sub2[-1,]
    dat4<-rbind(dat4, sub2)
  } else {
    dat4<-rbind(dat4, sub)
  }
  if (i/length(dates)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  } 
}
dat4<-dat4[-1,]

trait2<-trait[1,]
loop.num<-0.1
for (i in 1:length(dates)) {
  sub<-subset(trait, trait$sample_id==dates[i])
  if(any(as.numeric(table(sub$Taxon.name))>1)) {
    sub2<-sub[1,]
    names<-sort(unique(sub$Taxon.name))
    loop.num1<-2
    for (j in 1:length(names)) {
      sub2<-rbind(sub2, sub[1,])
      nums<-which(sub$Taxon.name==names[j])
      sub2$Taxon.name[loop.num1]<-sub$Taxon.name[nums[1]]
      sub2$Taxon.ID[loop.num1]<-sub$Taxon.ID[nums[1]]
      sub2$Abundance[loop.num1]<-sum(sub$Abundance[nums])
      loop.num1<-loop.num1+1
    }
    sub2<-sub2[-1,]
    trait2<-rbind(trait2, sub2)
  } else {
    trait2<-rbind(trait2, sub)
  }
  if (i/length(dates)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  } 
}
trait2<-trait2[-1,]

write.csv(dat4, "Ellen - harmonized taxa.csv", row.names=FALSE, fileEncoding="latin1")
write.csv(trait2, "Ellen - harmonized trait.csv", row.names=FALSE, fileEncoding="latin1")


###---Double check the changes
taxa.check<-data.frame(id=unique(dat4$sample_id), taxa.num=NA, trait.num=NA, diff=NA)

for (i in 1:length(taxa.check$id)) {
  sub<-dat4[which(dat4$sample_id==taxa.check$id[i]),]
  sub2<-trait2[which(trait2$sample_id==taxa.check$id[i]),]
  taxa.check$taxa.num[i]<-length(sub$Taxon.name)
  taxa.check$trait.num[i]<-length(sub2$Taxon.name)
  taxa.check$diff[i]<-taxa.check$taxa.num[i]-taxa.check$trait.num[i]
}

sum(taxa.check$diff)
taxa.check$id[which(taxa.check$diff != 0)]
#wild