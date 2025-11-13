#this code harmonizes the taxon names for each dataset to the freshwaterecology.info database

###---Load libraries
library(Hmisc)

###---Set the criteria for site inclusion
eco.type<-"LO" #lotic, _LE for lentic
samp.num<-7 #must have at least this many sampling years
dur.num<-10 #must have at least this much total time series duration

###---Set directories for saving files
taxa.dir<-paste0(getwd(), "/Harmonized, Tax=MZB, Eco=", eco.type,", Num=", samp.num, ", Dur=", dur.num) #use this directory if you followed Step 6, path 1
taxa.dir<-paste0(getwd(), "/Harmonized for EQRs, Tax=MZB, Eco=", eco.type,", Num=", samp.num, ", Dur=", dur.num) #use this directory if you followed Step 6, path 2
if(!dir.exists(taxa.dir)) dir.create(taxa.dir)

###---Load the harmonization sheet
#https://docs.google.com/spreadsheets/d/1pcpr-NXxbCcQL4a3V8o0McSnvr9t1dId/edit?usp=sharing&ouid=109310749942317730589&rtpof=true&sd=true
harmon<-read.csv(file.choose(), header=TRUE, fileEncoding ="latin1")

###---Load all taxa files, set working directory to the "Selected taxa" directory from Step 6
(taxa.names<-list.files(pattern="*.csv"))

taxa.files<-list() #empty list
for (i in 1:length(taxa.names)) { #for however many file names
  taxa.files[[i]]<-read.csv(taxa.names[i], header=TRUE, fileEncoding="latin1") #load the data into the ith list position 
}

###################### START OF THE DATASET LOOP ###########################################

options(warn=2) #will end the loop if any warnings are encountered
loop.num<-0.1 #for tracking progress through the for loop
stop.loop<-FALSE #for stopping the loop if an error is encountered

taxa.names
data.nums<-c(1) #use to run a specific dataset or set of specific datasets
data.nums<-c(1:length(taxa.names)) #use to run datasets starting at a specified number until the end

for (i in data.nums) {
  dat<-taxa.files[[i]]
  
  #Remove trailing whitespaces
  dat$Taxon.name<-trimws(dat$Taxon.name)
  
  #Remove double whitespaces
  dat$Taxon.name<-gsub("  ", replacement=" ", dat$Taxon.name)
  
  ##--Check that all taxa are listed in the harmonization sheet
  names<-unique(dat$Taxon.name)
  names2<-toupper(names)
  for (j in 1:length(names2)) {
    num<-which(harmon$provided_upper==names2[j])
    if (length(num)==0) {
      print("Harmonization stopped - found a taxon that is not in the harmonization sheet, add manually")
      stop.loop<-TRUE #set call for a stop to true
      break() #break from this loop
    }
  }
  
  if (stop.loop==TRUE) { #if a stop has been called
    break() #break the dataset for loop
  }
  
  ##--Convert taxon names to uppercase
  dat$Taxon.name<-toupper(dat$Taxon.name)
  dat$Taxon.name2<-NA
  
  ##--Harmonize taxon names
  taxa.nums<-numeric()
  names<-unique(dat$Taxon.name)
  for (j in 1:length(names)) {
    num<-which(harmon$final_name==names[j])[1]
    nums<-which(dat$Taxon.name==names[j])
    if (num %nin% NA) {
      taxa.nums[j]<-NA
      dat$Taxon.name2[nums]<-harmon$final_name[num]
    } else {
      num<-which(harmon$provided_upper==names[j])[1]
      if (harmon$final_name[num]=="REMOVE") {
        taxa.nums[j]<-0
        dat$Taxon.name2[nums]<-"REMOVE"
      } else {
        taxa.nums[j]<-1
        dat$Taxon.name2[nums]<-harmon$final_name[num]
      }
    }
  }
  
  ##--Remove taxa marked for removal (typically those that are primarily terrestrial, marine, etc)
  dat2<-subset(dat, dat$Taxon.name2 %nin% "REMOVE")
  
  ##--Transfer the new names and trim the last column
  dat2$Taxon.name<-dat2$Taxon.name2
  dat2<-dat2[,-9]
  
  ###---Re-sum abundances for different taxa that are now the same taxon
  dates<-unique(dat2$Samp.ID)
  
  dat3<-dat2[1,] #create a blank dataset for saving finalized sample data
  for (j in 1:length(dates)) {
    sub<-subset(dat2, dat2$Samp.ID==dates[j]) #for each sampling date
    #check if any taxon name occurs more than once
    if(any(as.numeric(table(sub$Taxon.name))>1)) { #if it does, cycle through each taxon and sum abundances for those with the same name
      sub2<-sub[1,] #create a blank dataset for saving finalized abundance data for each taxon
      names<-sort(unique(sub$Taxon.name))
      row.num<-2 #use to build a new dataset starting at row 2, after the dummy row 1
      for (k in 1:length(names)) { #cycle through each taxon
        sub2<-rbind(sub2, sub[1,]) #insert a row with the site & sample info for this taxon
        nums<-which(sub$Taxon.name==names[k]) #identify the rows with this taxon in sub
        sub2$Taxon.name[row.num]<-sub$Taxon.name[nums[1]] #save the taxon name in the row for this taxon
        sub2$Abundance[row.num]<-sum(sub$Abundance[nums]) #insert the summed abundance values into the row for this taxon
        row.num<-row.num+1 #tick up to the next row
      }
      sub2<-sub2[-1,] #remove the first dummy row
      dat3<-rbind(dat3, sub2) #save the new data for this sample
    } else { #if multiple taxa with the same name are NOT found
      dat3<-rbind(dat3, sub) #save the sample data as is
    }
  }
  dat3<-dat3[-1,] #remove the first dummy row
  
  ##--Save the datasets
  name<-taxa.names[i]
  name<-gsub(" - selected taxa sites.csv", "", name)
  write.csv(x=dat3, file=paste(taxa.dir, "/", name, " - harmonized.csv", sep=""), row.names=FALSE, fileEncoding="latin1")
  
  ##--Report progress
  if (i/length(taxa.names)>=loop.num) {
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  } 
}

###################### END OF THE DATASET LOOP ###########################################


##--If the for loop breaks, use this code to manually check which taxa need to be added to the harmonization sheet
taxa.names[i]
dat<-taxa.files[[i]]
dat$Taxon.name<-trimws(dat$Taxon.name)
dat$Taxon.name<-gsub("  ", replacement=" ", dat$Taxon.name)

num<-numeric()
names<-sort(unique(dat$Taxon.name))
names2<-toupper(names)
for (j in 1:length(names2)) {
  num2<-which(harmon$provided_upper==names2[j])
  if (length(num2)==0) {
    num<-append(num, j)
  }
}
names[num]
