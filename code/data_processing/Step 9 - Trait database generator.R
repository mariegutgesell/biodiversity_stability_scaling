###---v1.2 produced on 13.10.2025
###---Code authored by James S. Sinclair for use by the Haase Lab, Division of River Ecology and Conservation, Senckenberg Research Institute

##Load required libraries (install if necessary)
library(Hmisc)
library(stringr)

##Set directory for saving files
taxa.dir<-paste0(getwd(),"/Trait data - Num=5, Dur=10") #use if following Step 6, path 1
taxa.dir<-paste0(getwd(),"/Trait data for EQRs - Num=7, Dur=10") #use if following Step 6, path 2
if(!dir.exists(taxa.dir)) dir.create(taxa.dir)

###---Load all taxa files, set working directory to the "Finalized taxa - long form" directory from Step 8
(taxa.names<-list.files(pattern="*.csv"))

taxa.files<-list() #empty list
for (i in 1:length(taxa.names)) { #for however many file names
  taxa.files[[i]]<-read.csv(taxa.names[i], header=TRUE, fileEncoding="latin1") #load the data into the ith list position 
}

###---Load each file and make a list of taxa
names<-character()
for (i in 1:length(taxa.names)) {
  dat<-taxa.files[[i]]
  names<-append(names, unique(dat$Taxon.name)) #uses names from the "Taxon.name" column in each dataset
}
taxa<-data.frame(name=sort(unique(names)))
str(taxa) #make sure everything reads in correctly

##--Load the FWE taxa dictionary titled "FWE taxa dictionary v8 - w traits and DISPERSE.csv"
dict<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #trait categories must have unique names and modalities should follow those in FWE
str(dict, list.len=ncol(dict))

#any taxa in the DISPERSE dataset have also had their trait values for body size, life duration, reproductive cycles, and dispersal applied to 
#the same Tachet traits because the DISPERSE values should be more up-to-date

##Convert names to uppercase
dict$name<-toupper(as.character(dict$name))
dict$fam<-toupper(as.character(dict$fam))
dict$name1<-toupper(as.character(dict$name1))
dict$name2<-toupper(as.character(dict$name2))

##--Set the list of taxa that used averaged traits extracted directly from the database
av.taxa<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")
av.taxa$averaged<-toupper(av.taxa$averaged)

##--Get the unique trait names from the dictionary
(traits<-unique(gsub("_.*", "", x=colnames(dict)[6:length(colnames(dict))])))

##--Create a new dataframe to hold all the trait values for each taxon from your taxa list
taxa2<-as.data.frame(taxa[,1])
colnames(taxa2)[1]<-c("name")
taxa2$taxon<-NA
taxa2$aqem<-NA
taxa2$gen<-NA
taxa2$sp<-NA

##--Check if the given names match a name in the dictionary
options(warn=2) #turn warnings into errors which will end any loop and allow you to identify the problem (warn=1 is the default)
for (i in 1:length(taxa2$name)) {
  nums<-which(dict$name==taxa2$name[i])
  if (length(nums)>0) { #if it found the exact name
    sub<-dict[nums,]
    if (length(sub$name)>1) { #if it found duplicate names
      nums2<-numeric() #find which one has the most trait values
      for (j in 1:length(sub$name)) {
        nums2[j]<-length(which(sub[j,c(6:length(colnames(sub)))]>0))
      }
      if (length(which(nums2>0))>0) {
        sub<-sub[which(nums2==max(nums2)),] #take the one with the most trait values
      }
      if (length(which(nums2>0))==0) {
        sub<-sub[1,] #otherwise just take the first one
      }
    }
    taxa2$taxon[i]<-sub$name
  }
  if (length(nums)==0) { #if it cannot find the exact name (e.g., perhaps it is missing the Ad. or Lv. specification)
    nums<-grep(paste("(?m)^", taxa2$name[i], "", sep=""), dict$name, perl=TRUE) #run a search using the name
    #the PERL regular expression (?m)^ ensures no text comes before the start of the species name (e.g., CYPHON matching CONTACYPHON)
    if (length(nums)>0) { #if it found something
      nums<-nums[1] #take the first entry
      sub<-dict[nums,]
      taxa2$taxon[i]<-sub$name
    }
  }
  if (length(nums)==0) { #if it still cannot find the exact name
    nums<-grep(taxa2$name[i], dict$name, fixed=TRUE)[1] #run a search using the name and take the first entry
    if (length(nums)>0) { #if it found something
      sub<-dict[nums,]
      taxa2$taxon[i]<-sub$name
    }
  }
}

##--Load the taxa harmonization sheet
harmon<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

#Check if any taxon names match the subset of the harmonization sheet that use different trait names
harmon2<-subset(harmon, harmon$final_trait_match=="No Match" & harmon$explanation != "Coleopteran")
for (i in 1:length(taxa2$name)) {
  rnum<-which(harmon2$final_name==taxa2$name[i])[1]
  if (rnum %nin% NA) {
    taxa2$taxon[i]<-harmon2$trait_name[rnum]
  }
}

##--Check if any taxa are missing, these will have to be fixed manually
taxa2[which(taxa2$taxon %in% NA),]

##--Re-run the name check and fill in the info from the dictionary using the adjusted taxon names
#this will error if there are any further problems matching the adjusted taxon names to the dictionary,
#which can be checked and fixed manually
for (i in 1:length(taxa2$taxon)) {
  num<-which(dict$name==taxa2$taxon[i])
  taxa2$aqem[i]<-dict$aqem[num]
  taxa2$gen[i]<-dict$name1[num]
  taxa2$sp[i]<-dict$name2[num]
}

##--Check which taxa had to be altered, i.e., those in which taxa2$taxon is different from taxa2$name
sub<-taxa2[which(match(taxa2$name, taxa2$taxon) %in% NA),]
View(sub)
#can open sub to look at these entries
#they are usually Coleopterans who are missing an Ad./Lv. specification and the code will automatically assign
#the first one it finds that matches the name, which is the Ad. version. Adults and larvae generally have the same
#trait values but make sure this adjustment is suitable for your project/question. If not you will need to find out
#whether the taxa you have identified are adults or larvae and assign that specification yourself and re-run the generator


##--Create empty columns to hold the trait values
taxa2[,colnames(dict)[6:length(colnames(dict))]]<-NA

##Create an empty dataframe to track the level at which each trait value was assigned
#1 is used when values are pulled directly from the dictionary, 0 is used when values are produced by averaging
cover<-taxa2[,c(1:3)]
for (i in 1:length(traits)) {
  cover[, (paste(traits[i], c("aqem", "subspecies", "genus", "family"), sep="_"))]<-NA
}


###---AQEM level

##Check for AQEM-level trait values first, this includes the species-level data if available
options(warn=2) #stop the loop if any warnings appear
count<-0.1 #used to track progress through the loop
for (i in 1:length(taxa2$taxon)) {
  sub<-dict[which(dict$name==taxa2$taxon[i]), 6:length(colnames(dict))] #find the associated taxon in the database
  #If you have an -Agg. or -Gr. taxon with little data then use the associated species entry, comment this IF statement out to skip this step 
  if (length(grep("-AGG\\.", taxa2$taxon[i]))>0 | length(grep("-GR\\.", taxa2$taxon[i]))>0) {
    if (sum(as.numeric(sub), na.rm=TRUE)<11) { #if the taxon has little data then do this, many have only feeding traits, i.e., sum=10
      name1<-str_remove(taxa2$taxon[i], "-AGG.") #remove -Agg. text
      name1<-str_remove(name1, "-GR.") #remove -Gr. text
      sub<-dict[grep(name1, dict$name)[1],] #search for the name that remains and take the first entry
    }
  }
  for (j in 1:length(traits)) { #for each trait
    sub2<-sub[,grep(traits[j], colnames(sub))] #get the associated trait values
    if (sum(as.numeric(sub2), na.rm=TRUE)>0) { #if data is available
      taxa2[i , grep(traits[j], colnames(taxa2))]<-as.numeric(sub2) #then put that data in
      cover[i, grep(traits[j], colnames(cover))][1]<-1 #set the coverage value to 1
      
      #adjust the coverage value to 0 if an averaged trait value was extracted directly from the database
      if (any(av.taxa$averaged %in% taxa2$taxon[i])) {
        cover[i, grep(traits[j], colnames(cover))][1]<-0
      }
      
    }
  }
  if (i/length(taxa2$taxon)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}

###---Subspecies level

##Use subspecies averages for each subspecies if they have no data but others from the same subspecies do
nums<-numeric()
count<-1

#First identify the subspecies
for (i in 1:length(taxa2$taxon)) {
  sub<-strsplit(taxa2$sp[i], " ")
  if (length(sub[[1]])>1 & sub[[1]][1] != "GEN." & sub[[1]][2] != "AD." & sub[[1]][2] != "LV." & length(grep("\\(", sub[[1]][1]))==0 & length(grep("\"", sub[[1]][1]))==0 & length(grep("/", sub[[1]][1]))==0 & taxa2$aqem[i] != 7456) {
    #this IF statement isolates entries with more than 2 names, but excludes those that have more than 2 because they are Gen. sp., Ad., Lv., multiple species (/), etc
    #also excludes Rhyacophila s. str. sp. (AQEM 7456) to ensure its values are assigned at the genus level
    nums[count]<-i
    count<-count+1
  }
}
#nums holds the positions of all subspecies in the dataset

##These are the subspecies in your original taxalist, check to make sure these are indeed subspecies
taxa2[nums,c(1:2)] #if none then you can skip the next loop and move to the genus level

##--Trait averaging for the subspecies
for (i in nums) { #for each subspecies
  sub<-dict[which(dict$name==taxa2$taxon[i]),] #get the associated taxon entry from the dictionary
  for (j in 1:length(traits)) { #for each trait
    if (sum(taxa2[i , grep(traits[j], colnames(taxa2))], na.rm=TRUE)==0) { #if the trait has no values
      sub1<-grep(taxa2$gen[i], dict$name1) #then search for the genus
      sub2<-grep(strsplit(taxa2$sp[i], split=" ")[[1]][1], dict$name2) #and the first part of the subspecies name
      sub3<-dict[sub1[which(sub1 %in% sub2)], c(1:5,grep(traits[j], colnames(dict)))] #take only members of the genus that have the first subspecies name
      #check for an SSP. value first
      if (sum(unlist(sub3[grep("SSP.", sub3$name2),-c(1:5)]), na.rm=TRUE)>0) { #if it has subspecies-level values
        taxa2[i , grep(traits[j], colnames(taxa2))]<-sub3[grep("SSP.", sub3$name2),-c(1:5)] #then use those values
        cover[i, grep(traits[j], colnames(cover))][2]<-1 #store a 1 for the cover value
      } else if (sum(unlist(sub3[,-c(1:5)]), na.rm=TRUE)>0) { #if no SSP. value but other subspecies have trait values
        if (length(colnames(sub3[,-c(1:5)]))==1) { #if only one trait modality
          taxa2[i , grep(traits[j], colnames(taxa2))]<-mean(sub3[,-c(1:5)], na.rm=TRUE) #then use the average of that modality
        }
        if (length(colnames(sub3[,-c(1:5)]))>1) { #if multiple modalities
          taxa2[i , grep(traits[j], colnames(taxa2))]<-colMeans(sub3[,-c(1:5)], na.rm=TRUE) #then use averaged values across the subspecies
        }
        cover[i, grep(traits[j], colnames(cover))][2]<-0 #store a 0 for the cover value since averages have been used
      }
    }
  }
}


###---Genus level

##Genus - if still missing data get genus level data
count<-0.1
for (i in 1:length(taxa2$taxon)) { #for each taxon
  for (j in 1:length(traits)) { #for each trait
    if (sum(taxa2[i, grep(traits[j], colnames(taxa2))], na.rm=TRUE)==0) { #if it has no trait data
      sub<-dict[which(dict$name==taxa2$taxon[i]),]
      if (length(grep("GEN. SP\\.", sub$name2))==0) { #if it has a genus (i.e., not family or subfamily)
        sub1<-subset(dict, dict$name1==sub$name1) #get all data for that genus
        nums1<-which(sub1$name2 == "SP.") #find if there is a genus-level entry
        if (length(grep("AD\\.", sub$name2[i]))>0) { #if it has an Adult specification
          sub1<-sub1[grep("AD\\.", sub1$name2),] #then get only the Adult entries
          nums1<-which(sub1$name2 == "SP. AD.")
        }
        if (length(grep("LV\\.", sub$name2[i]))>0) { #if it has a Larvae specification
          sub1<-sub1[grep("LV\\.", sub1$name2),] #then get only the Larvae entries
          nums1<-which(sub1$name2 == "SP. LV.")
        }
        nums2<-grep("SP\\.", sub1$name2) #find all the species-level entries
        sub2<-as.data.frame(sub1[nums1, grep(traits[j], colnames(sub1))]) #get the genus-level entry
        sub3<-as.data.frame(sub1[-nums2, grep(traits[j], colnames(sub1))]) #get only species-level entries for that genus
        if (length(nums1)==0) { #if there is no genus-level entry
          sub3<-sub1[, grep(traits[j], colnames(sub1))] #then sub3 is just sub1
        }
        if (sum(unlist(sub2), na.rm=TRUE)>0) { #if there is genus-level data
          if (length(colnames(sub2))==1) { #if only one trait modality
            taxa2[i , grep(traits[j], colnames(taxa2))]<-mean(sub2[,1], na.rm=TRUE) #use the value
          }
          if (length(colnames(sub2))>1) { #if multiple modalities
            taxa2[i , grep(traits[j], colnames(taxa2))]<-colMeans(sub2, na.rm=TRUE) #use the mean of each, colMeans is used to deal with instances of multiple genus-level entries, generally only one of which has data
          }
          cover[i, grep(traits[j], colnames(cover))][3]<-1 #and set the cover value to 1
        }
        if (sum(unlist(sub2), na.rm=TRUE)==0) { #if no genus-level data is available
          if (sum(unlist(sub3), na.rm=TRUE)>0) { #and if species-level values are available
            if (length(colnames(sub3))==1) { #if only one trait modality
              taxa2[i , grep(traits[j], colnames(taxa2))]<-mean(sub3[,1], na.rm=TRUE) #then use that value
            }
            if (length(colnames(sub3))>1) { #if multiple modalities
              taxa2[i , grep(traits[j], colnames(taxa2))]<-colMeans(sub3, na.rm=TRUE) #then average across all taxa within the genus
            }
            cover[i, grep(traits[j], colnames(cover))][3]<-0 #and set the cover value to 0
          }
        }
      }
    }
  }
  if (i/length(taxa2$taxon)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}


###---Family level
count<-0.1
for (i in 1:length(taxa2$taxon)) { #for each taxon
  for (j in 1:length(traits)) { #for each trait
    if (sum(taxa2[i, grep(traits[j], colnames(taxa2))], na.rm=TRUE)==0) { #if it has no trait data
      sub<-dict[which(dict$name==taxa2$taxon[i]),]
      if (sub$fam %nin% NA) { #if it has a family
        sub1<-subset(dict, dict$fam==sub$fam) #get all data for the associated family
        nums1<-grep(paste(sub$fam, "GEN. SP.", sep=" "), sub1$name) #find if there is a family-level entry
        if (length(grep("AD\\.", sub$name2[i]))>0) { #if it has an Adult specification
          sub1<-sub1[grep("AD\\.", sub1$name2),] #then get only the Adult entries
          nums1<-grep(paste(sub$fam, "GEN. SP. AD.", sep=" "), sub1$name) #find if there is a family-level entry
        }
        if (length(grep("LV\\.", sub$name2[i]))>0) { #if it has a Larvae specification
          sub1<-sub1[grep("LV\\.", sub1$name2),] #then get only the Larvae entries
          nums1<-grep(paste(sub$fam, "GEN. SP. LV.", sep=" "), sub1$name) #find if there is a family-level entry
        }
        sub2<-as.data.frame(sub1[nums1, grep(traits[j], colnames(sub1))]) #get the family-level entry
        sub3<-as.data.frame(sub1[-nums1, grep(traits[j], colnames(sub1))]) #get all non-family-level for that family
        if (length(nums1)==0) { #if there is no family-level entry (nums1) then -nums1 wont work so do this instead
          sub3<-sub1[, grep(traits[j], colnames(sub1))] 
        }
        if (sum(unlist(sub2), na.rm=TRUE)>0) { #if there is family-level data
          if (length(colnames(sub2))==1) { #if only one trait modality
            taxa2[i , grep(traits[j], colnames(taxa2))]<-mean(sub2[,1], na.rm=TRUE) #use the value
          }
          if (length(colnames(sub2))>1) { #if multiple modalities
            taxa2[i , grep(traits[j], colnames(taxa2))]<-colMeans(sub2, na.rm=TRUE)
          }
          cover[i, grep(traits[j], colnames(cover))][4]<-1 #and set the cover value to 1
        }
        if (sum(unlist(sub2), na.rm=TRUE)==0) { #if there is no family-level data
          if (sum(unlist(sub3), na.rm=TRUE)>0) { #and if taxa values are available for other taxa within the family
            if (length(colnames(sub3))==1) { #if only one trait modality
              taxa2[i , grep(traits[j], colnames(taxa2))]<-mean(sub3[,1], na.rm=TRUE) #then use that value
            }
            if (length(colnames(sub3))>1) { #if multiple modalities
              taxa2[i , grep(traits[j], colnames(taxa2))]<-colMeans(sub3, na.rm=TRUE) #then average across all taxa within the family
            }
            cover[i, grep(traits[j], colnames(cover))][4]<-0 #and set the cover value to 0
          }
        }
      }
    }
  }
  if (i/length(taxa2$taxon)>=count) { #report progress
    cat(paste(count*100,"% ", sep=""))
    count<-count+0.1
  } 
}


##--Percent and total coverage for each trait at each level
cover<-rbind(cover, NA, NA, NA)
cover$taxon[length(cover$taxon)-2]<-"Percent not averaged"
cover$taxon[length(cover$taxon)-1]<-"Percent using averages"
cover$taxon[length(cover$taxon)]<-"Total percent"

for (i in 4:length(colnames(cover))) {
  cover[length(cover$taxon)-2, i]<-length(which(cover[(1:(length(cover$taxon)-3)),i]==1))/length(taxa2$aqem)
  cover[length(cover$taxon)-1, i]<-length(which(cover[(1:(length(cover$taxon)-3)),i]==0))/length(taxa2$aqem)
  cover[length(cover$taxon), i]<-cover[length(cover$taxon)-2, i]+cover[length(cover$taxon)-1, i]
}

##--Save the trait and coverage data to your working directory
write.csv(taxa2, paste(taxa.dir, "/Trait database.csv", sep=""), row.names=FALSE)
write.csv(cover, paste(taxa.dir, "/Trait coverage.csv", sep=""), row.names=FALSE)
