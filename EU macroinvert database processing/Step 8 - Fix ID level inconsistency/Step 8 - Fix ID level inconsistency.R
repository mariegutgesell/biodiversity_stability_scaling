#this code fixes inconsistent levels of taxon identification in each dataset
#it cycles through each dataset family-by-family and checks whether multiple ID levels occur (e.g., a mixture of family, genus, and species level IDs)
#if so, it raises the IDs to the highest level
#NOTE: the code does not check IDs higher than family level

###---Load libraries
library(Hmisc)

###---Set the criteria for site inclusion
eco.type<-"LO" #lotic, _LE for lentic
samp.num<-7 #must have at least this many sampling years
dur.num<-10 #must have at least this much total time series duration

###---Set directories for saving files
taxa.mat<-paste0(getwd(), "/Final matrix, Tax=MZB, Eco=", eco.type,", Num=", samp.num, ", Dur=", dur.num) #use if you followed Step 6, path 1
taxa.long<-paste0(getwd(), "/Final long, Tax=MZB, Eco=", eco.type,", Num=", samp.num, ", Dur=", dur.num)
taxa.mat<-paste0(getwd(), "/Final for EQRs matrix, Tax=MZB, Eco=", eco.type,", Num=", samp.num, ", Dur=", dur.num) #use if you followed Step 6, path 2
taxa.long<-paste0(getwd(), "/Final for EQRs long, Tax=MZB, Eco=", eco.type,", Num=", samp.num, ", Dur=", dur.num)
if(!dir.exists(taxa.mat)) dir.create(taxa.mat)
if(!dir.exists(taxa.long)) dir.create(taxa.long)

###---Load the harmonization sheet (used to group taxa by family, genus, etc)
#https://docs.google.com/spreadsheets/d/1pcpr-NXxbCcQL4a3V8o0McSnvr9t1dId/edit?usp=sharing&ouid=109310749942317730589&rtpof=true&sd=true
harmon<-read.csv(file.choose(), header=TRUE, encoding="latin1")

###---Load all taxa files, set working directory to the "Harmonized taxa" directory from Step 7
(taxa.names<-list.files(pattern="*.csv"))

taxa.files<-list() #empty list
for (i in 1:length(taxa.names)) { #for however many file names
  taxa.files[[i]]<-read.csv(taxa.names[i], header=TRUE, fileEncoding="latin1") #load the data into the ith list position 
}

###################### START OF THE DATASET LOOP ###########################################

options(warn=2) #will end the loop if any warnings are encountered
loop.num<-0.1 #for tracking progress through the for loop

taxa.names
data.nums<-1 #use to run a specific dataset
data.nums<-c(1:length(taxa.names)) #use to run datasets starting at a specified number until the end

for (i in data.nums) {
  dat<-taxa.files[[i]]
  
  ##--Chironomids and Oligochaetes first, they are the most inconsistently ID'd taxonomic groups across datasets
  #also Oligochaetes are a higher than family-level ID and the code stops at the family level
  samps<-unique(dat$Samp.ID)
  dat2<-dat[1,] #create a blank dataset for saving finalized taxon names
  
  for (j in 1:length(samps)) {
    sub<-dat[which(dat$Samp.ID==samps[j]),] #check each sample
    
    #-Rename all Oligochaetes and Chironomids
    for (k in 1:length(sub$Taxon.name)) {
      num<-which(harmon$final_name==sub$Taxon.name[k])[1]
      #if any Chironomids or Oligochaetes are found
      if (harmon$family[num] %in% c("AEOLOSOMATIDAE","DORYDRILIDAE","ENCHYTRAEIDAE","GLOSSOSCOLECIDAE","HAPLOTAXIDAE","LUMBRICIDAE","LUMBRICULIDAE","NAIDIDAE","OCTOCHAETIDAE","PROPAPPIDAE","SPARGANOPHILIDAE")) {
        sub$Taxon.name[k]<-"OLIGOCHAETA GEN. SP." #change all Oligochaete taxa to this
      }
      if (harmon$family[num] %in% "CHIRONOMIDAE") {
        sub$Taxon.name[k]<-"CHIRONOMIDAE GEN. SP." #change all Chironomid taxa to this
      }
    }
    
    #-Resum abundances for renamed taxa
    if(any(as.numeric(table(sub$Taxon.name))>1)) {
      sub2<-sub[1,]
      names<-sort(unique(sub$Taxon.name))
      loop.num1<-2
      for (k in 1:length(names)) {
        sub2<-rbind(sub2, sub[1,])
        nums<-which(sub$Taxon.name==names[k])
        sub2$Taxon.name[loop.num1]<-sub$Taxon.name[nums[1]]
        sub2$Abundance[loop.num1]<-sum(sub$Abundance[nums])
        loop.num1<-loop.num1+1
      }
      sub2<-sub2[-1,]
      dat2<-rbind(dat2, sub2)
    } else {
      sub2<-sub
      dat2<-rbind(dat2, sub2)
    }
  }
  dat2<-dat2[-1,]
  
  ##--Convert long-form to matrix
  comm<-data.frame(Unique.ID=NA, Samp.ID=unique(dat2$Samp.ID), Sampling.date=NA, year=NA, month=NA, day=NA)
  
  for (j in 1:length(comm$Samp.ID)) {
    num<-which(dat2$Samp.ID==comm$Samp.ID[j])[1]
    comm$Unique.ID[j]<-dat2$Unique.ID[num]
    comm$Sampling.date[j]<-dat2$Sampling.date[num]
    comm$year[j]<-dat2$year[num]
    comm$month[j]<-dat2$month[num]
    comm$day[j]<-dat2$day[num]
  }
  
  comm[,sort(unique(dat2$Taxon.name))]<-0
  
  for (j in 1:length(comm$Samp.ID)) {
    sub<-dat2[which(dat2$Samp.ID==comm$Samp.ID[j]),]
    sub<-sub[order(sub$Taxon.name),]
    nums<-which(colnames(comm) %in% sub$Taxon.name)
    comm[j,nums]<-sub$Abundance
  }
  
  ##--Get IDs at family or lower versus higher-level IDs
  nums<-which(harmon$final_name %in% colnames(comm))
  
  harmon2<-harmon[which(harmon$final_name %in% colnames(comm)),]
  higher<-harmon2[which(harmon2$family %in% NA),]
  lower<-harmon2[which(harmon2$family %nin% NA),]
  
  ords<-sort(unique(higher$final_name))
  fams<-sort(unique(lower$family))
  
  ##--Check by family for mixed-level IDs
  #-First make a blank community dataset that we will build column by column
  comm2<-comm[,c(1:6)] #start with only the columns with the site/sample information
  
###################### START OF BY FAMILY FOR LOOP ###########################################
  
  #-For each family
  for (j in 1:length(fams)) {
    names<-sort(unique(lower[which(lower$family %in% fams[j]),]$final_name)) #get the names of all taxa within the family
    sub<-subset(comm, select=which(colnames(comm) %in% names)) #select the columns from the community dataset that match the above names
    
###################### START OF CHECK 1 ###########################################
    
    #CHECK 1: Is there more than one taxon in the family?
    if (length(colnames(sub))>1) { #if yes
      #CHECK 2: is there a family level ID?
      if (length(grep("DAE GEN. SP.", names))>0) { #if yes
        temp<-as.data.frame(sub[,1]) #make a temporary column to hold the abundance data
        temp[,1]<-rowSums(sub) #then sum all taxa in the family to the family level in the temporary column
        colnames(temp)[1]<-names[grep("DAE GEN. SP.", names)] #name the temporary column after the family
        comm2<-cbind(comm2, temp) #bind the temporary column to the blank community dataset
      } else { #if CHECK 2 finds no family level ID
        
        ##--Begin subfamily, genus, and subspecies checks here
        
        #CHECK 3: is there a subfamily level ID?
        if (length(grep("NAE GEN. SP.", names))>0) {
          subfam.names<-names[grep("NAE GEN. SP.", names)]
          colnums<-numeric()
          for (k in 1:length(subfam.names)) { #cycle through each subfamily
            subfam.name<-strsplit(subfam.names[k], split=" GEN. SP.")[[1]][1]
            lower2<-lower[which(lower$subfam==subfam.name),]
            subfam.nums<-which(colnames(sub) %in% lower2$final_name)
            if (length(subfam.nums)>1) { #if more than 1 taxon in the subfamily
              temp<-as.data.frame(sub[,1])
              colnums<-append(colnums, subfam.nums) #save the numbers of the columns being adjusted
              temp[,1]<-rowSums(sub[, subfam.nums]) #raise to subfamily level
              colnames(temp)[1]<-subfam.names[k]
              comm2<-cbind(comm2, temp) #add to the dataset
            }
            if (length(subfam.nums)==1) { #if only 1 taxa in the subfamily
              temp<-as.data.frame(sub[,1])
              colnums<-append(colnums, subfam.nums)
              temp[,1]<-sub[, subfam.nums] #keep the genus as is
              colnames(temp)[1]<-subfam.names[k] #set the genus name
              comm2<-cbind(comm2, temp) #add to the dataset
            }
          }
          sub<-as.data.frame(sub[,-colnums]) #remove the columns belonging to checked taxa
          if (length(sub)>0) {
            colnames(sub)<-names[-colnums] #fix column names if any taxa are left
          }
        } #end of CHECK 3
        
        #CHECK 4: is there a genus level ID?
        if (length(sub)>0) { #if any taxa are left to check
          names<-colnames(sub)
          if (length(grep(" SP.", names))>0) { #if there is a genus level ID
            gen.names<-names[grep(" SP.", names)]
            colnums<-numeric()
            for (k in 1:length(gen.names)) { #cycle through each genus
              gen.name<-strsplit(gen.names[k], split=" SP.")[[1]][1]
              lower2<-lower[which(lower$genus==gen.name),]
              gen.nums<-which(colnames(sub) %in% lower2$final_name)
              if (length(gen.nums)>1) { #if more than 1 taxa in the genus
                temp<-as.data.frame(sub[,1])
                colnums<-append(colnums, gen.nums)
                temp[,1]<-rowSums(sub[,gen.nums]) #raise to genus level
                colnames(temp)[1]<-gen.names[k]
                comm2<-cbind(comm2, temp) #add to the dataset
              }
              if (length(gen.nums)==1) { #if only 1 taxa in the genus
                temp<-as.data.frame(sub[,1])
                colnums<-append(colnums, gen.nums)
                temp[,1]<-sub[,gen.nums] #keep the genus as is
                colnames(temp)[1]<-gen.names[k] #set the genus name
                comm2<-cbind(comm2, temp) #add to the dataset
              }
            }
            sub<-as.data.frame(sub[,-colnums]) #remove finished taxa
            if (length(sub)>0) {
              colnames(sub)<-names[-colnums] #fix column names if any taxa are left
            }
          }
        } #end of CHECK 4
        
        #CHECK 5: is there a subspecies level ID?
        if (length(sub)>0) { #if any taxa are still left to check
          names<-colnames(sub)
          if (length(grep("SSP.", names))>0) { #if there is a subspecies level ID
            ssp.names<-names[grep("SSP.", names)]
            colnums<-numeric()
            for (k in 1:length(ssp.names)) { #cycle through the subspecies names
              ssp.name<-strsplit(ssp.names[k], split=" SSP.")[[1]][1]
              ssp.nums<-grep(ssp.name, colnames(sub))
              if (length(ssp.nums)>1) { #if more than 1 taxon in the subspecies
                temp<-as.data.frame(sub[,1])
                colnums<-append(colnums, ssp.nums)
                temp[,1]<-rowSums(sub[, ssp.nums]) #raise to subspecies level
                colnames(temp)[1]<-ssp.names[k]
                comm2<-cbind(comm2, temp) #add to the dataset
              }
              if (length(ssp.nums)==1) { #if only 1 taxon in the subspecies
                temp<-as.data.frame(sub[,1])
                colnums<-append(colnums, ssp.nums)
                temp[,1]<-sub[, ssp.nums] #keep the subspecies as is
                colnames(temp)[1]<-ssp.names[k] #set the subspecies name
                comm2<-cbind(comm2, temp) #add to the dataset
              }
            }
            sub<-as.data.frame(sub[,-colnums]) #remove finished taxa
            if (length(sub)>0) {
              colnames(sub)<-names[-colnums] #fix column names if any taxa are left
            }
          }
        } #end of CHECK 5
        
        #CHECK 6: are any taxa left?
        if (length(sub)>0) { #if yes
          comm2<-cbind(comm2, sub) #add them
        }
        
        ##--End subfamily, genus, and subspecies checks here
        
      } #end of CHECK 2
    } else { #if CHECK 1 finds only 1 taxon
      comm2<-cbind(comm2, sub) #keep data as is
    } #end of CHECK 1

###################### END OF CHECK 1 ###########################################
    
  } #end of for loop
  
###################### END OF BY FAMILY FOR LOOP ###########################################
  
  ##--Add higher than family-level IDs
  sub<-subset(comm, select=which(colnames(comm) %in% ords))
  if (length(sub)>0) {
    comm2<-cbind(comm2, sub)
  }
  
  ##--Order columns alphabetically
  names<-colnames(comm2)[-c(1:6)] #get only the taxon names
  comm3<-comm2[,c(1:6, order(names)+6)] #sort columns by the ordered names
  
  ##--Make a long-form version of the community matrix
  long<-dat2[1,]
  
  samps<-comm3$Samp.ID
  for (j in 1:length(samps)) {
    sub<-comm3[which(comm3$Samp.ID==samps[j]),]
    nums<-which(sub[,c(7:length(colnames(sub)))]>0)+6
    names<-colnames(sub)[nums]
    
    sub2<-long[rep(1, times=length(names)),]
    sub2[,c(1:6)]<-sub[1,c(1:6)]
    sub2$Taxon.name<-names
    sub2$Abundance<-as.numeric(sub[,nums])
    
    long<-rbind(long, sub2)
  }
  long<-long[-1,]
  
  ##--Save the datasets
  name<-taxa.names[i]
  name<-gsub(" - harmonized taxa.csv", "", name)
  write.csv(x=comm3, file=paste(taxa.mat, "/", name, " - final matrix.csv", sep=""), row.names=FALSE, fileEncoding="latin1")
  write.csv(x=long, file=paste(taxa.long, "/", name, " - final long.csv", sep=""), row.names=FALSE, fileEncoding="latin1")
  
  ##--Report progress
  if (i/length(taxa.names)>=loop.num) {
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }
  
} #end of dataset for loop

###################### END OF THE DATASET LOOP ###########################################