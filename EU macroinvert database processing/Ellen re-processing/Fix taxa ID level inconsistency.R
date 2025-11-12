library(Hmisc)

taxa<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #harmonized taxa
trait<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #harmonized traits
tream<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #site list
dict<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1") #dictization sheet

options(warn=2)

datasets<-unique(tream$Dataset.ID)

###---Taxa
loop.num<-0.1
for (i in c(1:length(datasets))) {
  sub<-subset(taxa, taxa$Site_ID_Ellen %in% tream$site_id[which(tream$Dataset.ID==datasets[i])])
  
  ##--Chironomids and Oligochaetes first
  samps<-unique(sub$sample_id)
  sub2<-sub[1,]
  
  for (k in 1:length(samps)) {
    sub3<-sub[which(sub$sample_id==samps[k]),]
    
    #Rename all Oligochaetes and Chironomids
    for (j in 1:length(sub3$Taxon.name)) {
      num<-which(dict$final_name==sub3$Taxon.name[j])[1]
      if (dict$family[num] %in% c("AEOLOSOMATIDAE","DORYDRILIDAE","ENCHYTRAEIDAE","GLOSSOSCOLECIDAE","HAPLOTAXIDAE","LUMBRICIDAE","LUMBRICULIDAE","NAIDIDAE","OCTOCHAETIDAE","PROPAPPIDAE","SPARGANOPHILIDAE")) {
        sub3$Taxon.name[j]<-"OLIGOCHAETA GEN. SP."
        sub3$Taxon.ID[j]<-8736
      }
      if (dict$family[num] %in% "CHIRONOMIDAE") {
        sub3$Taxon.name[j]<-"CHIRONOMIDAE GEN. SP."
        sub3$Taxon.ID[j]<-4642
      }
    }
    
    #Resum abundances for renamed taxa
    if(any(as.numeric(table(sub3$Taxon.name))>1)) {
      sub4<-sub3[1,]
      names<-sort(unique(sub3$Taxon.name))
      loop.num1<-2
      for (j in 1:length(names)) {
        sub4<-rbind(sub4, sub3[1,])
        nums<-which(sub3$Taxon.name==names[j])
        sub4$Taxon.name[loop.num1]<-sub3$Taxon.name[nums[1]]
        sub4$Taxon.ID[loop.num1]<-sub3$Taxon.ID[nums[1]]
        sub4$Abundance[loop.num1]<-sum(sub3$Abundance[nums])
        loop.num1<-loop.num1+1
      }
      sub4<-sub4[-1,]
      sub2<-rbind(sub2, sub4)
    } else {
      sub4<-sub3
      sub2<-rbind(sub2, sub4)
    }
  }
  sub2<-sub2[-1,]
  
  ##--Convert long-form to matrix
  comm<-data.frame(site_id=NA, sgn_id=NA, sample_id=samps, year=NA, month=NA, day=NA)
  
  for (j in 1:length(comm$sample_id)) {
    num<-which(sub$sample_id==comm$sample_id[j])[1]
    comm$site_id[j]<-sub$Site_ID_Ellen[num]
    comm$sgn_id[j]<-tream$Unique.ID[which(tream$site_id==comm$site_id[j])]
    comm$year[j]<-sub$year[num]
    comm$month[j]<-sub$month[num]
    comm$day[j]<-sub$day[num]
  }
  
  comm[,sort(unique(sub2$Taxon.name))]<-0
  
  for (j in 1:length(comm$sample_id)) {
    sub3<-sub2[which(sub2$sample_id==comm$sample_id[j]),]
    sub3<-sub3[order(sub3$Taxon.name),]
    nums<-which(colnames(comm) %in% sub3$Taxon.name)
    comm[j,nums]<-sub3$Abundance
  }
  
  ##--Get IDs at family or lower versus higher-level IDs
  nums<-which(dict$final_name %in% colnames(comm))
  
  dict2<-dict[which(dict$final_name %in% colnames(comm)),]
  higher<-dict2[which(dict2$family %in% NA),]
  lower<-dict2[which(dict2$family %nin% NA),]
  
  ords<-sort(unique(higher$final_name))
  fams<-sort(unique(lower$family))
  
  ##--Check by family for mixed-level IDs
  comm2<-comm[,c(1:6)]
  
  for (j in 1:length(fams)) {
    names<-sort(unique(lower[which(lower$family %in% fams[j]),]$final_name))
    sub2<-subset(comm, select=which(colnames(comm) %in% names))
    
    #CHECK 1: Is there more than one taxon in the family?
    if (length(colnames(sub2))>1) {
      #CHECK 2: is there a family level ID?
      if (length(grep("DAE GEN. SP.", names))>0) { #if yes
        temp<-as.data.frame(sub2[,1])
        temp[,1]<-rowSums(sub2) #then sum all taxa in the family to the family level
        colnames(temp)[1]<-names[grep("DAE GEN. SP.", names)]
        comm2<-cbind(comm2, temp)
      } else { #if CHECK 2 finds no family level ID
        
        ##--Begin subfamily, genus, and subspecies checks here
        
        #CHECK 3: is there a subfamily level ID?
        if (length(grep("NAE GEN. SP.", names))>0) {
          subfam.names<-names[grep("NAE GEN. SP.", names)]
          colnums<-numeric()
          for (k in 1:length(subfam.names)) { #cycle through each subfamily
            subfam.name<-strsplit(subfam.names[k], split=" GEN. SP.")[[1]][1]
            lower2<-lower[which(lower$subfam==subfam.name),]
            subfam.nums<-which(colnames(sub2) %in% lower2$final_name)
            if (length(subfam.nums)>1) { #if more than 1 taxon in the subfamily
              temp<-as.data.frame(sub2[,1])
              colnums<-append(colnums, subfam.nums)
              temp[,1]<-rowSums(sub2[, subfam.nums]) #raise to subfamily level
              colnames(temp)[1]<-subfam.names[k]
              comm2<-cbind(comm2, temp) #add to the dataset
            }
            if (length(subfam.nums)==1) { #if only 1 taxa in the subfamily
              temp<-as.data.frame(sub2[,1])
              colnums<-append(colnums, subfam.nums)
              temp[,1]<-sub2[, subfam.nums] #keep the genus as is
              colnames(temp)[1]<-subfam.names[k] #set the genus name
              comm2<-cbind(comm2, temp) #add to the dataset
            }
          }
          sub2<-as.data.frame(sub2[,-colnums]) #remove finished taxa
          if (length(sub2)>0) {
            colnames(sub2)<-names[-colnums]
          }
        } #end of CHECK 3
        
        #CHECK 4: is there a genus level ID?
        if (length(sub2)>0) {
          names<-colnames(sub2)
          if (length(grep(" SP.", names))>0) {
            gen.names<-names[grep(" SP.", names)]
            colnums<-numeric()
            for (k in 1:length(gen.names)) { #cycle through each genus
              gen.name<-strsplit(gen.names[k], split=" SP.")[[1]][1]
              lower2<-lower[which(lower$genus==gen.name),]
              gen.nums<-which(colnames(sub2) %in% lower2$final_name)
              if (length(gen.nums)>1) { #if more than 1 taxa in the genus
                temp<-as.data.frame(sub2[,1])
                colnums<-append(colnums, gen.nums)
                temp[,1]<-rowSums(sub2[,gen.nums]) #raise to genus level
                colnames(temp)[1]<-gen.names[k]
                comm2<-cbind(comm2, temp) #add to the dataset
              }
              if (length(gen.nums)==1) { #if only 1 taxa in the genus
                temp<-as.data.frame(sub2[,1])
                colnums<-append(colnums, gen.nums)
                temp[,1]<-sub2[,gen.nums] #keep the genus as is
                colnames(temp)[1]<-gen.names[k] #set the genus name
                comm2<-cbind(comm2, temp) #add to the dataset
              }
            }
            sub2<-as.data.frame(sub2[,-colnums]) #remove finished taxa
            if (length(sub2)>0) {
              colnames(sub2)<-names[-colnums]
            }
          }
        } #end of CHECK 4
        
        #CHECK 5: is there a subspecies level ID?
        if (length(sub2)>0) {
          names<-colnames(sub2)
          if (length(grep("SSP.", names))>0) { #SUBSPECIES
            ssp.names<-names[grep("SSP.", names)]
            colnums<-numeric()
            for (k in 1:length(ssp.names)) { #cycle through the subspecies names
              ssp.name<-strsplit(ssp.names[k], split=" SSP.")[[1]][1]
              ssp.nums<-grep(ssp.name, colnames(sub2))
              if (length(ssp.nums)>1) { #if more than 1 taxon in the subspecies
                temp<-as.data.frame(sub2[,1])
                colnums<-append(colnums, ssp.nums)
                temp[,1]<-rowSums(sub2[, ssp.nums]) #raise to subspecies level
                colnames(temp)[1]<-ssp.names[k]
                comm2<-cbind(comm2, temp) #add to the dataset
              }
              if (length(ssp.nums)==1) { #if only 1 taxon in the subspecies
                temp<-as.data.frame(sub2[,1])
                colnums<-append(colnums, ssp.nums)
                temp[,1]<-sub2[, ssp.nums] #keep the subspecies as is
                colnames(temp)[1]<-ssp.names[k] #set the subspecies name
                comm2<-cbind(comm2, temp) #add to the dataset
              }
            }
            sub2<-as.data.frame(sub2[,-colnums]) #remove finished taxa
            if (length(sub2)>0) {
              colnames(sub2)<-names[-colnums]
            }
          }
        } #end of CHECK 5
        
        #CHECK 6: are any taxa left?
        if (length(sub2)>0) {
          comm2<-cbind(comm2, sub2) #add them
        }
        
        ##--End subfamily, genus, and subspecies checks here
        
      } #end of CHECK 2
    } else { #if CHECK 1 finds only 1 taxon
      comm2<-cbind(comm2, sub2) #keep data as is
    } #end of CHECK 1
  } #end of for loop
  
  ##--Add higher than family-level IDs
  sub2<-subset(comm, select=which(colnames(comm) %in% ords))
  if (length(sub2)>0) {
    comm2<-cbind(comm2, sub2)
  }
  
  ##---Order columns alphabetically
  comm3<-comm2[,c(1:6,order(colnames(comm2)[-c(1:6)])+6)]
  
  ##--Save the dataset
  write.csv(comm3, paste(datasets[i], "- finalized taxa.csv"), row.names=FALSE, fileEncoding = "latin1")
  
  if (i/length(datasets)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }
}


########################################################################################################
########################################################################################################

###---Traits
loop.num<-0.1
for (i in c(1:length(datasets))) {
  sub<-subset(trait, trait$Site_ID_Ellen %in% tream$site_id[which(tream$Dataset.ID==datasets[i])])
  
  ##--Chironomids and Oligochaetes first
  samps<-unique(sub$sample_id)
  sub2<-sub[1,]
  
  for (k in 1:length(samps)) {
    sub3<-sub[which(sub$sample_id==samps[k]),]
    
    #Rename
    for (j in 1:length(sub3$Taxon.name)) {
      num<-which(dict$trait_name==sub3$Taxon.name[j])[1]
      if (dict$family[num] %in% c("AEOLOSOMATIDAE","DORYDRILIDAE","ENCHYTRAEIDAE","GLOSSOSCOLECIDAE","HAPLOTAXIDAE","LUMBRICIDAE","LUMBRICULIDAE","NAIDIDAE","OCTOCHAETIDAE","PROPAPPIDAE","SPARGANOPHILIDAE")) {
        sub3$Taxon.name[j]<-"OLIGOCHAETA GEN. SP."
        sub3$Taxon.ID[j]<-8736
      }
      if (dict$family[num] %in% "CHIRONOMIDAE") {
        sub3$Taxon.name[j]<-"CHIRONOMIDAE GEN. SP."
        sub3$Taxon.ID[j]<-4642
      }
    }
    
    #Resum
    if(any(as.numeric(table(sub3$Taxon.name))>1)) {
      sub4<-sub3[1,]
      names<-sort(unique(sub3$Taxon.name))
      loop.num1<-2
      for (j in 1:length(names)) {
        sub4<-rbind(sub4, sub3[1,])
        nums<-which(sub3$Taxon.name==names[j])
        sub4$Taxon.name[loop.num1]<-sub3$Taxon.name[nums[1]]
        sub4$Taxon.ID[loop.num1]<-sub3$Taxon.ID[nums[1]]
        sub4$Abundance[loop.num1]<-sum(sub3$Abundance[nums])
        loop.num1<-loop.num1+1
      }
      sub4<-sub4[-1,]
      sub2<-rbind(sub2, sub4)
    } else {
      sub4<-sub3
      sub2<-rbind(sub2, sub4)
    }
  }
  sub2<-sub2[-1,]
  
  ##--Convert long-form to matrix
  comm<-data.frame(site_id=NA, sgn_id=NA, sample_id=samps, year=NA, month=NA, day=NA)
  
  for (j in 1:length(comm$sample_id)) {
    num<-which(sub$sample_id==comm$sample_id[j])[1]
    comm$site_id[j]<-sub$Site_ID_Ellen[num]
    comm$sgn_id[j]<-tream$Unique.ID[which(tream$site_id==comm$site_id[j])]
    comm$year[j]<-sub$year[num]
    comm$month[j]<-sub$month[num]
    comm$day[j]<-sub$day[num]
  }
  
  comm[,sort(unique(sub2$Taxon.name))]<-0
  
  for (j in 1:length(comm$sample_id)) {
    sub3<-sub2[which(sub2$sample_id==comm$sample_id[j]),]
    sub3<-sub3[order(sub3$Taxon.name),]
    nums<-which(colnames(comm) %in% sub3$Taxon.name)
    comm[j,nums]<-sub3$Abundance
  }
  
  ##--Get IDs at family or lower versus higher-level IDs
  nums<-which(dict$trait_name %in% colnames(comm))
  
  dict2<-dict[which(dict$trait_name %in% colnames(comm)),]
  higher<-dict2[which(dict2$family %in% NA),]
  lower<-dict2[which(dict2$family %nin% NA),]
  lower<-lower[which(lower$trait_name %nin% higher$trait_name),] #some taxa have a family name but use a higher trait value
  
  ords<-sort(unique(higher$trait_name))
  fams<-sort(unique(lower$family))
  
  ##--Check by family for mixed-level IDs
  comm2<-comm[,c(1:6)]

  for (j in 1:length(fams)) {
    names<-sort(unique(lower[which(lower$family %in% fams[j]),]$trait_name))
    sub2<-subset(comm, select=which(colnames(comm) %in% names))
    
    ad.num<-grep("AD\\.", colnames(sub2))
    lv.num<-grep("LV\\.", colnames(sub2))
    oth.num<-which(c(1:length(colnames(sub2))) %nin% c(ad.num, lv.num))
    
    ##--ADULTS
    if (length(ad.num)>0) {
      ad.sub<-as.data.frame(sub2[,ad.num])
      colnames(ad.sub)<-colnames(sub2)[ad.num]
      ad.names<-colnames(ad.sub)
      
      #CHECK 1: Is there more than one AD. taxon in the family?
      if (length(ad.names)>1) {
        #CHECK 2: is there a family level ID?
        if (length(grep("DAE GEN. SP. AD.", ad.names))>0) { #if yes
          temp<-as.data.frame(ad.sub[,1])
          temp[,1]<-rowSums(ad.sub) #then sum all taxa in the family to the family level
          colnames(temp)[1]<-ad.names[grep("DAE GEN. SP. AD.", ad.names)]
          comm2<-cbind(comm2, temp)
        } else { #if CHECK 2 finds no family level ID
          
          ##--Begin subfamily, genus, and subspecies checks here
          
          #CHECK 3: is there a subfamily level ID?
          if (length(grep("NAE GEN. SP. AD.", ad.names))>0) {
            subfam.names<-ad.names[grep("NAE GEN. SP. AD.", ad.names)]
            colnums<-numeric()
            for (k in 1:length(subfam.names)) { #cycle through each subfamily
              subfam.name<-strsplit(subfam.names[k], split=" GEN. SP. AD.")[[1]][1]
              lower2<-lower[which(lower$subfam==subfam.name),]
              subfam.nums<-which(colnames(ad.sub) %in% lower2$trait_name)
              if (length(subfam.nums)>1) { #if more than 1 taxon in the subfamily
                temp<-as.data.frame(ad.sub[,1])
                colnums<-append(colnums, subfam.nums)
                temp[,1]<-rowSums(ad.sub[, subfam.nums]) #raise to subfamily level
                colnames(temp)[1]<-subfam.names[k]
                comm2<-cbind(comm2, temp) #add to the dataset
              }
              if (length(subfam.nums)==1) { #if only 1 taxa in the subfamily
                temp<-as.data.frame(ad.sub[,1])
                colnums<-append(colnums, subfam.nums)
                temp[,1]<-ad.sub[, subfam.nums] #keep the genus as is
                colnames(temp)[1]<-subfam.names[k] #set the genus name
                comm2<-cbind(comm2, temp) #add to the dataset
              }
            }
            ad.sub<-as.data.frame(ad.sub[,-colnums]) #remove finished taxa
            if (length(ad.sub)>0) {
              colnames(ad.sub)<-ad.names[-colnums]
            }
          } #end of CHECK 3
          
          #CHECK 4: is there a genus level ID?
          if (length(ad.sub)>0) {
            ad.names<-colnames(ad.sub)
            if (length(grep(" SP. AD.", ad.names))>0) {
              gen.names<-ad.names[grep(" SP. AD.", ad.names)]
              colnums<-numeric()
              for (k in 1:length(gen.names)) { #cycle through each genus
                gen.name<-strsplit(gen.names[k], split=" SP. AD.")[[1]][1]
                lower2<-lower[which(lower$genus==gen.name),]
                gen.nums<-which(colnames(ad.sub) %in% lower2$trait_name)
                if (length(gen.nums)>1) { #if more than 1 taxa in the genus
                  temp<-as.data.frame(ad.sub[,1])
                  colnums<-append(colnums, gen.nums)
                  temp[,1]<-rowSums(ad.sub[,gen.nums]) #raise to genus level
                  colnames(temp)[1]<-gen.names[k]
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
                if (length(gen.nums)==1) { #if only 1 taxa in the genus
                  temp<-as.data.frame(ad.sub[,1])
                  colnums<-append(colnums, gen.nums)
                  temp[,1]<-ad.sub[,gen.nums] #keep the genus as is
                  colnames(temp)[1]<-gen.names[k] #set the genus name
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
              }
              ad.sub<-as.data.frame(ad.sub[,-colnums]) #remove finished taxa
              if (length(ad.sub)>0) {
                colnames(ad.sub)<-ad.names[-colnums]
              }
            }
          } #end of CHECK 4
          
          #CHECK 5: is there a subspecies level ID?
          if (length(ad.sub)>0) {
            ad.names<-colnames(ad.sub)
            if (length(grep("SSP. AD.", ad.names))>0) { #SUBSPECIES
              ssp.names<-ad.names[grep("SSP. AD.", ad.names)]
              colnums<-numeric()
              for (k in 1:length(ssp.names)) { #cycle through the subspecies names
                ssp.name<-strsplit(ssp.names[k], split=" SSP. AD.")[[1]][1]
                ssp.nums<-grep(ssp.name, colnames(ad.sub))
                if (length(ssp.nums)>1) { #if more than 1 taxon in the subspecies
                  temp<-as.data.frame(ad.sub[,1])
                  colnums<-append(colnums, ssp.nums)
                  temp[,1]<-rowSums(ad.sub[, ssp.nums]) #raise to subspecies level
                  colnames(temp)[1]<-ssp.names[k]
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
                if (length(ssp.nums)==1) { #if only 1 taxon in the subspecies
                  temp<-as.data.frame(ad.sub[,1])
                  colnums<-append(colnums, ssp.nums)
                  temp[,1]<-ad.sub[, ssp.nums] #keep the subspecies as is
                  colnames(temp)[1]<-ssp.names[k] #set the subspecies name
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
              }
              ad.sub<-as.data.frame(ad.sub[,-colnums]) #remove finished taxa
              if (length(ad.sub)>0) {
                colnames(ad.sub)<-ad.names[-colnums]
              }
            }
          } #end of CHECK 5
          
          #CHECK 6: are any taxa left?
          if (length(ad.sub)>0) {
            comm2<-cbind(comm2, ad.sub) #add them
          }
          
          ##--End subfamily, genus, and subspecies checks here
          
        } #end of CHECK 2
      } else { #if CHECK 1 finds only 1 taxon
        comm2<-cbind(comm2, ad.sub) #keep data as is
      } #end of CHECK 1
    }
    
    ##--LARVAE
    if (length(lv.num)>0) {
      lv.sub<-as.data.frame(sub2[,lv.num])
      colnames(lv.sub)<-colnames(sub2)[lv.num]
      lv.names<-colnames(lv.sub)
      
      #CHECK 1: Is there more than one LV. taxon in the family?
      if (length(lv.names)>1) {
        #CHECK 2: is there a family level ID?
        if (length(grep("DAE GEN. SP. LV.", lv.names))>0) { #if yes
          temp<-as.data.frame(lv.sub[,1])
          temp[,1]<-rowSums(lv.sub) #then sum all taxa in the family to the family level
          colnames(temp)[1]<-lv.names[grep("DAE GEN. SP. LV.", lv.names)]
          comm2<-cbind(comm2, temp)
        } else { #if CHECK 2 finds no family level ID
          
          ##--Begin subfamily, genus, and subspecies checks here
          
          #CHECK 3: is there a subfamily level ID?
          if (length(grep("NAE GEN. SP. LV.", lv.names))>0) {
            subfam.names<-lv.names[grep("NAE GEN. SP. LV.", lv.names)]
            colnums<-numeric()
            for (k in 1:length(subfam.names)) { #cycle through each subfamily
              subfam.name<-strsplit(subfam.names[k], split=" GEN. SP. LV.")[[1]][1]
              lower2<-lower[which(lower$subfam==subfam.name),]
              subfam.nums<-which(colnames(lv.sub) %in% lower2$trait_name)
              if (length(subfam.nums)>1) { #if more than 1 taxon in the subfamily
                temp<-as.data.frame(lv.sub[,1])
                colnums<-append(colnums, subfam.nums)
                temp[,1]<-rowSums(lv.sub[, subfam.nums]) #raise to subfamily level
                colnames(temp)[1]<-subfam.names[k]
                comm2<-cbind(comm2, temp) #add to the dataset
              }
              if (length(subfam.nums)==1) { #if only 1 taxa in the subfamily
                temp<-as.data.frame(lv.sub[,1])
                colnums<-append(colnums, subfam.nums)
                temp[,1]<-lv.sub[, subfam.nums] #keep the genus as is
                colnames(temp)[1]<-subfam.names[k] #set the genus name
                comm2<-cbind(comm2, temp) #add to the dataset
              }
            }
            lv.sub<-as.data.frame(lv.sub[,-colnums]) #remove finished taxa
            if (length(lv.sub)>0) {
              colnames(lv.sub)<-lv.names[-colnums]
            }
          } #end of CHECK 3
          
          #CHECK 4: is there a genus level ID?
          if (length(lv.sub)>0) {
            lv.names<-colnames(lv.sub)
            if (length(grep(" SP. LV.", lv.names))>0) {
              gen.names<-lv.names[grep(" SP. LV.", lv.names)]
              colnums<-numeric()
              for (k in 1:length(gen.names)) { #cycle through each genus
                gen.name<-strsplit(gen.names[k], split=" SP. LV.")[[1]][1]
                lower2<-lower[which(lower$genus==gen.name),]
                gen.nums<-which(colnames(lv.sub) %in% lower2$trait_name)
                if (length(gen.nums)>1) { #if more than 1 taxa in the genus
                  temp<-as.data.frame(lv.sub[,1])
                  colnums<-append(colnums, gen.nums)
                  temp[,1]<-rowSums(lv.sub[,gen.nums]) #raise to genus level
                  colnames(temp)[1]<-gen.names[k]
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
                if (length(gen.nums)==1) { #if only 1 taxa in the genus
                  temp<-as.data.frame(lv.sub[,1])
                  colnums<-append(colnums, gen.nums)
                  temp[,1]<-lv.sub[,gen.nums] #keep the genus as is
                  colnames(temp)[1]<-gen.names[k] #set the genus name
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
              }
              lv.sub<-as.data.frame(lv.sub[,-colnums]) #remove finished taxa
              if (length(lv.sub)>0) {
                colnames(lv.sub)<-lv.names[-colnums]
              }
            }
          } #end of CHECK 4
          
          #CHECK 5: is there a subspecies level ID?
          if (length(lv.sub)>0) {
            lv.names<-colnames(lv.sub)
            if (length(grep("SSP. LV.", lv.names))>0) { #SUBSPECIES
              ssp.names<-lv.names[grep("SSP. LV.", lv.names)]
              colnums<-numeric()
              for (k in 1:length(ssp.names)) { #cycle through the subspecies names
                ssp.name<-strsplit(ssp.names[k], split=" SSP. LV.")[[1]][1]
                ssp.nums<-grep(ssp.name, colnames(lv.sub))
                if (length(ssp.nums)>1) { #if more than 1 taxon in the subspecies
                  temp<-as.data.frame(lv.sub[,1])
                  colnums<-append(colnums, ssp.nums)
                  temp[,1]<-rowSums(lv.sub[, ssp.nums]) #raise to subspecies level
                  colnames(temp)[1]<-ssp.names[k]
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
                if (length(ssp.nums)==1) { #if only 1 taxon in the subspecies
                  temp<-as.data.frame(lv.sub[,1])
                  colnums<-append(colnums, ssp.nums)
                  temp[,1]<-lv.sub[, ssp.nums] #keep the subspecies as is
                  colnames(temp)[1]<-ssp.names[k] #set the subspecies name
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
              }
              lv.sub<-as.data.frame(lv.sub[,-colnums]) #remove finished taxa
              if (length(lv.sub)>0) {
                colnames(lv.sub)<-lv.names[-colnums]
              }
            }
          } #end of CHECK 5
          
          #CHECK 6: are any taxa left?
          if (length(lv.sub)>0) {
            comm2<-cbind(comm2, lv.sub) #add them
          }
          
          ##--End subfamily, genus, and subspecies checks here
          
        } #end of CHECK 2
      } else { #if CHECK 1 finds only 1 taxon
        comm2<-cbind(comm2, lv.sub) #keep data as is
      } #end of CHECK 1
    }
    
    
    ##--OTHER
    if (length(oth.num)>0) {
      oth.sub<-as.data.frame(sub2[,oth.num])
      colnames(oth.sub)<-colnames(sub2)[oth.num]
      oth.names<-colnames(oth.sub)
      
      #CHECK 1: Is there more than one taxon in the family?
      if (length(oth.names)>1) {
        #CHECK 2: is there a family level ID?
        if (length(grep("DAE GEN. SP.", oth.names))>0) { #if yes
          temp<-as.data.frame(oth.sub[,1])
          temp[,1]<-rowSums(oth.sub) #then sum all taxa in the family to the family level
          colnames(temp)[1]<-oth.names[grep("DAE GEN. SP.", oth.names)]
          comm2<-cbind(comm2, temp)
        } else { #if CHECK 2 finds no family level ID
          
          ##--Begin subfamily, genus, and subspecies checks here
          
          #CHECK 3: is there a subfamily level ID?
          if (length(grep("NAE GEN. SP.", oth.names))>0) {
            subfam.names<-oth.names[grep("NAE GEN. SP.", oth.names)]
            colnums<-numeric()
            for (k in 1:length(subfam.names)) { #cycle through each subfamily
              subfam.name<-strsplit(subfam.names[k], split=" GEN. SP.")[[1]][1]
              lower2<-lower[which(lower$subfam==subfam.name),]
              subfam.nums<-which(colnames(oth.sub) %in% lower2$trait_name)
              if (length(subfam.nums)>1) { #if more than 1 taxon in the subfamily
                temp<-as.data.frame(oth.sub[,1])
                colnums<-append(colnums, subfam.nums)
                temp[,1]<-rowSums(oth.sub[, subfam.nums]) #raise to subfamily level
                colnames(temp)[1]<-subfam.names[k]
                comm2<-cbind(comm2, temp) #add to the dataset
              }
              if (length(subfam.nums)==1) { #if only 1 taxa in the subfamily
                temp<-as.data.frame(oth.sub[,1])
                colnums<-append(colnums, subfam.nums)
                temp[,1]<-oth.sub[, subfam.nums] #keep the genus as is
                colnames(temp)[1]<-subfam.names[k] #set the genus name
                comm2<-cbind(comm2, temp) #add to the dataset
              }
            }
            oth.sub<-as.data.frame(oth.sub[,-colnums]) #remove finished taxa
            if (length(oth.sub)>0) {
              colnames(oth.sub)<-oth.names[-colnums]
            }
          } #end of CHECK 3
          
          #CHECK 4: is there a genus level ID?
          if (length(oth.sub)>0) {
            oth.names<-colnames(oth.sub)
            if (length(grep(" SP.", oth.names))>0) {
              gen.names<-oth.names[grep(" SP.", oth.names)]
              colnums<-numeric()
              for (k in 1:length(gen.names)) { #cycle through each genus
                gen.name<-strsplit(gen.names[k], split=" SP.")[[1]][1]
                lower2<-lower[which(lower$genus==gen.name),]
                gen.nums<-which(colnames(oth.sub) %in% lower2$trait_name)
                if (length(gen.nums)>1) { #if more than 1 taxa in the genus
                  temp<-as.data.frame(oth.sub[,1])
                  colnums<-append(colnums, gen.nums)
                  temp[,1]<-rowSums(oth.sub[,gen.nums]) #raise to genus level
                  colnames(temp)[1]<-gen.names[k]
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
                if (length(gen.nums)==1) { #if only 1 taxa in the genus
                  temp<-as.data.frame(oth.sub[,1])
                  colnums<-append(colnums, gen.nums)
                  temp[,1]<-oth.sub[,gen.nums] #keep the genus as is
                  colnames(temp)[1]<-gen.names[k] #set the genus name
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
              }
              oth.sub<-as.data.frame(oth.sub[,-colnums]) #remove finished taxa
              if (length(oth.sub)>0) {
                colnames(oth.sub)<-oth.names[-colnums]
              }
            }
          } #end of CHECK 4
          
          #CHECK 5: is there a subspecies level ID?
          if (length(oth.sub)>0) {
            oth.names<-colnames(oth.sub)
            if (length(grep("SSP.", oth.names))>0) { #SUBSPECIES
              ssp.names<-oth.names[grep("SSP.", oth.names)]
              colnums<-numeric()
              for (k in 1:length(ssp.names)) { #cycle through the subspecies names
                ssp.name<-strsplit(ssp.names[k], split=" SSP.")[[1]][1]
                ssp.nums<-grep(ssp.name, colnames(oth.sub))
                if (length(ssp.nums)>1) { #if more than 1 taxon in the subspecies
                  temp<-as.data.frame(oth.sub[,1])
                  colnums<-append(colnums, ssp.nums)
                  temp[,1]<-rowSums(oth.sub[, ssp.nums]) #raise to subspecies level
                  colnames(temp)[1]<-ssp.names[k]
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
                if (length(ssp.nums)==1) { #if only 1 taxon in the subspecies
                  temp<-as.data.frame(oth.sub[,1])
                  colnums<-append(colnums, ssp.nums)
                  temp[,1]<-oth.sub[, ssp.nums] #keep the subspecies as is
                  colnames(temp)[1]<-ssp.names[k] #set the subspecies name
                  comm2<-cbind(comm2, temp) #add to the dataset
                }
              }
              oth.sub<-as.data.frame(oth.sub[,-colnums]) #remove finished taxa
              if (length(oth.sub)>0) {
                colnames(oth.sub)<-oth.names[-colnums]
              }
            }
          } #end of CHECK 5
          
          #CHECK 6: are any taxa left?
          if (length(oth.sub)>0) {
            comm2<-cbind(comm2, oth.sub) #add them
          }
          
          ##--End subfamily, genus, and subspecies checks here
          
        } #end of CHECK 2
      } else { #if CHECK 1 finds only 1 taxon
        comm2<-cbind(comm2, oth.sub) #keep data as is
      } #end of CHECK 1
    }
  
  } #end of for loop
  
  ##--Add higher than family-level IDs
  sub2<-subset(comm, select=which(colnames(comm) %in% ords))
  if (length(sub2)>0) {
    comm2<-cbind(comm2, sub2)
  }
  
  ##---Order columns alphabetically
  comm3<-comm2[,c(1:6,order(colnames(comm2)[-c(1:6)])+6)]
  
  ##--Save the dataset
  write.csv(comm3, paste(datasets[i], "- finalized trait.csv"), row.names=FALSE, fileEncoding = "latin1")
  
  if (i/length(datasets)>=loop.num) { #report progress
    cat(paste(loop.num*100,"% ", sep=""))
    loop.num<-loop.num+0.1
  }
}
