dat<-read.csv(file.choose(), header=TRUE, fileEncoding = "latin1")

for (i in 1:length(dat$Sample.ID)) {
  name<-strsplit(dat$Sample.ID[i], split="_")
  dat$Sampling.date[i]<-name[[1]][2]
}

which(dat$Sampling.date %in% NA)

write.csv(dat, "IRL_051_MZB_LO.csv", row.names=FALSE, fileEncoding = "latin1")
write.csv2(dat, "IRL_051_MZB_LO.csv", row.names=FALSE, fileEncoding = "latin1")
