#Magic Mux 2020 Data Presentation
#Authors: Bethany Bookout, Nick Hammond

#Outline

#1. 1.6 scan data
#a. Load in data
#b. Put all files tog
#2. 1.6 scan maintenance log?
#3. Mux 2020 data

#scan load























#MUX Load
muxfiles<-list.files(path=".", pattern = ".FP")

library(lubridate)

mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5))),"nm"), "Valve","Measurement time")
obs <- as.data.frame(matrix(,0,length(mux_colnames)))
names(obs) <- mux_colnames
obs$DateTime=ymd_hms(obs$DateTime)

for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(muxfiles[i])>4000){
  temp<-read.table(file=muxfiles[i],skip=2,header=FALSE, row.names = NULL, sep = "\t")
  names(temp) <- mux_colnames
  temp$DateTime=ymd_hms(temp$DateTime)
  obs<-rbind(obs,temp)
  #print(i)
}
}

log_files=list.files(path = ".", pattern = "*LOG.TXT")
logs<-read.table(file=log_files[1],header=F, row.names = NULL, sep = ",") #read in first file
logs$Date.Time=ymd_hms(obs$Date.Time, tz = "Etc/GMT+4")

