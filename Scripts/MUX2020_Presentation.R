#Magic Mux 2020 Data Presentation
#Authors: Bethany Bookout, Nick Hammond

#Outline

#1. 1.6 scan data
#a. Load in data
#b. Put all files tog
#2. 1.6 scan maintenance log?
#3. Mux 2020 data

#### 1.6 m SCAN load ####

#packages need
library(lubridate)

#set working directory
setwd('./MagicData/FP_2020')

#create list with files from 1.6m SCAN ("^2020" retrieves all files with name that begins with 2020)
fp.files<-list.files(path=".", pattern = ".fp")

### Read in first file
obs<-read.table(file=fp.files[1],skip=1,header=TRUE, row.names = NULL,
                fill= TRUE, sep = "\t") #read in first file
obs$Date.Time=ymd_hms(obs$Date.Time, tz = "Etc/GMT+4")

#reads in all files within folder in Github
for(i in 2:length(fp.files)){ 
  temp<-read.table(file=fp.files[i],skip=1,header=TRUE, row.names = NULL,
                   sep = "\t", fill = TRUE)
  temp$Date.Time=ymd_hms(temp$Date.Time, tz = "Etc/GMT+4")
  obs<-rbind(obs,temp)
}

#plot some data from the combined file to check that all the data is there
plot(obs$Date.Time,obs$X702.50)

#check for data gaps
gaps = matrix(NA,nrow=1,ncol=2)
for(i in 2:length(obs$Date.Time)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(difftime(obs$Date.Time[i],obs$Date.Time[i-1],units="hours") > 4.5){
    gaps[i,1]=obs$Date.Time[i-1]
    gaps[i,2]=obs$Date.Time[i]
  }
}





#MUX Load
muxfiles<-list.files(path=".", pattern = ".FP")

library(lubridate)

mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5))),"nm"), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime)

for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(muxfiles[i])>4000){
  temp<-read.table(file=muxfiles[i],skip=2,header=FALSE, row.names = NULL, sep = "\t")
  names(temp) <- mux_colnames
  temp$DateTime=ymd_hms(temp$DateTime)
  obs2<-rbind(obs2,temp)
  #print(i)
}
}

setwd("..")
log_files=list.files(path = ".", pattern = glob2rx("20*MUX.TXT"))
logs<-read.table(file=log_files[1],header=F, row.names = NULL, sep = ",", fill = TRUE) #read in first file
logs$Date.Time=ymd_hms(obs$Date.Time)

for(i in 2:length(log_files)){ #reads in all files within folder in Github
  temp<-read.table(file=log_files[1],header=F, row.names = NULL, sep = ",")
  temp$Date.Time=ymd_hms(temp$Date.Time)
  logs<-rbind(logs,temp)
  #print(i)
}


#Spectra plot examples and code dump
install.packages('photobiologyWavebands')
library(spectrolab)
spec  = as.spectra(spec_matrix_example, name_idx = 1)
plot(spec, lwd = 1.2)

matrix_obs=as.matrix(obs[,c(3:200)])

library(photobiologyWavebands)
library(ggplot2)
ggplot(sun.spct) + geom_line() + stat_peaks(span = NULL)
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_peaks(span = 21, geom = "point", colour = "red") +
  stat_peaks(span = 51, geom = "text", colour = "red", vjust = -0.3,
             label.fmt = "%3.0f nm")
ggplot(polyester.spct, range = UV()) + geom_line()
plot(sun.spct)
plot(polyester.spct, UV_bands(), range = UV(),
     annotations = c("=", "segments", "labels"))



