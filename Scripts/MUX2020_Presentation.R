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
setwd('C:/Users/hammo/OneDrive/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/FP_2020')

#create list with files from 1.6m SCAN ("^2020" retrieves all files with name that begins with 2020)
magicfiles<-list.files(path="C:/Users/hammo/OneDrive/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData/FP_2020", pattern = "^2020")

#takes above list and keeps only FP file
fp.files <- magicfiles[grep(".fp", magicfiles, fixed=T)] 

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

