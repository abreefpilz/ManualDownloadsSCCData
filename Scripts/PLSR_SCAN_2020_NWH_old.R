
# PLSR Script for 1.6m SCAN Predictions of Fe and Mn
# Authors: Nick Hammond


#### Load Packages ####

#packages needed
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(transformr)
library(stringr)
library(readxl)



#### Load in FP files from GitHub for the 1.6m SCAN ####

#set working directory
setwd('./MagicData/FP_2020')

#create list with files from 1.6m SCAN (".fp" retrieves all files from 1.6m SCAN bc they all have a lower case fp)
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

#create list of the first line of each .fp file to make sure SCAN ID is consistent (QAQC)
id = list()
for(i in 2:length(fp.files)){
  abc = read.table(file=fp.files[i],nrows=1,header=FALSE, row.names = NULL,
                   sep = "\t", fill = TRUE)
  id = rbind(id, abc)
}
print(id)

#rename variables for wavelength columns
header = c("Date.Time","Status_0", seq(200.00, 750.00, 2.50))
colnames(obs) <- header

#plot some data from the combined file to check that all the data is there
plot(obs$Date.Time,obs$'725')

#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's
for(i in 2:nrow(obs)){
  time1 = obs$Date.Time[i-1]
  time2 = obs$Date.Time[i]
  int = interval(time1,time2)
  if(int_length(int) > (10*60)){
    print(int_length(int))
  }
}


#### Load in FP files from GitHub for the MUX ####
# Valve  # Depth
#   1       0.1
#   2       1.6
#   3       3.8
#   4       5.0
#   5       6.2
#   6       8.0
#   7       9.0
#   8       purge
#   9       acid
#   10      air
#   12      DI

muxfiles<-list.files(path=".", pattern = ".FP")

mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5))),"nm"), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="Etc/GMT+4")

for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(muxfiles[i])>4000){
    temp<-read.table(file=muxfiles[i],skip=2,header=FALSE, row.names = NULL, sep = "\t")
    names(temp) <- mux_colnames
    temp$DateTime=ymd_hms(temp$DateTime, tz="Etc/GMT+4")
    obs2<-rbind(obs2,temp)
    #print(i)
  }
}

mux_only=obs2[obs2$DateTime>"2020-04-24 14:15:00",] #change to not default to UTC, should be 14:15 in GMT 4
mux_only=mux_only[order(mux_only$DateTime),]

###### Pump log load ######
setwd("..")
log_files=list.files(path = ".", pattern = glob2rx("20*MUX.TXT"))
logs<-read.table(file=log_files[1],header=T, row.names = NULL, sep = ",", fill = TRUE, stringsAsFactors =F) #read in first file

for(i in 2:length(log_files)){ #reads in all files within folder in Github
  temp<-read.table(file=log_files[i], header=T, row.names = NULL, sep = ",",fill = TRUE, stringsAsFactors =F)
  logs<-rbind(logs,temp)
  #print(i)
}

pumpCols <- c("Time", "Valve", "Dir", "PumpTime", "Measure","Purge", "Notes")
colnames(logs) = pumpCols

logs=na.omit(logs)

logs$Time=ymd_hms(logs$Time, tz="Etc/GMT+4")

#filter out unnecessary data
logs <- logs %>%
  filter(str_detect(Measure,"Manual", negate = TRUE)) %>%
  filter(str_detect(Dir,"Forward")) %>%
  filter(str_detect(Notes,"Manual", negate = TRUE))%>%
  filter(str_detect(Notes,"Manual - Start!", negate = TRUE))

#fix structure of data to numerical or date
logs$PumpTime <- seconds(logs$PumpTime)

#create measurement time column
logs$Time_p_Pump <- logs$Time+logs$PumpTime

##### Assign proper pump valve with fp data #####

#assign valve by closest time in pump log, steps 2 program results in warning, need to fix maybe, only effects valve 9 on steps 2, code always chooses first valve listed
for (k in 1:nrow(mux_only)) {
  temptime = interval(start = mux_only$DateTime[k]-minutes(2), end = mux_only$DateTime[k]+minutes(2) ) #trying something out with data
  mux_only$correctvalve_a[k]=logs$Valve[logs$Time %within% temptime]
  #mux_only$correctvalve_b[k]=logs$Valve[logs$Time_p_Pump %within% temptime]
  mux_only$logtime_a[k]=logs$Time[logs$Time %within% temptime]
  #mux_only$logtime_b[k]=logs$Time_p_Pump[logs$Time %within% temptime]
}



#### Load in 2020 WQ data from local device storage (entire dataset in csv format) ####

#Change working directory to folder where WQ data is housed
setwd("C:/FCR_BVR Metals Data/Spreadsheets for Plotting/FCR/")

pathWQ = "C:/FCR_BVR Metals Data/Spreadsheets for Plotting/FCR/Subsetted Date Ranges/"
WQ = "FCR_Jan_Aug_2020.xlsx"
dataWQ<-read_xlsx(paste(pathWQ,WQ,sep="")) #Import data as .csv file


#### Subset SCAN, MUX, and WQ dataframes to desired date range ####
