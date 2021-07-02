
# Script to load in FP data for both 1.6m SCAN and MUX, 
# then match WQ data to FP data 
# Authors: Nick Hammond
# Last Updated: 07/02/2021


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
setwd('./MagicData')

#create list with files from 1.6m SCAN (".fp" retrieves all files from 1.6m SCAN bc they all have a lower case fp)
fp.files20<-list.files(path="./FP_2020", pattern = ".fp")
fp.files21<-list.files(path="./FP_2021", pattern = ".fp")
fp.files <- c(fp.files20,fp.files21)


### Read in first file
obs<-read.table(file=paste0("./FP_2020/",fp.files[1]),skip=1,header=TRUE, row.names = NULL,
                fill= TRUE, sep = "\t") #read in first file
obs$Date.Time=ymd_hms(obs$Date.Time, tz = "Etc/GMT+4")

#reads in all files within folder in Github
for(i in 2:length(fp.files)){
  if(file.exists(paste0("./FP_2020/",fp.files[i])))
  { 
    temp<-read.table(file=paste0("./FP_2020/",fp.files[i]),skip=1,header=TRUE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    temp$Date.Time=ymd_hms(temp$Date.Time, tz = "Etc/GMT+4")
    obs<-rbind(obs,temp)}
  if(file.exists(paste0("./FP_2021/",fp.files[i])))
  { 
    temp<-read.table(file=paste0("./FP_2021/",fp.files[i]),skip=1,header=TRUE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    temp$Date.Time=ymd_hms(temp$Date.Time, tz = "Etc/GMT+4")
    obs<-rbind(obs,temp)}
}


# Remove duplicated rows (resulting from overlap in files)
obs = unique(obs)

#create list of the first line of each .fp file to make sure SCAN ID is consistent (QAQC)
id = list()
for(i in 2:length(fp.files)){
  if(file.exists(paste0("./FP_2020/",fp.files[i]))){
    abc = read.table(file=paste0("./FP_2020/",fp.files[i]),nrows=1,header=FALSE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    id = rbind(id, abc)
  }
  if(file.exists(paste0("./FP_2021/",fp.files[i]))){
    abc = read.table(file=paste0("./FP_2021/",fp.files[i]),nrows=1,header=FALSE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    id = rbind(id, abc)
  }
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
  if(int_length(int) > (24*60*60)){
    print(int)
  }
}

SSCAN = obs


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

muxfiles<-list.files(path="./FP_2021", pattern = ".FP")

mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5))),"nm"), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="Etc/GMT+4")

for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(paste0("./FP_2021/",muxfiles[i]))>4000){
    temp<-read.table(file=paste0("./FP_2021/",muxfiles[i]),skip=2,header=FALSE, row.names = NULL, sep = "\t")
    names(temp) <- mux_colnames
    temp$DateTime=ymd_hms(temp$DateTime, tz="Etc/GMT+4")
    obs2<-rbind(obs2,temp)
    #print(i)
  }
}

# Plot data
plot(obs2$DateTime,obs2$`255nm`)

# Subset to date range after 2020-06-16 13:39 because data before that is messy
# May go back and clean this up later
mux_only=obs2[obs2$DateTime>"2020-06-16 14:00:00",] #change to not default to UTC, should be 14:15 in GMT 4
mux_only=mux_only[order(mux_only$DateTime),]

MUX = mux_only

#### Load in 2020 WQ data from local device storage (entire dataset in csv format) ####

#Change working directory to folder where WQ data is housed
setwd("C:/Users/hammo/OneDrive/Documents/Magic Sensor PLSR/")
pathWQ = "C:/Users/hammo/OneDrive/Documents/Magic Sensor PLSR/Data/"
WQ = "FCR_Jan_Nov_2020.xlsx"
dataWQ<-read_xlsx(paste(pathWQ,WQ,sep="")) #Import data as .csv file


#### Match WQ times with 1.6m SCAN times to find the reading closest to the sampling time ####

WQtimes <- dataWQ[which(dataWQ$Depth_m==1.6),]
WQtimes <- WQtimes$Date
WQtimes <- ymd_hms(WQtimes, tz="Etc/GMT+4")

df.final<-SSCAN %>% slice(which.min(abs(as.numeric(Date.Time) - as.numeric(WQtimes[1])))) #Create a new dataframe of just the first sample 
for (i in 2:length(WQtimes)){ #loop through all samples and add the closest values to the final dataframe
  SSCAN_atThisTime <- SSCAN %>% slice(which.min(abs(as.numeric(Date.Time) - as.numeric(WQtimes[i]))))
  df.final <- rbind(df.final,SSCAN_atThisTime)
}

SSCAN_FP_Overlaps_2020 = df.final

#Plot to check
plot(WQtimes,SSCAN_FP_Overlaps_2020$Date.Time, xlab = "Sampling Times",
     ylab = "SSCAN times")

#Write to csv
write.csv(SSCAN_FP_Overlaps_2020, file = "SSCAN_FP_Overlaps_2020.csv")

#### Match WQ times with MUX times to find the reading closest to the sampling time ####

#Subset date range to second deployment of MUX in Oct
WQtimes <- WQtimes[WQtimes>"2020-10-16 12:00:00"]

df.final<-MUX %>% group_by(Valve) %>% slice(which.min(abs(as.numeric(DateTime) - as.numeric(WQtimes[1])))) #Create a new dataframe of just the first sample
for (i in 2:length(WQtimes)){ #loop through all sample times and add the closest values to the final dataframe
  MUX_atThisDepth <- MUX %>% group_by(Valve) %>% slice(which.min(abs(as.numeric(DateTime) - as.numeric(WQtimes[i]))))
  df.final <- rbind(df.final,MUX_atThisDepth)
}

MUX_FP_Overlaps_2020 = df.final

#Plot to check
plot(WQtimes,MUX_FP_Overlaps_2020$DateTime[which(MUX_FP_Overlaps_2020$Valve==1)],
     xlab = "Sampling Times", ylab = "MUX times")

#Write to csv
write.csv(MUX_FP_Overlaps_2020, file = "MUX_FP_Overlaps_Oct_Nov_2020.csv")


# Write full time series of SSCAN and MUX FP data to csv
write.csv(SSCAN, file = "SSCAN_FP_TS_2020.csv")
write.csv(MUX, file = "MUX_FP_TS_2020.csv")
