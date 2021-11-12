
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
header = c("DateTime","Status_0", seq(200.00, 750.00, 2.50))
colnames(obs) <- header

#plot some data from the combined file to check that all the data is there
plot(obs$DateTime,obs$'725')

#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's
for(i in 2:nrow(obs)){
  time1 = obs$DateTime[i-1]
  time2 = obs$DateTime[i]
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
obs2 <- as.data.frame(matrix(NA,0,length(mux_colnames)))
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

# Fix timestamp at 05-27-2021 23:43:29
obs2[207,1] = mdy_hms("05-27-2021 23:43:29",tz="Etc/GMT+4")

# Ensure data are in chronological order
obs2=obs2[order(obs2$DateTime),]

MUX = obs2

#### Load in 2020 WQ data from local device storage (entire dataset in csv format) ####

#Change working directory to folder where WQ data is housed
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")
pathWQ = "C:/Users/hammo/Documents/Magic Sensor PLSR/Data/"
WQ = "Metals_2021.xlsx"
dataWQ<-read_xlsx(paste(pathWQ,WQ,sep="")) #Import data as .csv file

# Filter dataWQ to just the necessary data
dataWQ = dataWQ %>% filter(Reservoir=="FCR") %>%
  filter(Site=="50") %>%
  select(1:8) %>%
  mutate(DateTime = ymd_hms(DateTime, tz = "Etc/GMT+4"))

#### Match WQ times with 1.6m SCAN times to find the reading closest to the sampling time ####
   # ! Need to edit this to mirror the code below for the MUX ! #

WQtimes <- dataWQ %>% filter(Depth_m==1.6) %>%
  select(DateTime)

df.final<-SSCAN %>% slice(which.min(abs(as.numeric(DateTime) - as.numeric(WQtimes$DateTime[1])))) #Create a new dataframe of just the first sample 
for (i in 2:length(WQtimes$DateTime)){ #loop through all samples and add the closest values to the final dataframe
  SSCAN_atThisTime <- SSCAN %>% slice(which.min(abs(as.numeric(DateTime) - as.numeric(WQtimes$DateTime[i]))))
  df.final <- rbind(df.final,SSCAN_atThisTime)
}

SSCAN_FP_Overlaps_2021 = df.final

#Plot to check
plot(WQtimes,SSCAN_FP_Overlaps_2021$DateTime, xlab = "Sampling Times",
     ylab = "SSCAN times")

#Write to csv
write.csv(SSCAN_FP_Overlaps_2021, file = "SSCAN_FP_Overlaps_2021.csv")

#### Match WQ times with MUX times to find the reading closest to the sampling time ####

# Create a 'Depths' column for the MUX df, to allow subsetting by time and depth
Valves = as.data.frame(MUX$Valve)
colnames(Valves)=c("valve")
valve_depth <- data.frame(
  valve = c (1:7,9,10,12), 
  Depth= c("0.1","1.6","3.8","5.0","6.2","8.0","9.0","acid","air","DI"),
  stringsAsFactors = FALSE
)
Valves = Valves %>% 
  left_join(valve_depth, by="valve") %>% 
  select(Depth)
MUX = cbind(MUX,Valves)
MUX$Depth = as.numeric(MUX$Depth)

WQtimes <- dataWQ %>% select(DateTime, Depth_m)
#WQtimes$Depth_m <- as.character(WQtimes$Depth_m)
WQtimes <- WQtimes[WQtimes$DateTime>"2021-05-26 00:30:00",]

# Remove samples from 06-02-2021 because the MUX was not deployed during that time
WQtimes <- WQtimes %>% filter(date(DateTime) != "2021-06-02")

df.final<- MUX %>%
  filter(Depth == WQtimes$Depth_m[1]) %>% slice(which.min(abs(as.numeric(DateTime) - as.numeric(WQtimes$DateTime[1])))) #Create a new dataframe of just the first sample
for (i in 2:length(WQtimes$DateTime)){ #loop through all sample times and add the closest values to the final dataframe
  MUX_atThisDepth <- MUX %>%
    filter(Depth == WQtimes$Depth_m[i]) %>% slice(which.min(abs(as.numeric(DateTime) - as.numeric(WQtimes$DateTime[i]))))
  df.final <- rbind(df.final,MUX_atThisDepth)
}

MUX_FP_Overlaps_2021 = df.final

#Plot to check
plot(WQtimes$DateTime,MUX_FP_Overlaps_2021$DateTime,
     xlab = "Sampling Times", ylab = "MUX times")

plot(MUX_FP_Overlaps_2021$DateTime,MUX_FP_Overlaps_2021$Depth)
points(WQtimes$DateTime,WQtimes$Depth_m,col="blue")

#Write to csv
write.csv(MUX_FP_Overlaps_2021, file = "MUX_FP_Overlaps_2021.csv")


# Write full time series of SSCAN and MUX FP data to csv
write.csv(SSCAN, file = "SSCAN_FP_TS_2021.csv")
write.csv(MUX, file = "MUX_FP_TS_2021.csv")
