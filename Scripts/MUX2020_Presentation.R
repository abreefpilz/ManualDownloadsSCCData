#Magic Mux 2020 Data Presentation
#Authors: Bethany Bookout, Nick Hammond, Rachel Corrigan

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

#plot some data from the combined file to check that all the data is there
plot(obs$Date.Time,obs$X450.00)
header = c("Date.Time","Status_0", seq(200.00, 750.00, 2.50))
colnames(obs) <- header


#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's
for(i in 2:nrow(obs)){
  time1 = obs$Date.Time[i-1]
  time2 = obs$Date.Time[i]
  int = interval(time1,time2)
  if(int_length(int) > (10*60)){
    print(int)
  }
}

#Plot wavelength vs. absorbance for discrete time points
#First we have to rearrange the data so that each time point is a column and 
# the rows are wavelengths
obs_trans = t(obs)
obs_trans = cbind(rownames(obs_trans), data.frame(obs_trans, row.names=NULL))
times = obs_trans[1,]
colnames(obs_trans) = times
obs_trans = obs_trans[-c(1,2),]



#MUX Load
muxfiles<-list.files(path=".", pattern = ".FP")

# install.packages("tidyverse")
# install.packages("magrittr")
library(lubridate)
library(tidyverse)
library(magrittr)

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

#pump log load
setwd("..")
log_files=list.files(path = ".", pattern = glob2rx("20*MUX.TXT"))
logs<-read.table(file=log_files[1],header=T, row.names = NULL, sep = ",", fill = TRUE) #read in first file


for(i in 2:length(log_files)){ #reads in all files within folder in Github
  temp<-read.table(file=log_files[1], header=T, row.names = NULL, sep = ",",fill = TRUE)
  logs<-rbind(logs,temp)
  #print(i)
}

pumpCols <- c("Time", "Valve", "Dir", "PumpTime", "Measure","Purge", "Notes")
colnames(logs) = pumpCols

logs=na.omit(logs)

logs$Time=ymd_hms(logs$Time)

#filter out unnecessary data
logs <- logs %>%
  filter(str_detect(Measure,"Manual", negate = TRUE)) %>%
  filter(str_detect(Dir,"Forward")) %>%
  filter(str_detect(Notes,"Manual", negate = TRUE))%>%
  filter(str_detect(Notes,"Manual - Start!", negate = TRUE))

#fix structure of data to numerical or date
logs$PumpTime <- seconds(logs$PumpTime)

#create measurement time column
logs$Time_p_Pump <- paste(PumpLogTime$Time, PumpLogTime$PumpTime, sep = ":")

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


#####Cleaning Script from Rachel######

#Things we want to change
#1.Filter by Automatic on $Notes
#2.Add seconds of $PumpTime instead of paste to $Time
#3.Change read in script to put all raw files together at beginning

#Read in pump log files and filter to select Forward pump directions
################################################################################
#however you have the pump logs saved on your computer, this is just how they were saved on mine.
#pumplogAug <- read.csv("2018pumpAug.csv")
#pumplogSep <- read.csv("2018pumpSep.csv")
#pumplogOct <- read.csv("2018pumpOct.csv")
#pumplogNov <- read.csv("2018pumpNov.csv")

pumpCols <- c("Time", "Valve", "Dir", "PumpTime", "Measure","Purge", "Notes")
colnames(pumplogAug) = pumpCols
colnames(pumplogSep) = pumpCols
colnames(pumplogOct) = pumpCols
colnames(pumplogNov) = pumpCols


pumpAugF <- pumplogAug %>%
  filter(str_detect(Dir,"Forward"))

pumpSepF <- pumplogSep %>%
  filter(str_detect(Dir,"Forward"))

pumpOctF <- pumplogOct %>%
  filter(str_detect(Dir,"Forward"))

pumpNovF <- pumplogNov %>%
  filter(str_detect(Dir,"Forward"))

#combine
PumpLogTime <- rbind(pumpAugF, pumpSepF, pumpOctF)

############################################################
#Formate and Create Time+Pump and Time+Pump+Measure Columns
#This might not work for you, but maybe it's helpful.
############################################################
#format time and pump time
PumpLogTime$Time <- as.POSIXct(strptime(PumpLogTime$Time, format= "%m/%d/%y %H:%M"))
PumpLogTime$Time <- format(PumpLogTime$Time, "%Y-%m-%d %H:%M")
PumpLogTime$PumpTime <- as.POSIXct(strptime(PumpLogTime$PumpTime, format = "%S"))
PumpLogTime$PumpTime <- format(PumpLogTime$PumpTime, "%S")
#assign 00 if no recording
PumpLogTime$PumpTime[is.na(PumpLogTime$PumpTime)] <- 00

#combine time and pump time
PumpLogTime$Time_p_Pump <- paste(PumpLogTime$Time, PumpLogTime$PumpTime, sep = ":")
PumpLogTime$Time_p_Pump <- as.POSIXct(PumpLogTime$Time_p_Pump, format = "%Y-%m-%d %H:%M:%S")

#format to add pump time and measure time
PumpLogTime$PumpTime <- as.numeric(as.character(PumpLogTime$PumpTime))
PumpLogTime$Measure <- as.numeric(as.character(PumpLogTime$Measure))

#add together
PumpLogTime$Measure[is.na(PumpLogTime$Measure)] <- 00
PumpLogTime$Pump_Meas <- as.duration(PumpLogTime$PumpTime + PumpLogTime$Measure)

#reformat to time
PumpLogTime$PumpTime <- as.POSIXct(strptime(PumpLogTime$PumpTime, format = "%S"))
PumpLogTime$PumpTime <- format(PumpLogTime$PumpTime, "%S")
PumpLogTime$Pump_Meas <- as.POSIXct(strptime(PumpLogTime$Pump_Meas, format = "%S"))
PumpLogTime$Pump_Meas <- format(PumpLogTime$Pump_Meas, "%S")

#add time and pump+measure time
PumpLogTime$TimePumpMea <- paste(PumpLogTime$Time, PumpLogTime$Pump_Meas, sep=":")
PumpLogTime$TimePumpMea <- as.POSIXct(PumpLogTime$TimePumpMea, format = "%Y-%m-%d %H:%M:%S")

#Going to write this out as a csv and finish in Excel for now aaaah

################################################################################
#Read in FP file for calibration
################################################################################

dataCalFP <- read.csv("FP_18.csv")
dataCalFP<-dataCalFP[,-216:-223] #Remove high wavelengths
colnames(dataCalFP)<-c("DateTime","status",seq(200,730,2.5)) #Add column names
dataCalFP$DateTime=ymd_hms(dataCalFP$DateTime, tz = "Etc/GMT+4")
dataCalFP$Date=NA
dataCalFP$Date=format(dataCalFP$DateTime, format ='%Y-%m-%d')
dataCalFP$Date=as.POSIXct(dataCalFP$Date, format ='%Y-%m-%d')

################################################################################
#Merge FP and Pump log times, then set port numbers for FP rows
################################################################################
#read in full pump log for 2018 with pump+measure time column made 

pumpAll <- read.csv("ALL_PUMP_TIME_2018.csv")
pumpAll$DateTime = NA
pumpAll$DateTime = pumpAll$TimePumpMea
pumpAll$DateTime <- mdy_hms(pumpAll$DateTime, format="%Y-%m-%d %H:%M:%S")
pumpAll$DateTime = as.POSIXct(pumpAll$TimePumpMea, format="%m/%d/%y %H:%M:%S")


#merge FP and pump log
all_pump_and_FP =merge(pumpAll, dataCalFP, by="DateTime", all=TRUE)

#assign port numbers for FP rows
for(i in 1:length(all_pump_and_FP$DateTime)){
  if(is.na(all_pump_and_FP$Valve[i])){
    all_pump_and_FP$Valve[i] = all_pump_and_FP$Valve[i-1]
  }
}

FP_with_port = subset(all_pump_and_FP, status == "Ok")  #scan with port
FP_with_port=FP_with_port[,-c(2,4:10, 225)]
FP_with_port=FP_with_port[!FP_with_port$Valve==10,]
FP_with_port$DateTime=ymd_hms(FP_with_port$DateTime, tz = "Etc/GMT+4")
FP_with_port$Date=NA
FP_with_port$Date=format(FP_with_port$DateTime, format ='%Y-%m-%d')
FP_with_port$Date=as.POSIXct(FP_with_port$Date, format ='%Y-%m-%d')
write.csv(FP_with_port, "All_FPwithPort_2018.csv")


FP_sort = FP_with_port[order(FP_with_port$Valve),]
FP_sort$DateTime = as.POSIXct(FP_sort$DateTime, format="%m/%d/%y %H:%M:S")
