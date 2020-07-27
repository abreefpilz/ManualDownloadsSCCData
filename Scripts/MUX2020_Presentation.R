#Magic Mux 2020 Data Presentation
#Authors: Bethany Bookout, Nick Hammond, Adrienne Breef-Pilz, Rachel Corrigan

#Outline

#1. 1.6 scan data
#a. Load in data
#b. Put all files tog
#2. 1.6 scan maintenance log? When does dataset start? April 10, disregard any data before then
#3. Mux 2020 data

#### 1.6 m SCAN load ####
# important dates: April 10 15:19:53 4.5m scan deployed; 
# April 24 9:49:52 4.5m pulled up, mux deployed again, 
# start all data on April 10 15:19

#packages need
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)

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

#create list of the first line of each .fp file to make sure SCAN ID is consistent
id = list()
for(i in 2:length(fp.files)){
  abc = read.table(file=fp.files[i],nrows=1,header=FALSE, row.names = NULL,
                  sep = "\t", fill = TRUE)
  id = rbind(id, abc)
}

#plot some data from the combined file to check that all the data is there
plot(obs$Date.Time,obs$'725')
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
colnames(obs_trans)[1] = "wavelength"
obs_trans = obs_trans[-c(1,2),]

plot(obs_trans$wavelength,obs_trans$`2020-04-01 11:56:59`, type='l', ylim=c(0,30))
lines(obs_trans$wavelength,obs_trans$`2020-05-01 12:02:38`, col="red")
lines(obs_trans$wavelength,obs_trans$`2020-06-01 11:56:50`, col="green")
lines(obs_trans$wavelength,obs_trans$`2020-07-01 12:00:12`, col="yellow")

# Create animated GIF of wavelength vs. absorption over time
#To create animation- rearrange data so that there are three columns:
#Date.Time, Wavelength, Absorbance
obs_animate = pivot_longer(obs, cols=3:223, names_to = "wavelength", values_to = "absorbance")

#subset data to a smaller interval (one day)
sub= interval(start=obs_animate$Date.Time[1], end=obs_animate$Date.Time[221*144])
obs_animate_sub = obs_animate[obs_animate$Date.Time %within% sub,]

# Create animated GIF of wavelength vs. absorption over time
#install.packages('gganimate')
require(gganimate)
require(transformr)
p <- ggplot(obs_animate_sub, aes(x = wavelength, y = absorbance)) +
  geom_line(aes(group=Date.Time))+ 
  transition_time(Date.Time) +
  labs(title = "Date.Time: {frame_time}")
a <-  animate(p)+
  save_animation()
anim_save("1.6m_1day.gif",animation = a)




###### MUX Load ######
muxfiles<-list.files(path=".", pattern = ".FP")


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
logs$Time_p_Pump <- logs$Time+logs$PumpTime

##### Assign proper pump valve with fp data #####

#assign valve by closest time in pump log
for (k in nrow(obs2)) {
  #obs2$correctedvalve[k]=logs$Valve[which(logs$Time)]
  #logs$Time<obs2$Time<logs$Time_p_pump
}



##### 4.5 m scan #####
deploy_time = interval(start = "2020-04-10 15:15:00", end = "2020-04-24 10:00:00" )
scan_45=obs2[obs2$DateTime %within% deploy_time,]

#plot some data to check that it's all there
plot(scan_45$DateTime,scan_45$'252.5nm')

#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's
for(i in 2:nrow(scan_45)){
  time1 = scan_45$DateTime[i-1]
  time2 = scan_45$DateTime[i]
  int = interval(time1,time2)
  if(int_length(int) > (10*60)){
    print(int)
  }
}

#Plot wavelength vs. absorbance for discrete time points
#First we have to rearrange the data so that each time point is a column and 
# the rows are wavelengths
scan_45_trans = t(scan_45)
scan_45_trans = cbind(rownames(scan_45_trans), data.frame(scan_45_trans, row.names=NULL))
times = scan_45_trans[1,]
colnames(scan_45_trans) = times
colnames(scan_45_trans)[1] = "wavelength"
scan_45_trans = scan_45_trans[-c(1,2),]

plot(scan_45_trans$wavelength,scan_45_trans$`2020-04-10 15:27:45`, type='l', ylim=c(0,30))
lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-13 15:19:59`, col="red")
lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-17 15:20:01`, col="green")
lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-23 15:20:01`, col="yellow")

# Create animated GIF of wavelength vs. absorption over time
#To create animation- rearrange data so that there are three columns:
#Date.Time, Wavelength, Absorbance
scan_45_animate = pivot_longer(scan_45, cols=3:223, names_to = "wavelength", values_to = "absorbance")

#subset data to a smaller interval (one day)
sub= interval(start=scan_45_animate$Date.Time[1], end=scan_45_animate$Date.Time[221*144])
scan_45_animate_sub = scan_45_animate[scan_45_animate$Date.Time %within% sub,]


#####Cleaning Script from Rachel######


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
