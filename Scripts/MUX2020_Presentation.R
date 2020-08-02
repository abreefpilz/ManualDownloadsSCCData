#Magic Mux 2020 Data Presentation
#Authors: Bethany Bookout, Nick Hammond, Adrienne Breef-Pilz, Rachel Corrigan

#MUX depth code for 2020, 1 = surface; 2 = 1.6m; 3= 3.8m; 4 = 5.0m; 5 = 6.2m; 6 = 8.0m; 7 = = 9.0m; 9 = acid rinse; 10 = air; 12 = water rinse (used surface and DI rinse at different points in 2020)

#### 1.6 m SCAN load ####
# important dates: April 10 15:19:53 4.5m scan deployed; 
# April 24 9:49:52 4.5m pulled up, mux deployed again, 
# start all data on April 10 15:19

#packages need
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(transformr)
library(stringr)


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
header = c("Date.Time","Status_0", seq(200.00, 750.00, 2.50))
colnames(obs) <- header
plot(obs$Date.Time,obs$'725')

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

##old code!
##Plot wavelength vs. absorbance for discrete time points
##First we have to rearrange the data so that each time point is a column and 
##the rows are wavelengths

#obs_trans = t(obs)
#obs_trans = cbind(rownames(obs_trans), data.frame(obs_trans, row.names=NULL))
#times = obs_trans[1,]
#colnames(obs_trans) = times
#colnames(obs_trans)[1] = "wavelength"
#obs_trans = obs_trans[-c(1,2),]

#plot(obs_trans$wavelength,obs_trans$`2020-04-01 11:56:59`, type='l', ylim=c(0,30))
#lines(obs_trans$wavelength,obs_trans$`2020-05-01 12:02:38`, col="red")
#lines(obs_trans$wavelength,obs_trans$`2020-06-01 11:56:50`, col="green")
#lines(obs_trans$wavelength,obs_trans$`2020-07-01 12:00:12`, col="yellow")

# Create animated GIF of wavelength vs. absorption over time
#To create animation- rearrange data so that there are three columns:
#Date.Time, Wavelength, Absorbance
obs_animate = pivot_longer(obs, cols=3:223, names_to = "wavelength", values_to = "absorbance")

#subset data to a smaller interval (one day)
sub= interval(start="2020-04-10 15:19:53", end="2020-04-24 9:49:52", tz="Etc/GMT+4")
obs_animate_sub = obs_animate[obs_animate$Date.Time %within% sub,]
obs_animate_sub$wavelength = as.numeric(obs_animate_sub$wavelength)

# Create animated GIF of wavelength vs. absorption over time
#install.packages('gganimate')

p <- ggplot(obs_animate_sub, aes(x = wavelength, y = absorbance)) +
  geom_line(aes(group=Date.Time))
a <- p + transition_time(Date.Time) +
  labs(title = "Date.Time: {frame_time}") +
  scale_x_continuous(breaks = c(seq(200,750,100)), limits = c(200,750)) +
  ease_aes('cubic-in-out')
animate(a, nframes=150, fps=6)
anim_save("1.6_Apr10_Apr24.gif", animation = last_animation())

#More plots (for powerpoint)
dev.off()
# All wavelengths
ggplot(obs_animate, aes(x=Date.Time,y=absorbance))+
  geom_line(aes(colour=factor(wavelength))) +
  theme(legend.position='none')
# Select a few wavelengths
obs_animate2 = filter(obs_animate, wavelength==200|wavelength==300|wavelength==400
   |wavelength==500|wavelength==600|wavelength==700)

# Add vertical lines for cleaning dates
cleaning = as.POSIXct(c("2020-04-20 12:40", "2020-05-04 15:00", "2020-05-11 12:30",
                        "2020-05-18 10:55", "2020-05-25 10:52", "2020-06-22 10:18",
                        "2020-06-29 12:40", "2020-07-06 12:48", "2020-07-13 11:24",
                        "2020-07-20 11:50"), tz="Etc/GMT+4")

png("1.6m_ts_2020.png",width = 7, height = 3, units = 'in', res = 300)
ggplot(obs_animate2, aes(x=Date.Time,y=absorbance))+
  geom_line(aes(colour=factor(wavelength)))+
  geom_vline(xintercept = cleaning, linetype="dotted", 
             color = "black", size=0.6)
dev.off()
  #theme(legend.position='none')
# Plot a Subset of same time period as 4.5m Scan
obs_animate_sub2 = filter(obs_animate_sub, wavelength==200|wavelength==300|wavelength==400
                      |wavelength==500|wavelength==600|wavelength==700)
png("1.6m_ts_Apr10_24.png",width = 7, height = 3, units = 'in', res = 300)
ggplot(obs_animate_sub2, aes(x=Date.Time,y=absorbance))+
  geom_line(aes(colour=factor(wavelength)))
dev.off()





###### MUX Load ######

# Valve  # Depth
#   1       0.1
#   2       1.6
#   3       3.8
#   4       5.0
#   5       6.2
#   6       8.0
#   7       9.0
#   8       NA
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

mux_only=obs2[obs2$DateTime>"2020-04-24 10:15:00",] #change to not default to UTC, should be 14:15 in GMT 5
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
  mux_only$correctvalve_b[k]=logs$Valve[logs$Time_p_Pump %within% temptime]
  mux_only$logtime_a[k]=logs$Time[logs$Time %within% temptime]
  mux_only$logtime_b[k]=logs$Time_p_Pump[logs$Time %within% temptime]
}

mux_only2=mux_only[,c(1,224,226:229,2:223,225)]

#####graphs of absorbance and wavelength for 2020 for each depth######

#create a data frame of valve number and depth
valve_depth <- data.frame(
  Valve = c (1:12), 
  Depth= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0", "NA", "acid_r", "air","NA", "water_r"),
  stringsAsFactors = FALSE
)

#put the data in long format and add valve depth

mux_only_long=mux_only%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")%>%
  left_join(valve_depth, by="Valve")%>%
  filter(wavelength %in% c("200nm", "300nm", "400nm", "500nm", "600nm", "700nm"))%>%
  filter(Depth %in% c('0.1','1.6','3.8','5.0','6.2','8.0','9.0'))


#create  a multipanel plot of absorbance over time separated by depth 

ggplot(mux_only_long, aes(x=DateTime, y=absorbance, color=wavelength)) + 
 geom_line() +
   facet_grid(rows = vars(Depth))


##### 4.5 m scan #####
deploy_time = interval(start = "2020-04-10 15:15:00", end = "2020-04-24 10:00:00", tz="Etc/GMT+4" )
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

##old code!
##Plot wavelength vs. absorbance for discrete time points
##First we have to rearrange the data so that each time point is a column and 
##the rows are wavelengths
#scan_45_trans = t(scan_45)
#scan_45_trans = cbind(rownames(scan_45_trans), data.frame(scan_45_trans, row.names=NULL))
#times = scan_45_trans[1,]
#colnames(scan_45_trans) = times
#colnames(scan_45_trans)[1] = "wavelength"
#scan_45_trans = scan_45_trans[-c(1,2),]

#plot(scan_45_trans$wavelength,scan_45_trans$`2020-04-10 15:27:45`, type='l', ylim=c(0,30))
#lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-13 15:19:59`, col="red")
#lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-17 15:20:01`, col="green")
#lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-23 15:20:01`, col="yellow")

# Create animated GIF of wavelength vs. absorption over time
#To create animation- rearrange data so that there are three columns:
#Date.Time, Wavelength, Absorbance
scan_45_animate = pivot_longer(scan_45, cols=3:223, names_to = "wavelength", values_to = "absorbance")
wvlng = str_split_fixed(scan_45_animate$wavelength,pattern = "(nm)$", n=2)
wvlng = wvlng[,1]
scan_45_animate$wavelength = wvlng
scan_45_animate$wavelength = as.numeric(scan_45_animate$wavelength)

#subset data to a smaller interval (one day)
sub_45 = interval(start="2020-04-10 15:19:53", end="2020-04-24 9:49:52", tz="Etc/GMT+4")
scan_45_animate_sub = scan_45_animate[scan_45_animate$DateTime %within% sub_45,]


# Create animated GIF of wavelength vs. absorption over time
#install.packages('gganimate')

p <- ggplot(scan_45_animate_sub, aes(x = wavelength, y = absorbance)) +
  geom_line(aes(group=DateTime))
a <- p + transition_time(DateTime) +
  labs(title = "DateTime: {frame_time}") +
  scale_x_continuous(breaks = c(seq(200,750,100)), limits = c(200,750)) +
  ease_aes('cubic-in-out')
animate(a, nframes=150, fps=6)
anim_save("4.5_Apr10_Apr24.gif", animation = last_animation())

# More plots!
scan_45_animate2 = filter(scan_45_animate, wavelength==200|wavelength==300|wavelength==400
                      |wavelength==500|wavelength==600|wavelength==700)

png("4.5m_ts_Apr10_24.png",width = 7, height = 3, units = 'in', res = 300)
ggplot(scan_45_animate2, aes(x=DateTime,y=absorbance))+
  geom_line(aes(colour=factor(wavelength)))
dev.off()

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
