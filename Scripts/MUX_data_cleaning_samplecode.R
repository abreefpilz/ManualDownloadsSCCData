####################
# PLSR Script @ FCR, for Nick and Bethany 
# Rachel Corrigan
# July 2020

#Files needed to run PLSR: 
# 1. (dataWQ) Water quality data
# 2. (dataCalFP) FP data corresponding to water quality data dates and depths (ports)
# 3. (TS_FP) Full timeseries of FPs from which you are trying to estimate chemsitry
#
# The water quality data is easy to work with! So I don't have any tips or tricks for that.
#
# There are lots of steps to get the FP data formatted. FP files (TS_FP and dataCalFP) need to have ports/depths matched for each row. 
# You need to use the pump logs to match the times and valves correctly for each scan. To do this, I use the pump log times and
# make a time column that is the pump time (hour:minutes) plus the measure time (seconds).
# The measure time (H:M:S) is always a few seconds lagged from the time on the FP scan. 
# The I merge the FP file with the pump log file by time, and set the port for rows that do not have
# ports (i.e. the rows from the FP dataframe) as the prior port number. This is likely a 
# crude way of matching things, but it's working for now! Currently, I am shortcutting using Excel to make sure all 
# of my times are formatted correctly for my dataCalFP file.

# After getting the ports matched to the FP files, make a dataframe that has 1 FP scan for each day that you
# have full depth water quality data. So if you have full depths, then you will have the same number
# of rows of FP data. You have to have the same number of rows for dataWQ and dataCalFP because that is used
# to make the regression equation. This equation is whats used to estimate the chemistry from the full
# FP timeseries (TS_FP).

# This code is not perfect and will not work for you. Hopefully it shows the steps that I've been developing
# to get my data formatted for PLSR!

 
################################################################################
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
write.csv(FP_sort, "timeSeries_FP_Oct2018_RSC.csv")



################################################################################
#Subset to corresponding dates (this is specific to the time that you're doing the PLSR)
################################################################################
#filter observed wq data and FP to dates that we have overlap 
sub_fcrWQ <- fcrWQ %>%
  filter(DateTime >= '2018-10-01' & DateTime <= '2018-10-29') %>%
  filter(Site == 50)

sub_dataCalFP <- FP_with_port %>%
  filter(Date >= '2018-10-01' & Date <= '2018-10-29')%>%
  filter(Valve != 2) 

#filter observed wq data and FP to dates that we have overlap WHOLE YEAR 
sub_fcrWQ <- fcrWQ %>%
  filter(Site == 50)   #incase there are any other sites hanging on in your dataframe

sub_dataCalFP <- FP_with_port 

################################################################################
#Subset to filter one scan per day for dataCalFP
################################################################################
#I was just selecting the first 9 observations from the scan, one of each depth. 
#THIS DOESN'T WORK ALL OF THE TIME and I'm having to do this in other ways for 2019.

# it would be better to select the first occurance of each port for each day but I have
# not written that yet. 

n <- 9
sub_dataCalFP$Date=as.Date(sub_dataCalFP$DateTime, format="%Y-%m-%d")

unique_FP <- sub_dataCalFP %>%
  distinct(Valve, Date, .keep_all = TRUE)

day_dataCalFP <- day_dataCalFP %>%
  group_by(Date)%>%
  distinct(Valve, Date, .keep_all = TRUE)#%>%
#slice(seq_len(8)) 

#double check that this worked!

################################################################################
#Now sub just one scan for just overlapping days!
################################################################################
sub_fcrWQ$DateTime=as.Date(sub_fcrWQ$DateTime, format="%Y-%m-%d")
day_fcrWQ <- sub_fcrWQ %>%
  filter(DateTime %in% sub_dataCalFP$Date)
#day_fcrWQ=day_fcrWQ[-(25:27),] #remove day that isnt complete
day_fcrWQ=day_fcrWQ[,-(2:3)] #remove unnecessary columns
day_fcrWQ$ID <- seq.int(nrow(day_fcrWQ)) #add row index
day_fcrWQ=day_fcrWQ[,c(13,1:12)] #reorder columns to have index first


day_dataCalFP <- sub_dataCalFP %>%
  filter(Date %in% day_fcrWQ$DateTime) 

day_dataCalFP$ID <- seq.int(nrow(day_dataCalFP)) #add row index
day_dataCalFP=day_dataCalFP[,c(218,1:216)] #reorder columns to have index first and remove data row that was made for data cleaning


#should be 32 of each for October 2018
timesCalFP<-cbind(as.data.frame(day_dataCalFP[,1]))#, rep(c(1,3:9),4)) #date from scan and port number. I removed port 2 here because there is no corresponding depth for wq samples

#write out csvs to make PLSR script cleaner
write.csv(day_dataCalFP, "FPday_Oct2018_RSC.csv")
write.csv(day_fcrWQ, "WQ_noOutliersRemoved_Oct2018_RSC.csv")

write.csv(day_dataCalFP, "allFP_with_WQoverlap_2018.csv")
write.csv(day_fcrWQ, "allWQoverlap2018.csv")

####################################
# PLSR PORTION OF SCRIPT taken from CCC script
####################################


library(pls)  #Load the pls package
library(lubridate)
library(dplyr)
library(stringr)
library(scales)
pathD<-"/Users/rachelcorrigan/Dropbox/fcr/MagicData/" #Specify folder where data is located
setwd("/Users/rachelcorrigan/Dropbox/fcr/MagicData/")


################################################################################
#Read in FCR WQ data (chem + metals)
################################################################################

WQ<-"WQ_noOutliersRemoved_Oct2018_RSC.csv"  #Specify file where data is located  
dataWQ<-read.table(file=paste(pathD,WQ,sep=""),sep=",",header = TRUE)  #Import data as .csv file
#dataWQ=dataWQ[,-1]
WQparam <- c("TN_ugL","TP_ugL","NH4_ugL","NO3NO2_ugL","SRP_ugL","DOC_mgL","TFe_mgL","TMn_mgL","SFe_mgL","SMn_mgL")   


################################################################################
#### Reading of  FingerPrint (FP) file corresponding to lab concentrations for calibration 
#### 
FPcaldata_name<-"FPday_Oct2018_RSC.csv"  #FP that overlaps with WQ days and depths
dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
dataCalFP=dataCalFP[,-1]
colnames(dataCalFP)<-c("ID","Date/Time","status",seq(200,730,2.5)) #Add column names
timesCalFP<-cbind(data.matrix(dataCalFP[,2]),rep(c(1,3:9),4)) #date from scan and port number. I removed port 2 here because there is no corresponding depth for wq samples
dataCalFP<-dataCalFP[,-1:-3] #Remove unneccessary columns
dataCalFP<-data.matrix(dataCalFP) #Convert to data matrix


################################################################################
#### This replaces the ID and Date from the original dataWQ with the exact values
#### from the SCAN so that manual values can be plotted later on in the TS plots
dataWQ$ID<-timesCalFP[,2]
dataWQ$Date<-timesCalFP[,1]

################################################################################
#### Reading of  FingerPrint (FP) file corresponding to the entire time series (TS) 
#### 
TimeSeriesFP_name<-"timeSeries_FP_Oct2018_RSC.csv"
#TimeSeriesFP_name<-"FP_with_Port_ALL2018.csv"
TS_FP<-read.table(file=paste(pathD,TimeSeriesFP_name,sep=""),sep=",", skip=1)  #Import Time Series data as .csv file
colnames(TS_FP)<-c("port","Date","status",seq(200,730,2.5)) #Add column names
TS_FP$Date = as.POSIXct(TS_FP$Date, format = "%m/%d/%y %H:%M:%S")
#TS_FP<-TS_FP[!(TS_FP$port==11),] 
Dat<-strptime(TS_FP$Date, format = "%Y-%m-%d %H:%M:%S") #Create record of date and time

################################################################################
####  Create matrix to store calculated concentrationss:TS_conc
TS_conc<-as.data.frame(matrix(0,dim(TS_FP)[1],12))  #Create data frame for date/time and predicted NO3-N values
TS_conc[,1]<-TS_FP$port
TS_conc[,2]<-as.character(Dat, "%Y-%m-%d %H:%M:%S")
colnames(TS_conc)<-c("port","DateTime",WQparam) #Add column names
TS_FP<-TS_FP[,(-1:-3)]
TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix

#######################
#Specify number of components for wq param
ncomp=7 #7 for TP and TN, but need to determin number of components for each WQ param

################################################################################
#### function which does the calibration and then calculates conc from the TS for
#### a given chemical parameter (param).  It does the calibration for a given 
#### number of components (ncomp)
################################################################################


PLSR_SCAN<-function(param,dataCalFP,dataWQ,TS_FP,ncomp,yesplot=FALSE){
  
  WQ<-data.matrix(subset(dataWQ,select=param)) #Make matrix of the param values
  temp<-cbind(dataCalFP,WQ) #combines FP and WQ columns to remove the rows containing NAs 
  temp<-temp[complete.cases(temp),] #removes the rows containing NAs
  WQ<-data.matrix(subset(temp,select=param)) # recreate a data matrix from the WQ vector minus the NAs
  dataFP<-temp[,-dim(temp)[2]]  # redefines the FP matrix rid off the NA values of missing WQ
  
  
  fit<-plsr(WQ~data.matrix(dataFP),ncomp=ncomp,validation="CV")  #PLSR model to predict param with cross validation
  summary(fit)  #See summary of PLSR model to choose number of components
  Pfit<-predict(fit,dataFP,ncomp=ncomp,type=c("response")) #Predict concentrations based on PLSR model
  #x11()
  WQP<-as.data.frame(matrix(0,1,dim(dataFP)[1])) #Create data frame for predicted param values
  WQP<-as.data.frame(Pfit[1:length(Pfit)])  #Insert predicted param values into data frame
  
  Pfit_TS<-predict(fit,TS_FP,ncomp=ncomp,type=c("response"))
  WQP_TS<-as.data.frame(matrix(0,1,dim(TS_FP)[1])) #Create data frame for predicted Time series values
  WQP_TS<-as.data.frame(Pfit_TS[1:length(Pfit_TS)])  #Insert predicted param values into data frame
  
  if (yesplot==TRUE){
    plot(WQ,as.matrix(WQP),
         xlab=paste("measured",param,"?g/L",sep=" "),
         ylab=c("PLSR_predicted")) #Compare predicted and lab values of param
    
    fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
    abline(fit2)
    summary(fit2)
    tp_resid <- resid(fit2)
  }
  
  assign("WQP_TS",WQP_TS,env=.GlobalEnv)
}



################################################################################
#### Example for running the function for one parameter only
################################################################################

param<-"TP_ugL"
ncomp=7
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)


#############################
##Run whats inside the function to plot and identify points that are outliers
out <- sapply(list(WQ,as.matrix(WQP)),"[",identify(WQ,as.matrix(WQP)))
out

#remove points
dataWQ = dataWQ[-c(7),]
dataCalFP = dataCalFP[-c(7),]
timesCalFP = timesCalFP[-c(7),]
#then rerun PLSR 

