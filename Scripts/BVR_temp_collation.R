##BVR temp string collation script
#Author: Bethany Bookout
#Created: 12 FEB 2020
#Last Edited: 12 FEB 2020

library(plyr)
library(readr)
library(driftR) #package to read in EXO and hobo data frames

#load in all hobo temp string data
#how do I want to do this?? by depth? then merge by time?
mydir = "BVR_SensorString"
myfiles = list.files(path=mydir, pattern="Hobo*", full.names=TRUE)
myfiles

dat_csv = ldply(myfiles, read_csv)
dat_csv

#load minidot DO data
DO_5m=read.table("BVR_SensorString/BVR_DO_5m_20181206_20191206.TXT", sep=",", skip = 6, header = T)


#load EXO data
EXO_BVR=read.table("BVR_SensorString/BVR_1_5m_20191007_20191212.csv", sep= ",", skip = 8, header = T, blank.lines.skip = T)
#what is wrong with this loading?????
EXO_BVR=dr_read("BVR_SensorString/BVR_1_5m_20191007_20191212.csv", instrument = "EXO")
