##BVR temp string collation script
#Author: Bethany Bookout
#Created: 12 FEB 2020
#Last Edited: 12 FEB 2020

library(plyr)
library(readr)

mydir = "BVR_SensorString"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles

dat_csv = ldply(myfiles, read_csv)
dat_csv