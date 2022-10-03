#### Script for exploratory data analysis of MUX predictions and potential covariates ####
### Author: Nick Hammond
### Last Edited: 09/21/2022

#*****************************************************************
#* TITLE:  MUx 2020 Results Figures Scripts
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 03 Oct 2022
#*                                    
#* NOTES:  This script utilizes data files from '2a_PLSR_MUX_2020.R', 'thermocline_SchmidtStability_calcs_MUX20.R',
#*         and the EDI catwalk and met station data packages to produce multi-panel plots of:
#*         - PLSR predicted Fe and Mn concentrations w/ PI's
#*         - Catwalk Data (temp, DO)
#*         - Schmidt stability
#*         - Metoorological data
#*         Figures 4 & 5 in the manuscript and SI figures XXX
#*                                  
#*        
#*****************************************************************

# Load packages
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(transformr)
library(stringr)
library(readxl)
library(pls) 
library(scales)
library(ggpubr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(zoo)

setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")


#### Read and format catwalk data ####

# Specify directory and file name for data file
path = "C:/Users/hammo/Documents/Magic Sensor PLSR/Data/Covariate data/"
df_name = "FCR_Catwalk_2018_2021.csv"
#Download EDI catwalk dataset
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/6/23a191c1870a5b18cbc17f2779f719cf"  
infile1 <- c(paste0(path,df_name,sep=""))
download.file(inUrl1,infile1,method="curl")
#Import data as .csv file
catwalk = read_csv(paste(path,df_name,sep="")) 
# Select the reservoir, site, and date range we want
catwalk = catwalk %>% filter(Reservoir=="FCR" & Site==50) %>%
  filter(DateTime>"2020-10-16 13:10:00") %>%
  filter(DateTime<"2020-11-09 14:00:00")
#Select the variables we want
catwalk_exp = catwalk %>% select(Reservoir,Site,DateTime,RDO_mgL_5_adjusted,
                                 RDO_mgL_9_adjusted, EXODO_mgL_1,ThermistorTemp_C_surface,
                                 ThermistorTemp_C_1,ThermistorTemp_C_2, ThermistorTemp_C_3,
                                 ThermistorTemp_C_4, ThermistorTemp_C_8, ThermistorTemp_C_9,
                                 ThermistorTemp_C_5, ThermistorTemp_C_6, ThermistorTemp_C_7,
                                 EXOfDOM_QSU_1, EXOSpCond_uScm_1, EXOChla_ugL_1)
#### Convert DateTime to PosixCT
catwalk_exp$DateTime = ymd_hms(catwalk_exp$DateTime, tz="America/New_York") # Sensors are on EST (GMT+5) but converting to
                                                                            # EDT (GMT+4) to match MUX data

# convert temp and DO to long format for plotting
therm_depths = data.frame(depth_m = c(0.1,1:9), depth = c("ThermistorTemp_C_surface",
                                                        paste0("ThermistorTemp_C_",rep(1:9))))
DO_depths = data.frame(depth_m = c(1.6,5,9), depth = c("EXODO_mgL_1","RDO_mgL_5_adjusted",
                                                       "RDO_mgL_9_adjusted"))
catwalk_exp_long = catwalk_exp %>% pivot_longer(cols=c(7:16),names_to = "depth", 
                                                values_to = "temperature") %>%
  left_join(therm_depths, by = "depth")

DO_long = catwalk_exp %>% 
  select(Reservoir, Site, DateTime, EXODO_mgL_1, RDO_mgL_5_adjusted,
         RDO_mgL_9_adjusted) %>%
    pivot_longer(cols=c(4:6),names_to = "depth", values_to = "DO_mgL") %>%
  left_join(DO_depths, by = "depth")

# filter temp by depth
catwalk_exp_long = catwalk_exp_long %>% filter(depth_m %in% c(0.1,1,2,3,4,5,6,7,8,9))



#### Read and format met data ####
# Specify directory and file name for data file
df_name = "FCR_Met_final_2015_2021.csv"
#Download EDI met dataset
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/389/6/a5524c686e2154ec0fd0459d46a7d1eb"  
infile1 <- c(paste0(path,df_name,sep=""))
download.file(inUrl1,infile1,method="curl") 
#Import data as .csv file
met = read_csv(paste(path,df_name,sep=""))
#Select the reservoir, site, and date range we want
met = met %>% filter(Reservoir=="FCR" & Site==50) %>%
    filter(DateTime>"2020-10-16 13:10:00") %>%
    filter(DateTime<"2020-11-09 14:00:00")
# Select the variables we want
met_exp = met %>% select(Reservoir,Site,DateTime,WindSpeed_Average_m_s,ShortwaveRadiationDown_Average_W_m2,
                         Rain_Total_mm, AirTemp_Average_C)
#Convert DateTime to PosixCT
# Sensors are on EST (GMT+5) but converting to EDT (GMT+4) to match MUX data
met_exp$DateTime = ymd_hms(met_exp$DateTime, tz="America/New_York") 



#### Read and format MUX PLSR predictions ####
MUX_preds = read.csv(paste0(getwd(),"/Raw_predictions/MUX20_predictions_boot_050622.csv"))
MUX_preds$DateTime = ymd_hms(MUX_preds$DateTime, tz="America/New_York")
# Select date range for MUX predictions
MUX_preds = MUX_preds %>%
  filter(DateTime>"2020-10-16 13:10:00") %>%
  filter(DateTime<"2020-11-09 14:00:00")
# for plotting...
MUX_preds$Depth_m = as.numeric(MUX_preds$Depth_m)


#### Read and format FCR WQ data (from PLSR models) ####
dataWQ <- read_csv(paste0(getwd(),"/Raw_predictions/MUX20_dataWQ_050622.csv"))
dataWQ$DateTime = ymd_hms(dataWQ$DateTime, tz="America/New_York")
dataWQ = dataWQ %>% select(-c('...1','ID'))


#### Read in Thermocline depth data ####
ThermoDepths = read_csv(paste0(getwd(),"/Data/Covariate data/FCR_ThermoDepth_MUX20_013122.csv"))
# Convert DateTime to PosixCT
ThermoDepths$datetime = ymd_hms(ThermoDepths$datetime,tz="America/New_York")
# Smooth time series using moving average
ThermoDepths = ThermoDepths %>% 
  mutate(thermo.depth_roll = rollmean(thermo.depth,k=9,fill = NA))


#### Read in schmidt stability data ####
Schmidt = read_csv(paste0(getwd(),"/Data/Covariate data/FCR_SchmidtStability_MUX20_082322.csv"))
#Convert DateTime to PosixCT
Schmidt$datetime = ymd_hms(Schmidt$datetime,tz="America/New_York")


# vector to add turnover line
turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00")))
colnames(turnover)= c("Date")




#### Multipanel Plots ####

# Create variable for BeginTime and EndTime
Begin_time = as.POSIXct("2020-10-16 00:00:00")
End_time = as.POSIXct("2020-11-09 18:00:00")

# MUX Predictions

TFe_plot = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth_m)), size=1.5) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m)), size=7) +
  labs(x="Date",y="Total Fe (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
    theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 26),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))  

TMn_plot = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=TMn_mgL, color= as.character(Depth_m)), size=1.5) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerTMn_min, ymax=uncerTMn_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=TMn_mgL, colour= as.character(Depth_m)), size=7) +
  labs(x="Date",y="Total Mn (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 26),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black")) 
  
SFe_plot = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=SFe_mgL, color= as.character(Depth_m)), size=1.5) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerSFe_min, ymax=uncerSFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=SFe_mgL, colour= as.character(Depth_m)), size=7) +
  labs(x="Date",y="Soluble Fe (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 26),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))

SMn_plot = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=SMn_mgL, color= as.character(Depth_m)), size=1.5) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerSMn_min, ymax=uncerSMn_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=SMn_mgL, colour= as.character(Depth_m)), size=7) +
  labs(x="Date",y="Soluble Mn (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 26),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))


# Thermocline depth & Schmidt stability
thermocline_plot = ggplot() +
  geom_path(data=ThermoDepths, aes(x=datetime, y=thermo.depth_roll), size=2, color = "black") +
  labs(x="Date", y="Thermocline Depth (m)")+
  #theme_ipsum() +
  #theme(legend.position=c(0.95,0.95))+
  scale_x_datetime(date_minor_breaks = "1 day", limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect()) 

schmidt_plot = ggplot() +
  geom_path(data=Schmidt, aes(x=datetime, y=schmidt.stability), size=2, color = "black") +
  labs(x="Date", y= expression("Schmidt Stability (J/"*"m"^2*")"))+
  theme_bw() +
  #theme(legend.position=c(0.95,0.95))+
  scale_x_datetime(date_minor_breaks = "1 day", limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black")) 


# Catwalk sensor data
DO_plot = ggplot() +
  geom_path(data=DO_long, aes(x=DateTime, y=DO_mgL, color = as.character(depth_m)), size=1.5) +
  labs(x="Date",y="DO (mg/L)",color="Depth (m)") +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))

Cond_plot = ggplot() +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=EXOSpCond_uScm_1), size=1, color="blue") +
  xlab("Date") +
  ylab("Specific Cond. (uS/cm)") +
  ylim(c(35,50)) +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "blue", size=25),
  ) 

fdom_plot = ggplot() +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=EXOfDOM_QSU_1), size=1, color="brown") +
  xlab("Date") +
  ylab("fDOM (QSU)") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  ylim(c(18,24)) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=25),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.box.background = element_rect()) 

Temp_plot = ggplot() +
  geom_path(data=catwalk_exp_long, aes(x=DateTime, y=temperature, color = as.factor(depth_m)), size=1) +
  labs(x="Date", y="Temp. (deg C)", color= "Depth (m)")+
  theme_bw() +
  #theme(legend.position=c(0.95,0.95))+
  scale_x_datetime(date_minor_breaks = "1 day", limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))



# Met data
SW_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=ShortwaveRadiationDown_Average_W_m2), size=1.5) +
  labs(x="Date",y="Shortwave Rad. Down (W/m2)") +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))

wind_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=WindSpeed_Average_m_s), size=1.5, color="black") +
  labs(x="Date",y="Avg Wind Speed (m/s)") +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))

rain_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=Rain_Total_mm), size=1.5, color="black") +
  labs(x="Date",y="Total Rain (mm)") +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))

AirTemp_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=AirTemp_Average_C), size=1.5, color = "black") +
  labs(x="Date", y="Air Temp. (deg C)")+
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black"))

#### Create png file of multipanel plots ####

# Figure 4
jpeg('MUX20_Schmidt_Temp_DO_TFe_TMn_FullDepths_FullTS_092122.jpeg', width = 34, height = 36, units = 'in', res = 600)

schmidt_plot / Temp_plot / DO_plot / TFe_plot / TMn_plot + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 42, hjust = 0, vjust = 0))
 
dev.off()


# Figure SI_MUX_SFe_SMn_predictions
jpeg('MUX20_Schmidt_Temp_DO_SFe_SMn_FullDepths_FullTS_100322.jpeg', width = 34, height = 36, units = 'in', res = 600)

schmidt_plot / Temp_plot / DO_plot / SFe_plot / SMn_plot + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 42, hjust = 0, vjust = 0))

dev.off()

# Figure SI_met_data
jpeg('MUX20_Schmidt_SW_AirTemp_Wind_Rain_FullDepths_FullTS_100322.jpeg', width = 34, height = 36, units = 'in', res = 600)

schmidt_plot / SW_plot / AirTemp_plot / wind_plot / rain_plot  + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 42, hjust = 0, vjust = 0))

dev.off()



#### To plot soluble:totals ratios ####

# Convert negative values to zero
MUX_preds = MUX_preds %>% mutate(TFe_mgL = if_else(TFe_mgL > 0, TFe_mgL, 0),
                                 TMn_mgL = if_else(TMn_mgL > 0, TMn_mgL, 0),
                                 SFe_mgL = if_else(SFe_mgL > 0, SFe_mgL, 0),
                                 SMn_mgL = if_else(SMn_mgL > 0, SMn_mgL, 0))
# Calculate soluble:total ratios
MUX_preds = MUX_preds %>% mutate(Fe_ratio = SFe_mgL / TFe_mgL,
                                 Mn_ratio = SMn_mgL / TMn_mgL,
                                 Fe_ratio = if_else(Fe_ratio > 1, 1, Fe_ratio), # If ratios > 1, set to 1
                                 Mn_ratio = if_else(Mn_ratio > 1, 1, Mn_ratio)) %>%
  group_by(Depth_m) %>%                                    # rolling average
  mutate(Fe_ratio_ma10 = rollmean(Fe_ratio,k=10,fill = NA),
         Mn_ratio_ma10 = rollmean(Mn_ratio,k=10,fill = NA)) %>%
  ungroup(Depth_m)

# Split MUX_preds by depth (for plotting)
MUX_preds_hypo = MUX_preds %>% filter(Depth_m > 3.8)
MUX_preds_epi = MUX_preds %>% filter(Depth_m <= 3.8)

dataWQ_hypo = dataWQ %>% filter(Depth_m > 3.8)
dataWQ_epi = dataWQ %>% filter(Depth_m <= 3.8)





#### code for plotting Sol:Tot Fe and Mn on same plot (at all three hypo depths) ####
mux_preds_ratios = MUX_preds_hypo %>% rename(Fe = Fe_ratio_ma10, Mn = Mn_ratio_ma10) %>% 
  pivot_longer(cols = c(18:19),names_to = "variable", values_to = "ratio") # %>% 
  filter(Depth ==9)

Fe_Mn_ratio_plot = ggplot() +
  geom_path(data=mux_preds_ratios, aes(x=DateTime,y=ratio, color= as.factor(Depth_m)), size=3) +
  #geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth)), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m)), size=3.5) +
  labs(x="Date",y="Soluble:Total", title = "2020 Turnover Deployment", color = "Depth (m)") +
  ylim(0,1) +
  theme(legend.position="right")+
  #ggtitle("Predicted Total Fe and Sensor Data at 1.6m") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  facet_wrap(~variable, nrow = 2) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect(),
    title = element_text(size=40), 
    strip.text = element_text(size=36)
  )

png('MUX20_TFe_TMn_Ratios_hypo_FullTS_051322.png', width = 28, height = 12, units = 'in', res = 300)

Fe_Mn_ratio_plot

dev.off()







#### Old Code ####

Fe_ratio_plot = ggplot() +
  geom_point(data=MUX_preds_hypo, aes(x=DateTime,y=Fe_ratio_ma10, color= as.character(Depth_m)), size=3) +
  #geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m)), size=3.5) +
  labs(x="Date",y="Fe Soluble:Total", color = "Depth (m)") +
  ylim(0,1) +
  theme(legend.position="right")+
  #ggtitle("Predicted Total Fe and Sensor Data at 1.6m") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect()
  )


Mn_ratio_plot = ggplot() +
  geom_point(data=MUX_preds_hypo, aes(x=DateTime,y=Mn_ratio_ma10, color= as.character(Depth_m)), size=3) +
  #geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m)), size=3.5) +
  labs(x="Date",y="Mn Soluble:Total", color = "Depth (m)") +
  #ylim(0,1) +
  theme(legend.position="right")+
  #ggtitle("Predicted Total Fe and Sensor Data at 1.6m") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect()
  )




# Merge MUX metals predictions dataframe with catwalk dataframe
#combined = merge(MUX_preds,catwalk_exp,by="DateTime",all = TRUE)

surface = MUX_preds %>% filter(Depth == 0.1)
one_m = MUX_preds %>% filter(Depth == 1.6)
three_m = MUX_preds %>% filter(Depth == 3.8)
five_m = MUX_preds %>% filter(Depth == 5.0)
six_m = MUX_preds %>% filter(Depth == 6.2)
eight_m = MUX_preds %>% filter(Depth == 8.0)
nine_m = MUX_preds %>% filter(Depth == 9.0)


# Value used to transform the data
coeff_r <- (4)

# A few constants
FeColor <- "#69b3a2"
DOColor <- rgb(0.2, 0.6, 0.9, 1)
TempColor <- rgb(0.2, 0.6, 0.9, 1)

# DO vs. Fe Plot
png('MUX_Fe_DO_post_turnover.png', width = 15, height = 12, units = 'in', res = 300)
DO_plot = ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgL), size=2, color=FeColor) + 
  geom_path(data=catwalk_exp, aes(x=DateTime, y=RDO_mgL_9_adjusted/coeff_r), size=2, color=DOColor) +
  xlab("Date") +
  scale_y_continuous(
    # Features of the first axis
    name = "TFe_mgL",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff_r, name="DO (mg/L)")
  ) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.text.y.right = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = FeColor, size=25),
    axis.title.y.right = element_text(color = DOColor, size=25)
  ) 
#ggtitle(" ISCO Discharge and Fe Loads 2018")
DO_plot
dev.off()


# Temp vs. Fe Plot
png('MUX_Fe_Temp_post_turnover.png', width = 15, height = 12, units = 'in', res = 300)
Temp_plot = ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgL), size=2, color=FeColor) + 
  geom_path(data=catwalk_exp, aes(x=DateTime, y=ThermistorTemp_C_1 /coeff_r), size=2, color=TempColor) +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=ThermistorTemp_C_9 /coeff_r), size=2, color=TempColor) +
  xlab("Date") +
  scale_y_continuous(
    # Features of the first axis
    name = "TFe_mgL",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff_r, name="Temp (deg C)")
  ) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.text.y.right = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = FeColor, size=25),
    axis.title.y.right = element_text(color = TempColor, size=25)
  ) +
ggtitle("MUX Predicted TFe at 1.6m, Temperature at 1m")
Temp_plot
dev.off()




# alternate code
DO_plot <- ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgL)) +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=RDO_mgL_9_adjusted), colour="blue") +
  #ylim(0, 0.5)+
  labs(x="Date", y = "TFe_mgL", title = "Predicted TFe") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")
DO_plot


turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00")))
colnames(turnover)= c("Date")

png("One_m_only_033021.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=MUX_one, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth)), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  ylim(0,8)+
  scale_color_manual(values=c("#D39200"))+
  labs(x="Date", y = "Total Fe (mg/L)", title = "Total Iron Pre- and Post-Turnover 2020 (High-Frequency Observations)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()


MUX_one = MUX_preds %>% filter(Depth == 1.6)



#MUX_preds = MUX_preds %>%
#  group_by(Depth) %>%
#  mutate(TFe_ma10 = rollmean(TFe_mgL,k=10,fill = NA)) %>%
#  ungroup(Depth)

# Split MUX_preds by depth (for plotting)
#MUX_preds_hypo = MUX_preds %>% filter(Depth > 3.8)
#MUX_preds_epi = MUX_preds %>% filter(Depth <= 3.8)



#D39200 = 1.6m
#619CFF = 8m
#FF61C3 = 9m
#00C19F = 6.2m


