#### Script for exploratory data analysis of MUX predictions and potential covariates ####
### Author: Nick Hammond
### Last Edited: 03/30/2021

# Set wd, load packages, source code
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

setwd("C:/Users/hammo/OneDrive/Documents/Magic Sensor PLSR/")



# Read in catwalk data
catwalk = read.csv(paste0(getwd(),"/Data/Covariate data/Catwalk_EDI_2020.csv"))

# Read in met data
met = read.csv(paste0(getwd(),"/Data/Covariate data/Met_final_2015_2020.csv"))

# Read in MUX PLSR predictions
MUX_preds = read.csv(paste0(getwd(),"/Raw_predictions/MUX_Oct_Nov_2020_predictions_032521.csv"))
MUX_preds$DateTime = ymd_hms(MUX_preds$DateTime, tz="Etc/GMT+4")

# Select the variables we want
catwalk_exp = catwalk %>% select(Reservoir,Site,DateTime,RDO_mgL_5_adjusted,
                                 RDO_mgL_9_adjusted, EXODO_mgL_1,ThermistorTemp_C_surface,
                                 ThermistorTemp_C_1,ThermistorTemp_C_2, ThermistorTemp_C_3,
                                 ThermistorTemp_C_4, ThermistorTemp_C_8, ThermistorTemp_C_9,
                                 ThermistorTemp_C_5, ThermistorTemp_C_6, ThermistorTemp_C_7,
                                 EXOfDOM_QSU_1, EXOSpCond_uScm_1, EXOChla_ugL_1)

# Select the variables we want
met_exp = met %>% select(Reservoir,Site,DateTime,WindSpeed_Average_m_s,ShortwaveRadiationDown_Average_W_m2,
                             Rain_Total_mm)

# Convert DateTime to PosixCT
catwalk_exp$DateTime = mdy_hm(catwalk_exp$DateTime, tz="Etc/GMT+5")
met_exp$DateTime = ymd_hms(met_exp$DateTime, tz="Etc/GMT+5")

# Select the reservoir, site, and date range we want
catwalk_exp = catwalk_exp %>% filter(Reservoir=="FCR" & Site==50) %>%
  filter(DateTime>"2020-10-16 14:10:00") %>%
  filter(DateTime<"2020-11-09 14:00:00")

# Select the reservoir, site, and date range we want
met_exp = met_exp %>% filter(Reservoir=="FCR" & Site==50) %>%
  filter(DateTime>"2020-10-16 13:10:00") %>%
  filter(DateTime<"2020-11-09 14:00:00")

# Select date range for MUX predictions
MUX_preds = MUX_preds %>%
  filter(DateTime>"2020-10-16 13:10:00") %>%
  filter(DateTime<"2020-11-09 14:00:00")

# Smooth time series using moving average
MUX_preds = MUX_preds %>%
  group_by(Depth) %>%
  mutate(TFe_ma10 = rollmean(TFe_mgl,k=10,fill = NA)) %>%
  ungroup(Depth)

# Merge MUX metals predictions dataframe with catwalk dataframe
#combined = merge(MUX_preds,catwalk_exp,by="DateTime",all = TRUE)

surface = MUX_preds %>% filter(Depth == 0.1)
one_m = MUX_preds %>% filter(Depth == 1.6)
three_m = MUX_preds %>% filter(Depth == 3.8)
five_m = MUX_preds %>% filter(Depth == 5.0)
six_m = MUX_preds %>% filter(Depth == 6.2)
eight_m = MUX_preds %>% filter(Depth == 8.0)
nine_m = MUX_preds %>% filter(Depth == 9.0)

# convert temp to long format for plotting
therm_depths = data.frame(depth_m = c(0.1,1:9), depth = c("ThermistorTemp_C_surface",
                                                        paste0("ThermistorTemp_C_",rep(1:9))))
catwalk_exp_long = catwalk_exp %>% pivot_longer(cols=c(7:16),names_to = "depth", 
                                                values_to = "temperature") %>%
  left_join(therm_depths, by = "depth")

# filter temp by depth
catwalk_exp_long = catwalk_exp_long %>% filter(depth_m %in% c(0.1,1,2,3))

# Multi-panel plot

#D39200 = 1.6m
#619CFF = 8m
#FF61C3 = 9m
#00C19F = 6.2m

# vector to add turnover line
turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00")))
colnames(turnover)= c("Date")

TFe_plot = ggplot() +
  geom_path(data=one_m, aes(x=DateTime,y=TFe_mgl), size=1, color="#D39200") +
  labs(x="Date",y="Total Fe (mg/L)") +
    theme(legend.position="right")+
  ggtitle("Predicted Total Fe and Sensor Data at 1.6m") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "#D39200", size=25),
    plot.title = element_text(size = 25)
  ) 

TMn_plot = ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TMn_mgl), size=1, color="red") +
  xlab("Date") +
  ylab("Total Mn (mg/L)") +
  #scale_x_continuous(breaks=ticks) +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "red", size=25)
  ) 

SFe_plot = ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=SFe_mgl), size=1, color="darkgreen") +
  xlab("Date") +
  ylab("Soluble Fe (mg/L)") +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "brown", size=25)
  )

DO_plot = ggplot() +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=EXODO_mgL_1), size=1, color="blue") +
  xlab("Date") +
  ylab("Diss. Oxy. (mg/L)") +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 18),
    axis.text.y.left = element_text(size= 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "blue", size=20),
  ) 

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
  #ylim(c(35,50)) +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "brown", size=25),
  ) 

Temp_plot = ggplot() +
  geom_path(data=catwalk_exp_long, aes(x=DateTime, y=temperature, color = as.factor(depth_m)), size=1) +
  labs(x="Date", y="Temp. (deg C)", color= "Depth (m)")+
  #theme_ipsum() +
  theme(legend.position=c(0.95,0.95))+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=25),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.box.background = element_rect()
  ) 


SW_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=ShortwaveRadiationDown_Average_W_m2), size=1, color=2) +
  xlab("Date") +
  ylab("Shortwave Rad. Down (W/m2)") +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 18),
    axis.text.y.left = element_text(size= 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = 2, size=20),
  ) 







png('MUX_TFe_DO_Temp_fdom_one.png', width = 15, height = 12, units = 'in', res = 300)

TFe_plot / DO_plot / Temp_plot / fdom_plot
 
dev.off()






#### Old Code ####

# Value used to transform the data
coeff_r <- (4)

# A few constants
FeColor <- "#69b3a2"
DOColor <- rgb(0.2, 0.6, 0.9, 1)
TempColor <- rgb(0.2, 0.6, 0.9, 1)

# DO vs. Fe Plot
png('MUX_Fe_DO_post_turnover.png', width = 15, height = 12, units = 'in', res = 300)
DO_plot = ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgl), size=2, color=FeColor) + 
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
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgl), size=2, color=FeColor) + 
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
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgl)) +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=RDO_mgL_9_adjusted), colour="blue") +
  #ylim(0, 0.5)+
  labs(x="Date", y = "TFe_mgl", title = "Predicted TFe") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")
DO_plot


turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00")))
colnames(turnover)= c("Date")

png("One_m_only_033021.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=MUX_one, aes(x=DateTime,y=TFe_mgl, color= as.character(Depth)), size=0.5) +
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

