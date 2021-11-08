# Project: Modeling lake mixing dynamics with submersed macrophytes, energy dissipation by biomass
# Last Modified 8 November 2021
# Contributers: Ellen Albright, Robert Ladwig, Grace Wilkinson
# Description: Data cleaning - high frequency temperature profile data

# Data citation: ______________

# The code can be run with the following datasets:
# ...

# Clear environment, set working directory - UPDATE AS NEEDED --------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd('C:/Users/Ellen/Desktop/Box Sync/Albright DISSERTATION/Albright_Chapter_4/DATA/SUBMIT')

# REQUIRED PACKAGES FOR CODE - install as needed
# install.packages("tidyverse")
library(tidyverse)

### OVERVIEW OF SCRIPT ORGANIZATION ### ------------------------------------------------------------------------------------------------------------------------------------
# PART 1 - Data cleaning - Pond F, Deep Site
### ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Read in individual csv's from each temperature logger (each represents a different depth in profile)
# Select columns you want to work with, and give them sensible names. Add columns for pond, site id, and water depth
F_Deep_0<-read.csv("F_Deep_0m.csv")
F_Deep_0<-F_Deep_0 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 
  
F_Deep_0.25<-read.csv("F_Deep_0.25m.csv")
F_Deep_0.25<-F_Deep_0.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_0.5<-read.csv("F_Deep_0.5m.csv")
F_Deep_0.5<-F_Deep_0.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_0.75<-read.csv("F_Deep_0.75m.csv")
F_Deep_0.75<-F_Deep_0.75 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=0.75) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1<-read.csv("F_Deep_1m.csv")
F_Deep_1<-F_Deep_1 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1.25<-read.csv("F_Deep_1.25m.csv")
F_Deep_1.25<-F_Deep_1.25 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1.25) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_1.5<-read.csv("F_Deep_1.5m.csv")
F_Deep_1.5<-F_Deep_1.5 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=1.5) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_Deep_2<-read.csv("F_Deep_2m.csv")
F_Deep_2<-F_Deep_2 %>% 
  rename(datetime=ï..DateTime, doy=DOY, doy_frac=DOY_frac, temp_c=Temp_C) %>% 
  mutate(pond="F", site_id=19, temp_depth_m=2) %>% 
  select(datetime,doy,doy_frac,pond,site_id,temp_depth_m,temp_c) 

F_deep<-rbind(F_Deep_0,F_Deep_0.25,F_Deep_0.5,F_Deep_0.75,F_Deep_1,F_Deep_1.25,F_Deep_1.5,F_Deep_2)
F_deep<-filter(F_deep,doy>"142"&doy<"241") #filter for data range of all sensors in lake

# Quick visualization to make sure everything looks alright - looking good!
ggplot(data=F_deep, aes(x=doy_frac, y=temp_c, group=temp_depth_m))+
  geom_line(aes(color=temp_depth_m),size=1)+
  scale_color_viridis_c(direction = -1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Temperature (C)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.8,0.1),legend.text = element_text(size=14))

# Write one csv for temperature profiles from deep site, pond F
write.csv(F_deep,file="temp_profiles_Fdeep.csv")
