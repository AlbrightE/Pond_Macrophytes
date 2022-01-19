# Project: "Macrophytes alter thermal stratification and dissolved oxygen dynamics in shallow ponds"
#           Part of ISU Hort Farm Pond Study, 2020
# Last Modified 18 January 2022
# Contributors: Ellen Albright, Robert Ladwig, Grace Wilkinson
# Description: Data exploration and non-modeling analyses

# Data citation: ______________

# The code can be run with the following datasets:
# "biomass_canopy_2020.csv" - Aquatic macrophyte biomass and canopy height, experimental ponds B and F, May-August 2020 
#      Biomass, canopy height, and species composition for submersed aquatic macrophytes across in two experimental ponds
# "temp_profiles_2020.csv" - High frequency temperature profiles across experimental ponds B and F, May-August 2020 


# Clear environment, set working directory - UPDATE AS NEEDED --------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd('C:/Users/Ellen/Desktop/Box Sync/Albright DISSERTATION/Albright_Chapter_4/DATA/SUBMIT')

# REQUIRED PACKAGES FOR CODE - install as needed
# install.packages("tidyverse")
# install.packages("rLakeAnalyzer")
# install.packages("reshape")

library(tidyverse) #majority of figures made in ggplot2, will need this package to recreate figures
library(rLakeAnalyzer)
library(reshape)
library(viridis)

### OVERVIEW OF SCRIPT ORGANIZATION ### ------------------------------------------------------------------------------------------------------------------------------------
# PART 1: Macrophyte biomass and canopy height over the growing season
# PART 2: Thermal stratification and mixing patterns over the summer 
# PART n: Hypsography - estimate depth-area curve and compare to "by hand" hypsographic curve

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


### PART 1 - MACROPHYTE BIOMASS AND CANOPY HEIGHT OVER THE GROWING SEASON (FIGURES ____) ----------------------------------------------------------------------------------
# READ IN DATA: Aquatic macrophyte biomass and canopy height, experimental ponds B and F, May-August 2020 

bio<-read.csv("biomass_canopy_2020.csv")
# subset to work with canopy and biomass data separately (different spatial resolution)
biomass<-subset(bio, site_type == "Biomass")
biomass<-biomass %>% 
  mutate(doy.f=factor(doy))

biomass_outer<-subset(biomass, site_id!="F5" & site_id!="F8" & site_id!="F11" & site_id!="F14" & site_id!="B5" & site_id!="B8"& site_id!="B11" & site_id!="B14")
biomass_inner<-subset(biomass, site_id=="F5" | site_id=="F8" | site_id=="F11" | site_id=="F14" | site_id=="B5" | site_id=="B8"| site_id=="B11" | site_id=="B14")

canopy<-subset(bio,site_type == "Canopy")
canopy<-canopy %>% 
  mutate(canopy_per=(canopy_m/depth_m)*100) #calculate the percentage of the water column (depth_m) filled with plants (canopy_m)

# FIGURE - paired boxplots of biomass (by pond, DOY), visualize trends and divergence in biomass
ggplot(biomass, aes(x=doy.f, y=biomass_g, fill=pond))+
  geom_boxplot()+
  scale_fill_manual("Pond",values=c("#fee090","#abd9e9"))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Macrophyte Biomass (g DW)")+
  theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.position = c(0.1,0.9),legend.text = element_text(size=11))
ggsave("Biomass Boxplot.png",width=6.5, height=3.3, units="in", dpi=300)


ggplot(biomass_outer, aes(x=doy.f, y=biomass_g, fill=pond))+
  geom_boxplot()+
  scale_fill_manual("Pond",values=c("#fee090","#abd9e9"))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Macrophyte Biomass (g DW)")+
  theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.position = c(0.1,0.9),legend.text = element_text(size=11))
ggsave("Biomass Outer Boxplot.png",width=6.5, height=3.3, units="in", dpi=300)

ggplot(biomass_inner, aes(x=doy.f, y=biomass_g, fill=pond))+
  geom_boxplot()+
  scale_fill_manual("Pond",values=c("#fee090","#abd9e9"))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Macrophyte Biomass (g DW)")+
  theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.position = c(0.1,0.9),legend.text = element_text(size=11))
ggsave("Biomass Inner Boxplot.png",width=6.5, height=3.3, units="in", dpi=300)


  
# Density plot to look at overall biomass distribution
ggplot(biomass, aes(x=biomass_g, fill=pond)) + geom_density(alpha=.3) +   
  scale_fill_manual("Pond",values=c("#fee090","#abd9e9"))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(biomass_outer, aes(x=biomass_g, fill=pond)) + geom_density(alpha=.3) +   
  scale_fill_manual("Pond",values=c("#fee090","#abd9e9"))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(biomass_inner, aes(x=biomass_g, fill=pond)) + geom_density(alpha=.3) +   
  scale_fill_manual("Pond",values=c("#fee090","#abd9e9"))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())


# FIGURE - line plot of canopy height over time
ggplot(data=canopy, aes(x=doy, y=canopy_m, group=site_id))+
  geom_line(aes(color=site_id),size=1.5)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#d73027","#f46d43","#fdae61","#313695","#4575b4","#74add1"))+
  xlab("Day of Year") + ylab("Canopy Height (m)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.3,0.3),legend.text = element_text(size=14))
ggsave("canopy.png",width=7.5, height=5, units="in", dpi=300)

ggplot(data=canopy, aes(x=doy, y=canopy_per, group=site_id))+
  geom_line(aes(color=site_id),size=1.5)+
  geom_hline(yintercept=50,color="gray40",linetype="solid",size=1.1)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#d73027","#f46d43","#fdae61","#313695","#4575b4","#74add1"))+
  xlab("Day of Year") + ylab("Canopy Height (% Water Depth)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))+
  theme(legend.title=element_blank(), legend.position = c(0.3,0.3),legend.text = element_text(size=14))
ggsave("canopy_per.png",width=7.5, height=5, units="in", dpi=300)


### PART 2 - THERMAL STRATIFICATION AND MIXING PATTERNS OVER THE SUMMER (FIGURES ____) ------------------------------------------------------------------------------------
# Read in all high frequency temperature chain data
temp<-read.csv("temp_profiles_2020.csv", as.is=T)

# Select only Pond F, deep site to work with first
Fdeep<-subset(temp, pond=="F" & site_id=="19")
#doy_frac, temp_depth_m, temp_c

#Grab variables we need
Fdeeper<- Fdeep[c("doy_frac", "temp_depth_m", "temp_c")]

#Melt so that cast will recognize the data frame
Fmelt<- melt(Fdeeper, id=c("doy_frac", "temp_depth_m"))

#Now cast so that we have columns of temp for each depth
Fcast<- cast(Fmelt, doy_frac~temp_depth_m)

#create dataframe (definitely easier way to do this but it works)
Fcasttwo<-data.frame(Fcast)

#add whatever depths you want to (as the numeric value here)
Fcasttwo$X1.75<- 0

#calculate however you need to
Fcasttwo$X1.75<- rowMeans(Fcasttwo[c("X1.5","X2")],)

#now remelt to long format
Ffinal<- melt(Fcasttwo, id=c("doy_frac"))

#I lost your column names
#Grab just the numbers from this character
Ffinal$temp_depth_m <- substr(Ffinal$variable, 2, 9)

#And convert to numeric
Ffinal$temp_depth_m <- as.numeric(Ffinal$temp_depth_m)

#this is numeric too
Ffinal$temp_c<- as.numeric(Ffinal$value)

View(Ffinal)

ggplot(Ffinal, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))

# Now let's do Pond B, deep site ------------------------------------------------------------------------------------------------------------------------
Bdeep<-subset(temp, pond=="B" & site_id=="19")
#doy_frac, temp_depth_m, temp_c

#Grab variables we need
Bdeeper<- Bdeep[c("doy_frac", "temp_depth_m", "temp_c")]

#Melt so that cast will recognize the data frame
Bmelt<- melt(Bdeeper, id=c("doy_frac", "temp_depth_m"))

#Now cast so that we have columns of temp for each depth
Bcast<- cast(Bmelt, doy_frac~temp_depth_m)

#create dataframe (definitely easier way to do this but it works)
Bcasttwo<-data.frame(Bcast)

#add whatever depths you want to (as the numeric value here)
Bcasttwo$X1.75<- 0
Bcasttwo$X1<- 0

#calculate however you need to
Bcasttwo$X1.75<- rowMeans(Bcasttwo[c("X1.5","X2")],)
Bcasttwo$X1<- rowMeans(Bcasttwo[c("X0.75","X1.25")],)


#now remelt to long format
Bfinal<- melt(Bcasttwo, id=c("doy_frac"))

#I lost your column names
#Grab just the numbers from this character
Bfinal$temp_depth_m <- substr(Bfinal$variable, 2, 9)

#And convert to numeric
Bfinal$temp_depth_m <- as.numeric(Bfinal$temp_depth_m)

#this is numeric too
Bfinal$temp_c<- as.numeric(Bfinal$value)

View(Bfinal)


ggplot(Bfinal, aes(doy_frac, temp_depth_m, fill= temp_c)) + 
  geom_tile()+xlim(143,233)+
  scale_fill_viridis(option="C",name="Temperature (C)")+
  scale_y_reverse()+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Depth (m)")+theme(axis.text=element_text(color="black",size=12),legend.position="none",axis.title=element_text(size=14))






### SSI CALCULATIONS - LAKE ANALYZER -----------------------------------------------------------------------------------------------------------------------

#' @title Calculate the Schmidt stability
#' 
#' @description Schmidt stability, or the resistance to mechanical mixing due to the
#' potential energy inherent in the stratification of the water column.
#' 
#' Schmidt stability was first defined by Schmidt (1928) and later modified by
#' Hutchinson (1957). This stability index was formalized by Idso (1973) to
#' reduce the effects of lake volume on the calculation (resulting in a mixing
#' energy requirement per unit area).
#' 
#' @param wtr a numeric vector of water temperature in degrees C
#' @param depths a numeric vector corresponding to the depths (in m) of the wtr
#' measurements
#' @param bthA a numeric vector of cross sectional areas (m^2) corresponding to
#' bthD depths
#' @param bthD a numeric vector of depths (m) which correspond to areal
#' measures in bthA
#' @param sal a numeric vector of salinity in Practical Salinity Scale units
#' @return a numeric vector of Schmidt stability (J/m^2)
#' @seealso \code{\link{ts.schmidt.stability}} \code{\link{lake.number}}
#' \code{\link{wedderburn.number}}
#' @references Schmidt, W., 1928. \emph{Ueber Temperatur and
#' Stabilitaetsverhaltnisse von Seen}. Geo- graphiska Annaler 10, 145-177.
#' 
#' Hutchinson, G.E., 1957. \emph{A Treatise on Limnology}, vol. 1. John Wiley &
#' Sons, Inc., New York.
#' 
#' Idso, S.B., 1973. \emph{On the concept of lake stability}. Limnology and
#' Oceanography 18, 681-683.
#' @keywords arith
#' @examples
#' 
#' 
#' 	bthA	<-	c(1000,900,864,820,200,10)
#' 	bthD	<-	c(0,2.3,2.5,4.2,5.8,7)
#' 	
#' 	wtr	<-	c(28,27,26.4,26,25.4,24,23.3)
#' 	depths	<-	c(0,1,2,3,4,5,6)
#' 	
#' 	cat('Schmidt stability for input is: ')
#' 	cat(schmidt.stability(wtr, depths, bthA, bthD))
#' @export

# Pond F, deep site ----------------------------------------------------------------
wtr<-load.ts("temp_hf_Bdeep_wide.txt")
bathy<-load.bathy("pond_hypsography.txt")

ts_SSI<-ts.schmidt.stability(wtr,bathy,na.rm=FALSE)
SSI<-as.data.frame(ts_SSI)
plot(schmidt.stability~datetime,data=SSI)
range(SSI$schmidt.stability,na.rm=T)

# will need lubridate for this
SSI$doy<-yday(SSI$datetime)
SSI$doy_frac<-hour(SSI$datetime)
SSI$min<-minute(SSI$datetime)
SSI$minfrac[SSI$min=="30"] <- 0.5
SSI$minfrac[SSI$min=="0"] <- 0
SSI$hourfrac<-(SSI$doy_frac + SSI$minfrac)/24

SSI$doy_frac<-SSI$doy+SSI$hourfrac
SSI<-SSI %>% 
  select(datetime,doy,doy_frac,schmidt.stability)

SSI_daily<-SSI %>% 
  group_by(doy) %>% 
  summarize(mean_schmidt.stability=mean(schmidt.stability))

#plot daily means
ggplot(data=SSI_daily, aes(x=doy, y=mean_schmidt.stability))+
  geom_line(size=1)+xlim(143,233)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Daily Mean SSI (J/m2)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))
ggsave("mean_SSI.png",width=6.5, height=3.3, units="in", dpi=300)  

#plot sub-daily SSI
ggplot(data=SSI, aes(x=doy_frac, y=schmidt.stability))+
  geom_line(size=1)+xlim(143,233)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Schmidt's Stability Index (J/m2)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))
ggsave("raw_SSI.png",width=6.5, height=3.3, units="in", dpi=300)  

#focus in on derecho-day
ggplot(data=SSI, aes(x=doy_frac, y=schmidt.stability))+
  geom_line(size=1)+xlim(220,225)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Day of Year") + ylab("Schmidt's Stability Index (J/m2)")+theme(axis.text=element_text(color="black",size=12),axis.title=element_text(size=14))
ggsave("derecho_SSI.png",width=3, height=3, units="in", dpi=300)  

# plot SSI as a function of canopy height 
SSI_canopy<-subset(SSI_daily,doy==143|doy==150|doy==157|doy==166|doy==171|doy==178|doy==186|doy==192|doy==199|doy==206|doy==213|doy==220|doy==228)
F19<-subset(canopy, site_id=="F19")
B19<-subset(canopy, site_id=="B19")

F19_SSI<-subset(F19, doy!=129&doy!=234)
B19_SSI<-subset(B19, doy!=129&doy!=234)

plot(SSI_canopy$mean_schmidt.stability~B19_SSI$canopy_per,pch=16,cex=1.5,cex.axis=1.5)
View(F19)



### PART n - HYPSOGRAPHY --------------------------------------------------------------------------------------------------------------------------------------------
# Code from rLakeAnalyzer (Robert Ladwig's GitHub. Contributers - Jem Stachelek, Luke Winslow, Sam Albers)
# Estimate hypsography curve
# Estimates a depth-area curve for a lake using lake surface area,maximum depth and mean depth. 
# Two methods for estimating the curve are available:
#    1. "cone" assumes the lake is shaped as a cone and requires only surface area and maximum depth
#    2. "voldev" uses the volume development (Vd) parameter from Hakanson (1981) and Johansson et al. (2007)
#          Vd is a dimensionless parameter that describes lake basin shape in relation to the volume of cone whose base area and height equal the surface area and maximum lake depth, 
#          Estimated as Vd = Zmean/Zmax (Hakanson et al. 2000). 
#'         Method "voldev' requires lake surface area, mean and maximum depth. Depths at which the area is estimated can be set by as a numeric vector or as a regularly spaced sequence.
#' 
#' @param Zmax a single value of the maxiumum depth of the lake (in m)
#' @param Zmean a single value of the mean depth of the lake (in m)
#' @param lkeArea a single value of the surface area of the lake (in m^2)
#' @param depths a numeric vector of depths (in m) at which areas are estimated. 
#' If not specified depths is regularly spaced sequence of values with the interval set by zinterval. 
#' @param zinterval  a single value defining the depth interval at which volumes should be calculated, default is 1 m.
#' @param method specifies the method used to estimate depth-area relationship, can be "cone"(default) or "voldev". Method "voldev" requires Zmean. See notes for details.
#' 
#' @return a dataframe which defines the lake area for each depth. Columns are depths (m) and Area.at.z (m^2). Area at 0 m should equal the user entered lkeArea.
#' 
#' @references Hakanson, L. (1981). On lake bottom dynamics - the energy- topography factor. Canadian Journal of Earth Sciences, 18, 899-909.
#' Johansson, H., A. A. Brolin, and L. Hakanson. 2007. New approaches to the modelling of lake basin morphometry. Environ. Model. Assess. 12: 213-228.
#'
#' @examples  
#' Voldev.ex = approx.bathy(Zmax = 25, Zmean = 12, lkeArea = 39400000, method = "voldev")
#' Voldevshallow.ex = approx.bathy(Zmax = 25, Zmean = 6, lkeArea = 39400000, method = "voldev")
#' Cone.ex = approx.bathy(Zmax = 25, lkeArea = 39400000, method = "cone")
#' 
#'# plot depth-area curves
#'   plot(Cone.ex$depths ~ Cone.ex$Area.at.z, xlab = "Area (m^3)", ylab = "Depth (m)", 
#'    ylim = rev(range(Cone.ex$depths)))
#'   points(Voldev.ex$depths ~ Voldev.ex$Area.at.z, col = "red", 
#'    ylim = rev(range(Voldev.ex$depths)))
#'   points(Voldevshallow.ex$depths ~ Voldevshallow.ex$Area.at.z, col = "blue", 
#'    ylim = rev(range(Voldevshallow.ex$depths)))
#'  
#' 
#' @export

approx.bathy <- function(Zmax, lkeArea, Zmean = NULL, method = "cone", zinterval = 1, depths = seq(0, Zmax, by = zinterval)){
  Area = c()
  if(method == "cone"){
    radius = sqrt(lkeArea/pi)
    radii = stats::approx(c(0, Zmax), c(radius, 0), depths)$y
    Area = data.frame(depths = depths, Area.at.z = pi*radii^2)
  } #end of "cone"
  
  if(method == "voldev"){
    if(is.null(Zmean)){
      stop("Zmean required for method 'vd'. Use method 'cone' if Zmean is unknown")
    }
    Zrel = depths/Zmax
    Vd = 3*(Zmean/Zmax) 
    fVd = (1.7/Vd) + 2.5 - 2.4*Vd +0.23*Vd^3 # derived from Hakanson, L. (1981) as cited in Johansson et al. 2007
    for (i in 1:length(Zrel)){
      Area.at.z = lkeArea*( (1-Zrel[i]) * (1 + Zrel[i]*sin(sqrt(Zrel[i])) ) )^fVd
      tmp = data.frame(depths = depths[i], Area.at.z = Area.at.z)
      Area = rbind(Area, tmp)
    }# end of for loop
  } #end of "vd"
  return(Area)
}#end of function

# Now using the function for the hort farm ponds. Using volume development methods because ponds are not cone shaped
Voldev.ex<-approx.bathy(Zmax = 2, Zmean = 0.8, lkeArea = 400, method = "voldev", zinterval=0.25)
plot(Voldev.ex$depths ~ Voldev.ex$Area.at.z, col = "red", 
       ylim = rev(range(Voldev.ex$depths)),ylab="Depth (m)",xlab="Area (m2)")
points(pond_hypo$depths ~ pond_hypo$areas, col="blue")
pond_hypo<-read.csv("pond_hyposography.csv")

