setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(rLakeAnalyzer)
library(patchwork)
library(lubridate)
library(zoo)

get.datetime = function(data, error=FALSE){
  datetime.pattern = "(datetime|timestamp|time|date)"
  
  header = names(data)
  dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)
  
  if(length(dt_indx) < 1){
    if(error){
      stop('Unable to find a datetime column.')
    }else{
      warning('Unable to find a datetime column, attempting to ignore.')
      return(NULL)
    }
  }else if(length(dt_indx) > 1){
    stop('datetime column ambiguity. You can only have one column of datetime.')
  }
  
  return(data[,dt_indx])
}

ts.lake.number.modified <- function(wtr, wnd, wnd.height, bathy, seasonal=TRUE,
                                    biomass){
  
  depths = get.offsets(wtr)
  
  # Make sure data frames match by date/time. 
  all.data = merge(wtr, wnd, by='datetime')
  all.data = merge(all.data, biomass, by = 'datetime')
  
  cols = ncol(all.data)
  wtr = all.data[,c(1:ncol(wtr))]
  wnd = all.data[,c(1, (ncol(wtr)+1))]
  
  n = nrow(wtr)
  l.n = rep(NA, n)
  
  wtr.mat = as.matrix(wtr[,-1])
  dimnames(wtr.mat) <- NULL
  
  for(i in 1:n){
    if(any(is.na(wtr.mat[i,])) || is.na(wnd[i,2])){
      
      next
    }
    
    m.d = meta.depths(wtr.mat[i,], depths, seasonal=seasonal)
    if(any(is.na(m.d))){
      next
    }
    
    epi.dens = layer.density(0, m.d[1], wtr.mat[i,], depths, bathy$areas, bathy$depths)
    hypo.dens = layer.density(m.d[2], max(depths), wtr.mat[i,], depths, bathy$areas, bathy$depths)
    
    
    uS = uStar(wnd[i,2], wnd.height, epi.dens)
    
    St = schmidt.stability(wtr.mat[i,], depths, bathy$areas, bathy$depths)
    
    dz = 0.1
    layerD = seq(min(bathy$depths), max(bathy$depths), by=dz)
    layerAhat <- ifelse(layerD >= biomass$height[i], biomass$height[i],0)
    deps <- as.numeric(sub(".*_", "", colnames(wtr[,-c(1)])))
    layerRho <- approx(x = deps, y = water.density(wtr = wtr.mat[i,]), xout = layerD)$y
    #ifelse(layerD >= biomass$height[i], biomass$dens[i],0)
    DKE = biomass$cd[i] * uS**2 * biomass$area[i]**(3/2) * biomass$height[i] *
      (layerD %*% (layerAhat * layerRho))
      
    
    #thermo.depth <- function(wtr, depths, Smin = 0.1){\
    l.n[i] = lake.number.modified(bathy$areas, bathy$depths, uS, St, m.d[1], m.d[2], hypo.dens, DKE)
    # print(DKE)
  }
  
  output = data.frame(datetime=get.datetime(wtr), lake.number=l.n)
  
  return(output)
}

lake.number.modified <- function(bthA,bthD,uStar,St,metaT,metaB,averageHypoDense, DKE){
  g	<-	9.81
  dz	<-	0.1
  # if bathymetry has negative values, remove.
  # intepolate area and depth to 0
  
  # - implement here -, return proper format NOT DONE
  
  
  Ao	<-	bthA[1]
  Zo	<-	bthD[1]
  if (Ao==0){stop('Surface area cannot be zero, check *.bth file')}
  
  #interpolates the bathymetry data
  layerD	<-	seq(Zo,max(bthD),dz)
  layerA	<-	stats::approx(bthD,bthA,layerD)$y
  
  
  #find depth to the center of volume
  Zv = layerD*layerA*dz                    
  Zcv = sum(Zv)/sum(layerA)/dz
  St_uC = St*Ao/g
  # Calculates the Lake Number according to the formula provided
  pe = g*St_uC*(metaT+metaB)
  tke = (2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv - DKE)
  tke = ifelse(tke <= 0, 1, tke)
  Ln = pe/tke
  # Ln = ifelse(Ln == Inf, pe, Ln)
  # print(paste0(pe, ' ', tke, ' ', DKE, ' ', ln))
  return(Ln)
}

df <- read_csv('../Field_Data_Processing/temp_profiles_Fdeep.csv')
str(df)

df$datetime <- as.POSIXct( df$datetime, format = '%m/%d/%Y %H:%M')
# df$datetime <- as.factor(df$datetime)
df_wide <- df %>% 
  group_by(datetime) %>% 
  arrange(datetime) %>% 
  pivot_wider(id_cols = datetime, names_from  = temp_depth_m, values_from = temp_c) %>%
  rename('wtr_0' = `0`, 'wtr_0.25' = `0.25`, 'wtr_0.5' =`0.5`, 'wtr_0.75' = `0.75`,
         'wtr_1' = `1`, 'wtr_1.25' = `1.25`, 'wtr_1.5' = `1.5`, 'wtr_2' = `2`)
df_wide

wind <- read_csv('../1D_HeatMixing_macrophytes/bc/weather_station_ponds.csv')
height <- mean(wind$wind_z)
wind$date_time <- as.POSIXct( wind$date_time, format = '%m/%d/%Y %H:%M')

wind <- wind %>%
  rename('datetime' = date_time) %>%
  select(datetime, wind_speed)
wind$wind_speed[which(wind$wind_speed == 0)] = 1e-1

area = seq(-350,-1e-1, length.out = length(unique(df$temp_depth_m))) * (-1)
depth = unique(df$temp_depth_m)
bath = data.frame('depths' = depth, 'areas' = area)

df_wide_wind <- merge(df_wide, wind, by = 'datetime')

ln <- ts.lake.number(wtr = df_wide, wnd = wind, wnd.height = height,
                     bathy = bath, seasonal = T)

bm <- read_csv('ModelTest_MacrophyteSummary.csv')

df_bm <- df_wide
df_bm$doy <- yday(df_bm$datetime)
df_bm <- merge(df_bm, bm, by = 'doy')
df_bm$time <- as.Date(df_bm$doy, origin = '2019-12-31 00:00:00')
df_bm <- df_bm %>% 
  mutate(bm_height = water_depth_m - stand_height_m,
         bm_area = approx(depth, area, bm_height)$y,
         ahat = bm_area/(area %*% depth)) %>%
  select(datetime, bm_height, ahat, bm_area, water_volume_m3, biomass_gDW, biomass_gDWperM3,time)

lin.m <- lm(biomass_gDWperM3 ~ bm_height, df_bm)
df_bm$biomass_gDWperM3[which(is.na(df_bm$biomass_gDWperM3))] <- lin.m$coefficients[1] + 
  df_bm$bm_height[which(is.na(df_bm$biomass_gDWperM3))] * lin.m$coefficients[2]

iy =na.omit(!duplicated(df_bm$time))
ix = (match(as.POSIXct(df_bm$time[iy]), df_wide$datetime))

biomass <- data.frame('datetime' = df_wide$datetime,
                      'height' = NA,
                      'area' = NA,
                      'ahat' = NA,
                      'dens' = NA,
                      'cd' = 1.0)
biomass$height[ix[!is.na(ix)]] <- df_bm$bm_height[iy[!is.na(ix)]]
biomass$area[ix[!is.na(ix)]] <- df_bm$bm_area[iy[!is.na(ix)]]
biomass$ahat[ix[!is.na(ix)]] <- df_bm$ahat[iy[!is.na(ix)]]
biomass$dens[ix[!is.na(ix)]] <- df_bm$biomass_gDWperM3[iy[!is.na(ix)]]
biomass <- na.locf(biomass)

isx <- match(biomass$datetime, df_wide$datetime)
ln.modified <- ts.lake.number.modified(wtr = df_wide, wnd = wind, wnd.height = height,
                     bathy = bath, seasonal = T, biomass = biomass)

p1 <- ggplot(ln) +
  geom_line(aes(datetime, lake.number, col = 'Lake Number')) +
  geom_point(aes(datetime, lake.number, col = 'Lake Number')) +
  theme_minimal(); p1
p2 <- ggplot(df_wide_wind) + 
  geom_line(aes(datetime, wtr_0 - wtr_2, col = 'WTR_diff')) +
  geom_point(aes(datetime, wtr_0 - wtr_2, col = 'WTR_diff')) +
  theme_minimal(); p2
p3 <- ggplot(df_wide_wind) + 
  geom_line(aes(datetime, wind_speed, col = 'wind')) +
  geom_point(aes(datetime, wind_speed, col = 'wind')) +
  theme_minimal(); p3
p4 <- ggplot(ln.modified) +
  geom_line(aes(datetime, lake.number, col = 'Lake Number modified')) +
  geom_point(aes(datetime, lake.number, col = 'Lake Number modified')) +
  theme_minimal(); p4

p1 / p2 / p3 / p4 +plot_layout(guides = 'collect')

all.data = merge(df_wide, wind, by='datetime')
all.data = merge(all.data, biomass, by = 'datetime')
all.data = merge(all.data, ln.modified %>% rename(lake.number.modified = lake.number), by = 'datetime')
all.data = merge(all.data, ln , by = 'datetime')

ggplot(all.data) +
  geom_line(aes(datetime, wtr_0 - wtr_2, col = 'WTR_diff')) +
  geom_line(aes(datetime, wind_speed, col = 'wind speed')) +
  geom_line(aes(datetime, lake.number/1000, col = 'Lake Number')) +
  geom_line(aes(datetime, lake.number.modified/1000, col = 'Lake Number modified')) +
  theme_bw()

df <- all.data %>%
  mutate('temp.gradient' = wtr_0 - wtr_2) %>%
  select(datetime, temp.gradient, wind_speed, lake.number, lake.number.modified, area)
m.df <- reshape2::melt(df, id.vars = 'datetime')

(ggplot(df) +
  geom_point(aes(datetime, log10(lake.number.modified), col = temp.gradient)) +
  scale_color_gradient(low = "cyan", high = "darkred", na.value = NA) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() )/
  ggplot(df) +
  geom_point(aes(datetime, log10(lake.number), col = temp.gradient)) +
  scale_color_gradient(low = "cyan", high = "darkred", na.value = NA) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + plot_layout(guides = 'collect')

(ggplot(df) +
    geom_point(aes(datetime,temp.gradient, col =  log10(lake.number.modified))) +
    scale_color_gradient(low = "cyan", high = "darkred", na.value = NA) +
    geom_hline(yintercept=1, linetype="dashed", color = "black") +
    theme_bw() )/
  ggplot(df) +
  geom_point(aes(datetime, temp.gradient, col =  log10(lake.number))) +
  scale_color_gradient(low = "cyan", high = "darkred", na.value = NA) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + plot_layout(guides = 'collect')

(ggplot(df) +
    geom_point(aes(datetime, log10(lake.number.modified), col = area)) +
    scale_color_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
    geom_hline(yintercept=1, linetype="dashed", color = "black") +
    theme_bw() )/
  ggplot(df) +
  geom_point(aes(datetime, log10(lake.number), col = area)) +
  scale_color_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  theme_bw() + plot_layout(guides = 'collect')

(ggplot(df) +
  geom_point(aes(temp.gradient, lake.number.modified, col = area)) +
  scale_color_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
  theme_bw() +
  ggplot(df) +
  geom_point(aes(temp.gradient, lake.number, col = area)) +
  scale_color_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
  theme_bw() ) /
  (ggplot(df) +
     geom_point(aes(temp.gradient, lake.number.modified, col = wind_speed)) +
     scale_color_gradient(low = "yellow", high = "darkred", na.value = NA) +
     theme_bw() +
     ggplot(df) +
     geom_point(aes(temp.gradient, lake.number, col = wind_speed)) +
     scale_color_gradient(low = "yellow", high = "darkred", na.value = NA) +
     theme_bw() ) + plot_layout(guides = 'collect')


ggplot(m.df) +
  geom_line(aes(datetime, value)) +
  facet_wrap(~ variable, scales = 'free', ncol= 1) +
  theme_bw()

ggplot(m.df) +
  geom_density(aes(value)) +
  geom_jitter(aes(x = value,y = 0, col = datetime), height = 0.0001, alpha = 0.05)+
  facet_wrap(~ variable, scales = 'free', ncol= 1) +
  scale_color_gradient(low = "yellow", high = "darkgreen", na.value = NA) +
  theme_bw()
