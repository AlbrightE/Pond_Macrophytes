setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(tidyverse)
library(rLakeAnalyzer)

wtrF<-load.ts("temp_hf_Fmid_wide.txt") # load temperature data, pond F, reference pond
wtrB<-load.ts("temp_hf_Bmid_wide.txt") # load temperature data, pond B, nutrient addition pond

bathy<-load.bathy("pond_hypsography.txt") # load hypsographic curve, same for both ponds

wnd<-load.ts("wind2_wide.txt") # load wind data 

Ao<-400 # area of ponds
wnd.height<-4 # height of wind sensor above the ponds

# ts.lake.number
lake_no <- ts.lake.number(wtrF, wnd, wnd.height, bathy, seasonal=TRUE)
plot(lake_no, ylab='Lake Number', xlab='Date')

lake_noB<-ts.lake.number(wtrB, wnd, wnd.height, bathy, seasonal=TRUE)
plot(lake_noB, ylab='Lake Number', xlab='Date')

###### lake.number (CODE FROM ROBERT)
# lake.number
df = wtrF 
idx = match(wnd$datetime,df$datetime)
df = df[idx,]

ssi <-ts.schmidt.stability(df, bathy)
plot(ssi, ylab='Schmidt stability', xlab='Date')
zv <- bathy$depths %*% bathy$areas / sum(bathy$areas )

metaDeps <- ts.meta.depths(wtr = df)

epi.temp <- ts.layer.temperature(wtr = df, top = 0, bottom = metaDeps$top, bathy = bathy, na.rm = T)
hypo.temp <- ts.layer.temperature(wtr = df, top = metaDeps$top, bottom = max(bathy$depths), bathy = bathy, na.rm = T)

wnd_shear <- 1.310e-3 * 1.4310e-3 * wnd$wnd^2
wnd_shear <- uStar(wndSpeed = wnd$wnd, wndHeight = wnd.height, 
                   averageEpiDense = water.density(epi.temp$layer.temp))


bthA = bathy$areas
bthD = bathy$depths
uStar = sqrt(wnd_shear)
St = ssi
metaT = metaDeps$top
metaB = metaDeps$bottom
averageHypoDense = water.density(hypo.temp$layer.temp) 
g	<-	9.81
dz	<-	0.1
# if bathymetry has negative values, remove.
# intepolate area and depth to 0
Ao	<-	bthA[1]
Zo	<-	bthD[1]
if (Ao==0){stop('Surface area cannot be zero, check *.bth file')}
#interpolates the bathymetry data
layerD	<-	seq(Zo,max(bthD),dz)
layerA	<-	stats::approx(bthD,bthA,layerD)$y
#find depth to the center of volume
Zv = layerD*layerA*dz                    
Zcv = sum(Zv)/sum(layerA)/dz

St_uC = St$schmidt.stability*Ao/g
# Calculates the Lake Number according to the formula provided
Ln = g*St_uC*(metaT+metaB)/(2*averageHypoDense*uStar^2*Ao^(3/2)*Zcv)

df.ln <- data.frame('datetime' = df$datetime,
                    'lakenumber' = Ln)
ggplot(df.ln) +
  geom_line(aes(datetime, lakenumber))+
  xlab('') + ylab("Lake Number (-)") +
  geom_hline(yintercept=1, linetype = 2) +
  scale_y_continuous(trans='log10') +
  theme_minimal()
