setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(rLakeAnalyzer)
library(patchwork)

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

area = seq(-350,-1e-1, length.out = length(unique(df$temp_depth_m))) * (-1)
depth = unique(df$temp_depth_m)
bath = data.frame('depths' = depth, 'areas' = area)

df_wide_wind <- merge(df_wide, wind, by = 'datetime')
ln <- ts.lake.number(wtr = df_wide, wnd = wind, wnd.height = height,
                     bathy = bath, seasonal = T)

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

p1 / p2 / p3 +plot_layout(guides = 'collect')

plot(ln$lake.number, df_wide_wind$wind_speed)
plot(df_wide_wind$wtr_0-df_wide_wind$wtr_2, df_wide_wind$wind_speed)
plot(df_wide_wind$wind_speed, df_wide_wind$wtr_0-df_wide_wind$wtr_2)
