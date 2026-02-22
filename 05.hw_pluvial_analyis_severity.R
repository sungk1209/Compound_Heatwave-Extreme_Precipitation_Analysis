
###########################################################
#  1.filename: 2.hwdr_analyis_severity.R
#  2.Developer: Kyungmin Sung
#  3.Date: Oct.10.2025
#  4.Description: Calculate Heatwave-Drought Severity 
#             
##########################################################
require(viridis)
library(tidyverse)
require(zoo)
require(ggplot2)
require(lubridate)
require(data.table)

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)  # For spatial data handling
require(gridExtra)
require(scales)

select <- dplyr::select

file_path <- "../../data/"
result_path <- "./result/"

#clim_df<- readRDS(paste0(result_path, "/hwpl_clim.rds"))

# Filter out data 

clim_df <- clim_df %>%
  group_by(ID) %>%
  mutate(
    Tmax_percentile = percent_rank(TA) * 100,
    P_percentile = percent_rank(RN) * 100
  ) %>%
  ungroup()

hwp_events <- clim_df %>%
  filter(hw_pluvial == 1) %>%
  select(ID, date, TA, RN,Tmax_percentile, P_percentile, hw_pluvial)

hwp_events <- hwp_events %>%
  left_join(stn_info, by = c("ID" = "STN_ID")) 

hwp_events <- hwp_events %>%
  mutate(year = year(date))

#Cut intervals for heat map

hwp_events <- hwp_events %>%
  mutate(
    Tmax_bin = cut(Tmax_percentile, breaks = c(50, 60, 70, 80, 90, 100), include.lowest = TRUE, right = TRUE),
    P_bin = cut(P_percentile, breaks =  c(50, 60, 70, 80, 90, 100), include.lowest = TRUE, right = TRUE)
  )

# 2. 복합 강도 계산: 각 구간 조합별 평균 복합강도
tile_df <- hwp_events %>%
  filter(!is.na(Tmax_bin), !is.na(P_bin)) %>%
  group_by(P_bin, Tmax_bin) %>%
  summarise(
    compound_intensity = mean((Tmax_percentile * P_percentile) / 100, na.rm = TRUE),  # 정규화된 강도
    .groups = "drop"
  ) %>%
  mutate(label = round(compound_intensity, 2))

ggplot(tile_df, aes(x = Tmax_bin, y = P_bin, fill = compound_intensity)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.5) +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Compound\nIntensity") +
  labs(
    title = "Compound Heatwave–Pluvial Intensity Matrix",
    x = "Tmax Percentile",
    y = "Precip. Percentile"
  ) +
  theme_minimal(base_size = 13)

####create map 
# 1. Group by ID, Lon, Lat, bin indices

map_df <- hwp_events %>%
  filter(!is.na(Tmax_bin), !is.na(P_bin)) %>%
  group_by(ID, Long, Lat, Tmax_bin, P_bin) %>%
  summarise(.groups = "drop")

map_df <- map_df %>%
  left_join(tile_df, by = c("Tmax_bin", "P_bin"))

intensity_limits <- range(tile_df$compound_intensity, na.rm = TRUE)

korea_map <- ne_countries(country = "South Korea", scale = "medium", returnclass = "sf")
korea_states <- ne_states(country = "South Korea", returnclass = "sf")  # Get province-level borders

ggplot() +
  geom_sf(data = korea_states, fill = "gray90", color = "gray60") +
  geom_point(
    data = map_df,
    aes(x = Long, y = Lat, fill = compound_intensity),
    shape = 21, size = 3.5, color = "black"
  ) +
  scale_fill_viridis(
    option = "plasma",
    direction = -1,
    name = "Compound\nIntensity",
    limits = range(tile_df$compound_intensity, na.rm = TRUE)  # ensure consistent scale
  ) +
  labs(
    title = "Spatial Distribution of Compound Heatwave–Pluvial Intensity",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 13)


############      Mapping #### 


