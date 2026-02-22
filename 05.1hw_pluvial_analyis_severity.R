
###########################################################
#  1.filename: 2.hwdr_analyis_severity.R
#  2.Developer: Kyungmin Sung
#  3.Date: Oct.10.2025
#  4.Description: Calculate Heatwave-Drought Severity 
#             
##########################################################

library(tidyverse)
require(zoo)
require(ggplot2)
require(lubridate)
require(data.table)
require(slider)

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)  # For spatial data handling
require(gridExtra)
require(scales)
require(ggpmisc)

select <- dplyr::select

file_path <- "../../data/"
result_path <- "./result/"


clim_df <- clim_df %>%
  mutate(
    Tmax_percentile = percent_rank(TA) * 100,
    P_percentile    = percent_rank(RN) * 100
  ) %>%
  ungroup()

hw_df <- clim_df %>%
  filter(hw_pluvial== 1) %>%
  select(ID, date, Tmax_percentile)

severity_list <- list()

station_list <- unique(clim_df$ID)

for(st in station_list){
  
  df_st <- clim_df %>% filter(ID == st)
  
  # 7일 window에서 P_percentile 최대값 계산
  df_st <- df_st %>%
    mutate(
      Pmax_7day = slide_dbl(
        P_percentile,
        .f = ~ max(.x, na.rm = TRUE),
        .before = 0,   # 당일 포함
        .after  = 7,   # 이후 7일 포함
        .complete = FALSE
      )
    )
  
  # heatwave day에 대해 intensity 계산
  sev_st <- df_st %>%
    filter(hw_pluvial == 1) %>%
    mutate(intensity = Tmax_percentile * Pmax_7day / 100) %>%
    select(ID, date, Tmax_percentile, Pmax_7day, intensity)
  
  severity_list[[as.character(st)]] <- sev_st
}

hw_severity <- bind_rows(severity_list)

hw_severity <- hw_severity %>%
  left_join(stn_info, by = c("ID" = "STN_ID"))

hw_severity_season <- hw_severity %>%
  mutate(
    month = month(date),
    # 계절 정의 (DJF: 12, 1, 2 / MAM: 3, 4, 5 / JJA: 6, 7, 8 / SON: 9, 10, 11)
    season = case_when(
      month %in% c(12, 1, 2) ~ "DJF",
      month %in% c(3, 4, 5) ~ "MAM",
      month %in% c(6, 7, 8) ~ "JJA",
      TRUE ~ "SON"
    )
  )

sev_median <- hw_severity_season %>%
  group_by(ID, Long, Lat, season) %>%
  filter(intensity > 0) %>%
  summarise(mean_intensity = mean(intensity, na.rm = TRUE))

sev_max <- hw_severity_season %>%
  group_by(ID, Long, Lat, season) %>%
  filter(intensity > 0) %>%
  summarise(max_intensity = max(intensity, na.rm = TRUE))

sev_sum <- hw_severity_season %>%
  group_by(ID, Long, Lat, season) %>%
  filter(intensity > 0) %>%
  summarise(sum_intensity = sum(intensity, na.rm = TRUE),
            freq = n())

common_colors <- c(
  "#ffffcc", "#fed976", "#fd8d3c", 
  "#f03b20", "#bd0026", "#800080", "#002147"
)

severity_breaks <- c(0, 20, 40, 60, 80, 90, 100)

p_median <- ggplot() +
  geom_sf(data = korea_states, fill = "gray95", color = "gray70", linewidth = 0.3) +
  geom_point(
    data = sev_median ,
    aes(x = Long, y = Lat, fill = mean_intensity, size = mean_intensity),
    shape = 21, stroke = 0.3, color = "black"
  ) +
  scale_fill_stepsn(
    colours = common_colors,
    breaks  = severity_breaks,
    limits  = c(0, 100),
    name    = "Mean\nCompound\nIntensity"
  ) +
  facet_wrap(~season, ncol = 4) +
  scale_size_continuous(range = c(0.5, 3), guide = "none") +
  labs(title = "Mean Severity of Compound Heatwave–Pluvial Events (1986-2025)") +
  theme_minimal(base_size = 12)

p_median 

ggsave(p_median,filename = paste0(result_path,"/severity_mean.png"),
       width = 8.6,height = 4.3, units = "in", dpi = 300)

p_max <- ggplot() +
  geom_sf(data = korea_states, fill = "gray95", color = "gray70", linewidth = 0.3) +
  geom_point(
    data = sev_max,
    aes(x = Long, y = Lat, fill = max_intensity, size = max_intensity),
    shape = 21, stroke = 0.3, color = "black"
  ) +
  scale_fill_stepsn(
    colours = common_colors,
    breaks  = severity_breaks,
    limits  = c(0, 100),
    name    = "Max\nCompound\nIntensity"
  ) +
  facet_wrap(~season) +
  scale_size_continuous(range = c(1.5, 5), guide = "none") +
  labs(title = "Maximum Severity of Compound Heatwave–Pluvial Events") +
  theme_minimal(base_size = 12)
p_max

p_sum <- ggplot() +
  geom_sf(data = korea_states, fill = "gray95", color = "gray70", linewidth = 0.3) +
  geom_point(
    data = sev_sum,
    aes(x = Long, y = Lat, fill = sum_intensity, size = sum_intensity),
    shape = 21, stroke = 0.3, color = "black"
  ) +
  scale_fill_viridis(rev()) +
  # scale_fill_stepsn(
  #   colours = common_colors,
  #   breaks  = severity_breaks,
  #   limits  = c(0, 200), 
  #   name    = "Cumulative\nIntensity"
  # ) +
  facet_wrap(~season) +
  scale_size_continuous(range = c(1.5, 5), guide = "none") +
  labs(title = "Cumulative Severity (Frequency × Severity)") +
  theme_minimal(base_size = 12)
p_sum
###################==================
# Base VS Recent

common_colors <- c(
  "#ffffcc", "#fed976", "#fd8d3c", 
  "#bd0026", "#800080", "#002147"
)

severity_breaks <- c(0, 40, 60, 80, 90, 100)

sev_mean_base <- hw_severity_season %>%
  filter(year(date) < 2024) %>%
  filter(intensity > 0) %>%
  group_by(ID, season) %>%
  summarise(base_mean = mean(intensity,na.rm = TRUE)) 

sev_mean_base <- sev_mean_base %>%
left_join(stn_info, by = c("ID" = "STN_ID"))

p_mean_base <- ggplot() +
  geom_sf(data = korea_states, fill = "gray95", color = "gray70", linewidth = 0.3) +
  geom_point(
    data = sev_mean_base,
    aes(x = Long, y = Lat, fill = base_mean, size = base_mean),
    shape = 21, stroke = 0.3, color = "black"
  ) +
  scale_fill_stepsn(
    colours = common_colors,
    breaks  = severity_breaks,
    limits  = c(0, 100),
    name    = "Intensity"
  ) +
  facet_wrap(~season, ncol = 4) +
  scale_size_continuous(range = c(0.7, 3), guide = "none") +
  labs(title = "Compound Heatwave-Pluvial Intensity(1986-2023)") +
  theme_minimal(base_size = 12)
p_mean_base

ggsave(p_mean_base,filename = paste0(result_path,"/severity_mean_base.png"),
       width = 8.6,height = 4.3, units = "in", dpi = 300)

sev_mean_recent <- hw_severity_season %>%
  filter(year(date) >= 2024) %>%
  filter(intensity > 0) %>%
  group_by(ID, season) %>%
  summarise(recent_mean = mean(intensity,na.rm = TRUE)) 

 sev_mean_recent <- sev_mean_recent %>%
   left_join(stn_info, by = c("ID" = "STN_ID"))

p_mean_recent <- ggplot() +
  geom_sf(data = korea_states, fill = "gray95", color = "gray70", linewidth = 0.3) +
  geom_point(
    data = sev_mean_recent,
    aes(x = Long, y = Lat, fill = recent_mean, size = recent_mean),
    shape = 21, stroke = 0.3, color = "black"
  ) +
  scale_fill_stepsn(
    colours = common_colors,
    breaks  = severity_breaks,
    limits  = c(0, 100),
    name    = "Intensity"
  ) +
  facet_wrap(~season, ncol = 4) +
  scale_size_continuous(range = c(0.7, 3), guide = "none") +
  labs(title = "Compound Heatwave-Pluvial Intensity(2024-2025)") +
  theme_minimal(base_size = 12)
p_mean_recent

ggsave(p_mean_recent,filename = paste0(result_path,"/severity_mean_recent.png"),
       width = 8.6,height = 4.3, units = "in", dpi = 300)

# sev_mean_recent <- sev_mean_recent %>%
#   left_join(stn_info, by = c("ID" = "STN_ID"))

sev_anom <- sev_mean_recent %>%
  left_join(sev_mean_base, by=c("ID","Long","Lat","season")) %>%
  mutate(anomaly = recent_mean - base_mean)

mycol <- rev(brewer.pal(11, "RdBu"))

p_anomaly <- ggplot() +
  geom_sf(data = korea_states, fill = "gray95", color = "gray70", linewidth = 0.3) +
  geom_point(
    data = sev_anom,
    aes(x = Long, y = Lat, fill = anomaly, size = anomaly),
    shape = 21, stroke = 0.3, color = "black"
  ) +
  scale_fill_stepsn(
    colours = mycol,
    breaks  = c(-15,-10,-5,0,5,10,15),
    limits  = c(-15, 15),
    name    = "Anomaly"
  ) +
  scale_size_continuous(range = c(0.7, 3), guide = "none") +
  facet_wrap(~season, ncol = 4) +
  labs(title = "Regional Anomaly of Heatwave-Pluvial Intensity",
       subtitle = "Difference in Mean Event Intensity:(2024-2025) minus(1986-2023)") +
  theme_minimal(base_size = 12)
p_anomaly

ggsave(p_anomaly,filename = paste0(result_path,"/anomaly.png"),
       width = 8.6,height = 4.3, units = "in", dpi = 300)

freq_sev_df <- sev_sum %>%
  select(ID, freq, sum_intensity, Long, Lat, season) %>%
  left_join(sev_median, by=c("ID", "Long","Lat", "season"))


library(mgcv)

p_non_linear <- ggplot(freq_sev_df, aes(x = freq, y = mean_intensity)) +
  
  geom_smooth(method = "loess", 
              se = TRUE, linewidth = 1.2, alpha = 0.2, color = "grey30") +
  
  # 2. 데이터 포인트: 계절별 색상 적용
  geom_point(aes(color = season), alpha = 0.7, size = 2.5) +
  
  labs(
    x = "Frequency (Number of Events)",
    y = "Mean Severity (Intensity Score)",
    title = "Non-linear Frequency–Severity Relationship by Season (GAM)",
    subtitle = "Analysis of the relationship between event frequency and mean intensity at station level."
  ) +
  # 4. 
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray30", hjust = 0.5),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

p_non_linear

ggsave(p_non_linear,filename = paste0(result_path,"/sev_intensity.png"),
       width = 5,height = 4.3, units = "in", dpi = 300)

########################
# time series 
total_sev_ts <- hw_severity_season %>%
  mutate(year = year(date)) 

total_sev_ts <- total_sev_ts %>%
  mutate(
    year_season = case_when(
      month == 12 ~ year + 1,
      TRUE ~ year
    )
  )

total_sev_ts <- total_sev_ts %>%
  group_by(year_season, season) %>% 
  summarise(mean_intensity = median(intensity, na.rm = TRUE), .groups = "drop")

  
station_annual_mean_intensity <- hw_severity_season %>%
  mutate(year = year(date)) 
  
station_annual_mean_intensity <- station_annual_mean_intensity %>%
  mutate(
    year_season = case_when(
      month == 12 ~ year + 1,
      TRUE ~ year
    )
  )

station_annual_mean_intensity <- station_annual_mean_intensity %>%
  group_by(ID, year_season, season) %>%
  summarise(
    annual_mean_intensity = median(intensity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(annual_mean_intensity))


# --- 2. 개선된 시계열 트렌드 그래프 생성 (모든 지점 트렌드 추가) ---
p_ts_trend_with_all_stations <- ggplot(total_sev_ts, aes(x = year_season, y = mean_intensity)) +
  
  geom_point(data = station_annual_mean_intensity, 
            aes(x = year_season, y = annual_mean_intensity, group = ID),
            color = "gray80", linewidth = 0.3, alpha = 0.15) + 
  # linear trend line
  geom_smooth(method = "lm", formula = y ~ x, 
              color = "black", linewidth = 1.0, se = TRUE, alpha = 0.4) +
  
  # Non linear trend line
  geom_smooth(method = "loess", formula = y ~ x, 
              color = "#377eb8", linewidth = 1.0, se = FALSE, linetype = "dashed") +
  
  # data line & point
  geom_line(color = "#b2182b", linewidth = 1.2) +
  geom_point(color = "#b2182b", size = 2.0, shape = 21, fill = "white", stroke = 1.0) +
  
  # mark 2024-2025
  geom_point(data = total_sev_ts %>% filter(year_season >= 2024), 
             aes(x = year_season, y = mean_intensity),
             color = "#fdbf6f", size = 3.5, shape = 21, fill = "#d95f02", stroke = 1.2) +
  
  #  P-value 
  # stat_poly_eq(aes(label = after_stat(
  #   paste(eq.label, rr.label, p.value.label, sep = "*\", \"*"))),
  #   formula = y ~ x, parse = TRUE, size = 3.5, 
  #   vjust = 1, hjust = 0, family = "sans"
  # ) +
  
 facet_wrap(~season, scales = "free_y", ncol = 2) +
  
  labs(
    title = "Annual Total Compound Heatwave–Pluvial Severity Trend \n with Station-level Variability",
   # subtitle = "Aggregated Mean Intensity (Red), Linear Trend (Black), Loess Smoothing (Blue Dashed), and Individual Station Trends (Thin Gray)",
    x = "Year",
    y = "Mean Intensity (Across all stations)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1, "lines"),
    legend.position = "none"
  )

p_ts_trend_with_all_stations
ggsave(paste0(result_path, "/TS_intensity_median.png"),
       plot = p_ts_trend_with_all_stations, width = 7.2, height = 5.2, units = "in", dpi = 400)


mk_results <- total_sev_ts %>%
  group_by(season) %>%
  summarise(
    # Mann-Kendall 
    mk_test = list(trend::mk.test(mean_intensity)),
    .groups = 'drop'
  ) %>%
  #  P-value & Trend Direction (Tau) 추출
  mutate(
    p_value = sapply(mk_test, function(x) x$p.value),
    tau = sapply(mk_test, function(x) x$estimates["tau"]),
    statistic_S = sapply(mk_test, function(x) x$statistic),
    # 유의성 판단 (일반적으로 유의수준 alpha=0.05 기준)
    significance = ifelse(p_value < 0.001, "***",
                          ifelse(p_value < 0.01, "**",
                                 ifelse(p_value < 0.05, "*", "ns")))
  ) %>%
  select(-mk_test) # list 형태의 검정 결과 제거

# 결과 출력
print(mk_results)

#############################################################
cphw_mean <- cphw_seasonal_trend %>%
  group_by(ID, season) %>%
  summarise(cdh_ave = mean(cdh_count, na.rm = TRUE), .groups ="drop")

cphw_intensity <- station_annual_mean_intensity %>%
  group_by(ID, season) %>%
  summarise(intensity_ave = mean(annual_mean_intensity, na.rm = TRUE), .groups ="drop")

plot_df <- cphw_intensity %>%
  left_join(cphw_median, by = c("ID", "season")) %>%
  left_join(stn_info, by = c("ID" = "STN_ID"))

common_colors <- c(
  "#ffffcc", "#fed976", "#fd8d3c",
  "#f03b20", "#bd0026", "#800080", "#002147"
)


#freq_breaks <- c(0, 5, 10, 20, 30, 40, 50)

freq_breaks <- c(0, 1, 2, 3, 4, 5, 6)
sev_breaks <- seq(70, 100, by = 5)

p_freq_sev <- ggplot() +
  geom_sf(data = korea_states, fill = "gray97", color = "gray80", linewidth = 0.3) +
  geom_point(
    data = plot_df,
    aes(x = Long, y = Lat,
        fill = cdh_ave,          # 색 = frequency
        size = intensity_ave),   # 크기 = severity(median intensity)
    shape = 21, color = "black", stroke = 0.3, alpha = 0.9
  ) +
  scale_fill_stepsn(
    colours = common_colors,
    breaks  = freq_breaks,
    limits  = c(min(freq_breaks), max(freq_breaks)),
    name    = "CPHW\nFrequency"
  ) +
  scale_size_continuous(
    breaks = sev_breaks,
    range = c(0.5, 3),
    name  = "Median\nSeverity"
  ) +
  facet_wrap(~season, ncol = 2) +
  labs(
    title    = "Seasonal Compound Heatwave–Pluvial Events in Korea (1986–2025)",
    subtitle = "Color: Event Frequency, Size: Median Severity per Station",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    strip.text    = element_text(size = 10, face = "bold"),
    legend.position = "right",
    panel.grid    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank()
  )

p_freq_sev

ggsave(paste0(result_path, "/map_freq_sev.png"),
       plot = p_freq_sev, width = 7.2, height = 5.2, units = "in", dpi = 400)




