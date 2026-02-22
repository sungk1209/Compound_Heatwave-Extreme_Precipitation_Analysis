
###########################################################
#  1.filename: 05.2 plot_freq_severity.R
#  2.Developer: Kyungmin Sung
#  3.Date: Dec.06.2025
#  4.Description: Calculate Heatwave-Drought Severity 
#             
##########################################################

library(tidyverse)
require(zoo)
require(ggplot2)
require(lubridate)
require(data.table)
require(slider)
library(patchwork)

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)  # For spatial data handling
require(gridExtra)
require(scales)
require(ggpmisc)

select <- dplyr::select

result_path <- "./result/"

hw_joint <- hw_severity_season %>%
  select(-X)

hw_joint <- hw_joint %>%
  mutate(comp_int_rel = T_anom_rel * P_excess_rel)

hw_joint <- hw_joint %>%
  mutate(year = year(date))

hw_joint <- hw_joint %>%
  mutate(
    year_season = case_when(
      month == 12 ~ year + 1,
      TRUE ~ year
    )
  )

station_median <- hw_joint %>%
  group_by(ID, Long, Lat, year_season, season) %>%
  summarise(
    annual_mean_comb = max(comp_int_rel, na.rm = TRUE),
    .groups = "drop"
  ) 

median_events <- hw_joint %>%
  left_join(station_median,
            by = c("ID", "Long", "Lat", "year_season", "season")) %>%
  mutate(
    diff = abs(comp_int_rel - annual_mean_comb)   # median과 얼마만큼 차이나는지
  ) %>%
  group_by(ID, year_season, season,Long, Lat) %>%
  slice_min(order_by = diff, n = 1, with_ties = FALSE) %>%  
  ungroup()


hw_joint_processed <- median_events %>%
  mutate(
    Period = case_when(
      year_season <= 2023 ~ "1986–2023",
      TRUE ~ "2024–2025"
    )
  )

xlim_data <- quantile(hw_joint_processed$T_anom_rel, c(0.01, 0.99), na.rm = TRUE)
ylim_data <- quantile(hw_joint_processed$P_excess_rel, c(0.01, 0.99), na.rm = TRUE)

recent_events <- hw_joint_processed %>%
  filter(year_season >= 2024)
# p_med_scatter <- ggplot(median_events,
#                         aes(x = T_anom_rel, y = P_excess_rel)) +
#   geom_point(
#     aes(color = comp_int_rel),
#     size = 2,
#     alpha = 0.1
#   ) +
#   facet_wrap(~season, ncol = 2) +
#   scale_color_viridis_c(option = "plasma", name = "Intensity") +
#   labs(
#     title = "Seasonal Representative Compound Events (Median Event per Station-Year)",
#     x = "Relative Heatwave Anomaly (T_excess_rel)",
#     y = "Relative Pluvial Excess (P_excess_rel)"
#   ) +
#   theme_minimal(base_size = 12)
# 
# 
# p_med_scatter

p_joint_contour <- ggplot(hw_joint_processed,
                          aes(x = T_anom_rel, y = P_excess_rel)) +
  
  # 2차원 밀도 채우기 (밀집 지역을 색상 층으로 표현)
  # geom_density_2d_filled가 '원 형태의 분포를 가장 잘 표현합니다.
  geom_density_2d_filled(aes(fill = after_stat(level)), 
                         contour_var = "ndensity", # 밀도를 정규화하여 레벨 설정
                         alpha = 0.7) +
  
  # 원본 데이터 포인트
  geom_point(color = "grey40",size = 0.5, alpha = 0.05) +
  # geom_point(data = recent_events,
  #            aes(color = comp_int_rel), 
  #            shape = 4, 
  #            size = 3,  
  #            stroke = 1.5, 
  #            alpha = 1) +
  # 
  # 🌟 핵심: Period와 season을 모두 사용하여 패널 분리
  facet_grid(Period ~ season) + 
  
  scale_fill_viridis_d(option = "viridis", name = "Density\nLevel") +
  scale_color_viridis_c(option = "plasma", name = "Event\nIntensity", guide = "none") +
  coord_cartesian(xlim = xlim_data, ylim = ylim_data) +
  
  labs(
    title = "Joint Distribution of Compound Event Components (Median Events)",
    x = "Relative Heatwave Anomaly (T)",
    y = "Relative Pluvial Excess (Extreme P)",
    subtitle = "Density contours (circles) show shift between early (1986–2023) and recent (2024–2025) periods."
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )
p_joint_contour

ggsave(paste0(result_path, "/joint_contour_max_1214.png"),
       plot = p_joint_contour, width = 7.2, height = 5.2, units = "in", dpi = 400)

# --- 3-1. 상단 주변부 분포 (T_anom_rel) ---
p_top_bar <- ggplot(hw_joint_processed, aes(x = T_anom_rel)) +
  geom_histogram(aes(y = after_stat(density), fill = Period), 
                 bins = 20, position = "identity", alpha = 0.4) + # Period별 히스토그램
  geom_density(aes(color = Period), linewidth = 1) +
  scale_fill_manual(values = c("1986–2000 (Early)" = "#4daf4a", "2001–2025 (Recent)" = "#e41a1c")) +
  scale_color_manual(values = c("1986–2000 (Early)" = "#4daf4a", "2001–2025 (Recent)" = "#e41a1c")) +
  coord_cartesian(xlim = xlim_data) +
  labs(x = NULL, y = "Density") +
  facet_wrap(~season, ncol = 4) +
  theme_void(base_size = 10) +
  theme(legend.position = "none",
        plot.margin = margin(b = 0),
        strip.text = element_blank())
p_top_bar

# --- 3-2. 오른쪽 주변부 분포 (P_excess_rel) ---
p_right_bar <- ggplot(hw_joint_processed, aes(x = P_excess_rel)) +
  geom_histogram(aes(y = after_stat(density), fill = Period), 
                 bins = 20, position = "identity", alpha = 0.4) +
  geom_density(aes(color = Period), linewidth = 1) +
  scale_fill_manual(values = c("1986–2000 (Early)" = "#4daf4a", "2001–2025 (Recent)" = "#e41a1c")) +
  scale_color_manual(values = c("1986–2000 (Early)" = "#4daf4a", "2001–2025 (Recent)" = "#e41a1c")) +
  coord_flip(xlim = ylim_data) +
  labs(x = NULL, y = "Density") +
  facet_wrap(~season, ncol = 1) +
  theme_void(base_size = 10) +
  theme(legend.position = "none",
        plot.margin = margin(l = 0),
        strip.text = element_blank())

p_right_bar
# --- 4. 최종 조합 (patchwork) ---

p_joint_shift <- ggplot(hw_joint_processed,
                        aes(x = T_anom_rel, y = P_excess_rel)) +
  geom_point( size = 0.5, alpha = 0.05) + # 배경 데이터
  
  geom_density_2d(aes(color = Period), linewidth = 1.2) +
  geom_density_2d_filled(aes(fill = Period), alpha = 0.3) +
  
  scale_fill_manual(values = c("1986–2000 (Early)" = "#377eb8", "2001–2025 (Recent)" = "#e41a1c")) +
  scale_color_manual(values = c("1986–2000 (Early)" = "#377eb8", "2001–2025 (Recent)" = "#e41a1c")) +
  scale_colour_viridis_d(option = "plasma", name = "Intensity", guide = "none") +
  
  coord_cartesian(xlim = xlim_data, ylim = ylim_data) +
  facet_wrap(~season, ncol = 2) +
  labs(
    title = "Shift in Compound Event Distribution: Early vs. Recent Periods",
    x = "Relative Heatwave Anomaly (T_anom_rel)",
    y = "Relative Pluvial Excess (P_excess_rel)",
    subtitle = "Density contours show shift from 1986–2000 (Blue) to 2001–2025 (Red)."
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

p_joint_shift

xlim_data <- quantile(hw_joint$T_anom_rel, c(0.01, 0.99), na.rm = TRUE)
ylim_data <- quantile(hw_joint$P_excess_rel, c(0.01, 0.99), na.rm = TRUE)

recent_events <- hw_joint_processed %>%
  filter(year_season >= 2024)
# p_med_scatter <- ggplot(median_events,
#                         aes(x = T_anom_rel, y = P_excess_rel)) +
#   geom_point(
#     aes(color = comp_int_rel),
#     size = 2,
#     alpha = 0.1
#   ) +
#   facet_wrap(~season, ncol = 2) +
#   scale_color_viridis_c(option = "plasma", name = "Intensity") +
#   labs(
#     title = "Seasonal Representative Compound Events (Median Event per Station-Year)",
#     x = "Relative Heatwave Anomaly (T_excess_rel)",
#     y = "Relative Pluvial Excess (P_excess_rel)"
#   ) +
#   theme_minimal(base_size = 12)
# 
# 
# p_med_scatter

hw_joint <- hw_joint %>%
  mutate(
    Period = case_when(
      year_season <= 2000 ~ "1986–2000 (Early)",
      TRUE ~ "2001–2025 (Recent)"
    )
  )

hw_count <- hw_joint %>%
  group_by(season, Period) %>%
  summarise(event = n(), 
            .groups = "drop")

p_joint_contour <- ggplot(hw_joint,
                          aes(x = T_anom_rel, y = P_excess_rel)) +
  
  # 2차원 밀도 채우기 (밀집 지역을 색상 층으로 표현)
  # geom_density_2d_filled가 '원 형태의 분포를 가장 잘 표현합니다.
  geom_density_2d_filled(aes(fill = after_stat(level)), 
                         contour_var = "ndensity", # 밀도를 정규화하여 레벨 설정
                         alpha = 0.7) +
  
  # 원본 데이터 포인트
  geom_point(color = "grey40",size = 0.5, alpha = 0.05) +
  # geom_point(data = recent_events,
  #            aes(color = comp_int_rel), 
  #            shape = 4, 
  #            size = 3,  
  #            stroke = 1.5, 
  #            alpha = 1) +
  # 
  # 🌟 핵심: Period와 season을 모두 사용하여 패널 분리
  facet_grid(Period ~ season) + 
  
  scale_fill_viridis_d(option = "viridis", name = "Density\nLevel") +
  scale_color_viridis_c(option = "plasma", name = "Event\nIntensity", guide = "none") +
  coord_cartesian(xlim = xlim_data, ylim = ylim_data) +
  
  labs(
    title = "Joint Distribution of Compound Event Components (All Events)",
    x = "Relative Heatwave Anomaly (T_anom_rel)",
    y = "Relative Pluvial Excess (P_excess_rel)",
    subtitle = "Density contours (circles) show shift between early (1986–2000) and recent (2001–2025) periods."
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )
p_joint_contour

ggsave(paste0(result_path, "/joint_contour_allpoints.png"),
       plot = p_joint_contour, width = 7.2, height = 5.2, units = "in", dpi = 400)

