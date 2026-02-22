
###########################################################
#  1.filename: .hwdr_analyis_frequency.R
#  2.Developer: Kyungmin Sung
#  3.Date: Oct.10.2025
#  4.Description: Calculate Heatwave-Drought Coincidence  
#             
##########################################################

library(tidyverse)
library(ggplot2)
require(sf)
require(rnaturalearth)

# Same function and process to CDHW figure.R
#======================================================================
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) return("DJF")
  if (month %in% c(3, 4, 5)) return("MAM")
  if (month %in% c(6, 7, 8)) return("JJA")
  if (month %in% c(9, 10, 11)) return("SON")
}

cphw_df <- cphw_df %>%
  mutate(
    season = sapply(month, get_season)
  )

cphw_df <- cphw_df %>%
  mutate(
    year_season = case_when(
      month == 12 ~ year + 1,
      TRUE ~ year
    )
  )

cphw_df <-readRDS(paste0(result_path, "/hwpl_coincidence_df.rds"))
clim_df <- readRDS(paste0(result_path, "/hwpl_clim.rds"))


# 기준년도 평균과 2025년 추출
base_years  <- 1986:2023
target_years <- 2024 :2025

cphw_seasonal_trend <- cphw_df %>%
  group_by(ID, year_season, season) %>%
  summarise(cdh_count = sum(num.coincidence, na.rm = TRUE), .groups ="drop")

cphw_base_ave <- cphw_seasonal_trend %>%
  filter(year_season %in% base_years) %>%
  group_by(ID, season) %>%
  summarise(base_ave = mean(cdh_count, na.rm = TRUE), cdh_sd = sd(cdh_count, na.rm = TRUE),
            .groups ="drop")

cphw_base_ave <- cphw_base_ave%>%
  left_join(stn_info, by = c("ID" = "STN_ID")) 


######################
# create basemap 

korea_states <- ne_states(country = "South Korea", returnclass = "sf") 

common_breaks <- c(0, 1, 2, 3, 4, 5, 6)
common_colors <- c(
  "grey90",   
  "#fed976",  
  "#fd8d3c",  
  "#f03b20",  
  "#bd0026",   
  "#800080",
  "#1B2A41"
  
)

p_map_base <-  ggplot() +
  geom_sf(data = korea_states, fill = "gray97", color = "gray80", linewidth = 0.3) +
  
  geom_point(data = cphw_base_ave,
             aes(x = Long, y = Lat, fill = cdh_ave),
             color = "gray20", shape = 21, size = 3, stroke = 0.3, alpha = 0.9
  ) +
  # scale_fill_gradientn(
  #   colours = c("grey","#ffffcc", "#ffeda0", "#feb24c", "#f03b20",  "#800080"),
  #   name = "Mean \nAnnual CPLHW Events",
  #   limits = c(0, 6),
  #   breaks = seq(0, 6, by = 2) 
  # ) +
  scale_fill_stepsn(
    colours = common_colors,
    #breaks  = common_breaks,
    limits  = c(0, 10),
    name = "Mean\nCPHW Events"
  ) +
  scale_size_continuous(range = c(0.7, 3), guide = "none") +
  facet_wrap(~season, ncol = 4) +
  
  labs(
    title = "Seasonal Mean Compound Heatwave-Pluvial Events \n(1986-2000)",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
  #  plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
    # plot.margin = margin(10, 10, 10, 10)
  )

p_map_base

 ggsave(p_map_base,filename = paste0(result_path,"/CP_HW_base_seasonal.png"),
        width = 8.6,height = 4.3, units = "in", dpi = 300)
#######################

########################
# create target year map
cphw_target_ave <- cphw_seasonal_trend %>%
  filter(year_season %in% target_years) %>%
  group_by(ID, season) %>%
  summarise(target_ave = mean(cdh_count, na.rm = TRUE), .groups ="drop")
 
 cphw_target_ave <- cphw_target_ave%>%
   left_join(stn_info, by = c("ID" = "STN_ID")) 
 
korea_states <- ne_states(country = "South Korea", returnclass = "sf") 

p_map_target <-  ggplot() +
  geom_sf(data = korea_states, fill = "gray97", color = "gray80", linewidth = 0.3) +
  
  geom_point(data = cphw_target_ave,
             aes(x = Long, y = Lat, fill = cdh_ave),
             color = "gray20", shape = 21, stroke = 0.3, alpha = 0.9, size = 3
  ) +
  
  scale_fill_stepsn(
    colours = common_colors,
    breaks  = common_breaks,
    limits  = c(0, 6),
    name = "Mean\nCPHW Events"
  ) +
 # scale_size_continuous(range = c(0.7, 3), guide = "none") +
  facet_wrap(~season, ncol = 4) +
  
  labs(
    title = "Seasonal Mean Compound Heatwave-Pluvial Events \n (2001-2025)",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

p_map_target

ggsave(p_map_target,filename = paste0(result_path,"/CP_HW_2000_2025.png"),
       width = 8.6,height = 4.3, units = "in", dpi = 300)

########################################
## map for all periods
########################################

cphw_ave <- cphw_seasonal_trend %>%
  group_by(ID, season) %>%
  summarise(cdh_ave = mean(cdh_count, na.rm = TRUE), .groups ="drop")

cphw_ave <- cphw_ave%>%
  left_join(stn_info, by = c("ID" = "STN_ID")) 

cphw_ave <- cphw_ave%>%
  mutate(
  season = factor(season, levels = c("DJF", "MAM", "JJA", "SON")))

p_map <-  ggplot() +
  geom_sf(data = korea_states, fill = "grey90", color = "grey60", linewidth = 0.3) +
  
  geom_point(data = cphw_ave,
             aes(x = Long, y = Lat, fill = cdh_ave),
             color = "black", size = 2.5, shape = 21, stroke = 0.3, alpha = 0.9
  ) +
  scale_fill_stepsn(
    colours = common_colors,
    breaks  = common_breaks,
    limits  = c(0, 6),
    name = "Mean\nCPHW \nEvents \n"
  ) +
  #scale_size_continuous(range = c(0.7, 3), guide = "none") +
  facet_wrap(~season, ncol = 4) +
  
  labs(
    title = "Seasonal Mean Compound Heatwave-Pluvial Events \n (1986-2025)",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0.5),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 9),
    #panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

p_map

ggsave(p_map,filename = paste0(result_path,"/CP_HW_12151.png"),
       width = 9.0,height = 3.3, units = "in", dpi = 300)

####################################################################
### Annual average (all season)

korea_country <- ne_countries(
  country = "South Korea",
  scale = "medium",
  returnclass = "sf"
)

cphw_annual <- cphw_seasonal_trend %>%
  group_by(ID) %>%
  summarise(cdh_ave = mean(cdh_count, na.rm = TRUE), .groups ="drop")

cphw_annual <- cphw_annual%>%
  left_join(stn_info, by = c("ID" = "STN_ID")) 

common_breaks <- c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
common_colors <- c("yellow", "#fed976","#fd8d3c","#f03b20", "#bd0026","#800080","#1B2A41")


p_map_all_season <-  ggplot() +
  geom_sf(data = korea_country, fill = "grey20", color = "white", linewidth = 0.3) +
  
  geom_point(data = cphw_annual,
             aes(x = Long, y = Lat, fill = cdh_ave),
             color = "white", size = 5, shape = 21, stroke = 0.4
  ) +
  # scale_fill_gradientn(
  #   colours = c("grey","#ffffcc", "#ffeda0", "#feb24c", "#f03b20",  "#800080"),
  #   name = "Mean \nAnnual CPLHW Events",
  #   limits = c(0, 25),
  #   breaks = seq(0, 25, by = 5)
  # ) +
  scale_fill_stepsn(
    colours = common_colors,
    breaks  = common_breaks,
    limits  = c(0, 2.5),
    name = "Mean\nCPHW \nEvents \n"
  ) +
  #scale_size_continuous(range = c(0.7, 3), guide = "none") +
  #facet_wrap(~season, ncol = 4) +
  
  labs(
    title = "Annual Mean Compound Heatwave-Heavy Rainfall Events \n (1986-2025)",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(

    plot.background  = element_rect(fill = "grey20", color = NA),
    panel.background = element_rect(fill = "grey20", color = NA),
    legend.background = element_rect(fill = "grey20", color = NA),
    legend.key = element_rect(fill = "grey20", color = NA),

    plot.title = element_text(
      size = 11, face = "bold", hjust = 0.5, color = "white"
    ),
    plot.subtitle = element_text(
      size = 9, color = "gray90", hjust = 0.5
    ),
    strip.text = element_text(
      size = 9, face = "bold", color = "white"
    ),
  
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold", color = "white"),
    legend.text  = element_text(size = 9, color = "white"),
    
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


p_map_all_season

ggsave(p_map_all_season,filename = paste0(result_path,"/CP_HW.png"),
       width = 5.0,height = 5, units = "in", dpi = 300)


ggplot(cphw_annual, aes(x = cdh_ave)) +
  geom_density(aes(y = after_stat(density)),
                 alpha = 0.5, color = "skyblue", fill = "skyblue")

############ Distribution with Density

dist_df <- cphw_seasonal_trend %>%
  filter(!is.na(cdh_ave))

p_hist_base <- ggplot(cphw_ave, aes(x = cdh_ave)) +
  
  #geom_histogram(aes(y = after_stat(density)),
  #               bins = 15, alpha = 0.5, color = "skyblue", fill = "skyblue") +
  geom_density(aes(group= season, color = season, fill = season),linewidth = 0.8, alpha = 0.2) +
  #facet_wrap(~season, scales = "free") +
  labs(
    title = "Distribution of Station-wise Mean CPHW Counts",
    subtitle = sprintf("Seasonal mean over %d–%d", min(base_years), max(base_years)),
    x = "Mean CPHW events per season",
    y = "Probability density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey30", hjust = 0.5)
  )


p_hist_base

ggsave(paste0(result_path, "/CPHW_SeasonalMean_Distribution_hist_", 
              min(base_years), "_", max(base_years), ".png"),
       plot = p_hist_base, width = 5.2, height = 4.5, units = "in", dpi = 400)

#### Histogram for target 2023-2024

# dist_df <- cphw_target_ave %>%
#   filter(!is.na(cdh_ave))
# 
# p_hist <- ggplot(dist_df, aes(x = cdh_ave)) +
#   geom_histogram(aes(y = after_stat(density)),
#                  bins = 15, alpha = 0.5, color = "skyblue", fill = "skyblue") +
#   geom_density(linewidth = 0.8, alpha = 0.8) +
#   facet_wrap(~season, scales = "free_y") +
#   labs(
#     title = "Distribution of Station-wise Mean CPHW Counts",
#     subtitle = sprintf("Seasonal mean over %d–%d", min(target_years), max(target_years)),
#     x = "Mean CPHW events per season",
#     y = "Probability density"
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(size = 9, color = "grey30", hjust = 0.5)
#   )
# 
# p_hist



comparison_df <- bind_rows(
  cphw_base_ave %>% mutate(Period = "1986-2000"),
  cphw_target_ave %>% mutate(Period = "2001-2025")
) %>% filter(!is.na(cdh_ave))

p_hist_comparison <- ggplot(comparison_df, aes(x = cdh_ave, color = Period, fill = Period)) +
  geom_density(alpha = 0.4, linewidth = 1.0) +
  facet_wrap(~season, scales = "free") +
  labs(
    title = "Distribution Shift: Base Period vs. Latest Period",
    subtitle = "Comparison of station-wise mean CPHW counts",
    x = "Mean CPHW events per season",
    y = "Probability density"
  ) +
  scale_color_manual(values = c("1986-2000" ="#1b9e77", "2001-2025" = "#d95f02")) +
  scale_fill_manual(values = c("1986-2000" = "#1b9e77", "2001-2025" = "#d95f02")) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size=10, hjust = 0.5),
    plot.subtitle = element_text(size=9, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank()
  )
p_hist_comparison
ggsave(paste0(result_path, "/CPHW_comparison_Distribution_hist1215.png"),
       plot = p_hist_comparison, width = 5.2, height = 4.5, units = "in", dpi = 400)

## Combine two histogram and cdf

fig_B <- ggplot() +
  geom_histogram(data=cphw_base_ave, aes(x=cdh_ave, y=after_stat(density)),
                 fill="skyblue", alpha=0.4) +
  geom_density(data=cphw_base_ave, aes(x=cdh_ave), color="blue", linewidth = 0.8) +
  geom_histogram(data=cphw_target_ave, aes(x=cdh_ave, y=after_stat(density)),
                 fill="red", alpha=0.4) +
  geom_density(data=cphw_target_ave, aes(x=cdh_ave), color="red", linewidth = 0.8) +
  facet_wrap(~season, scales = "free") +
  labs(
    title = "Distributional Shift of Seasonal CPHW Events",
    subtitle = "Past climatology (1986–2023) vs Recent years (2024–2025)",
    x = "Station-wise seasonal mean CPHW",
    y = "Density"
  ) +
  theme_minimal(base_size=12)

fig_B
ggsave(paste0(result_path, "/CPHW_compare_보고서.png"),
       plot = fig_B, width = 5.2, height = 4.5, units = "in", dpi = 400)


recent_points <- cphw_seasonal_trend %>%
  filter(year_season %in% c(2024,2025)) %>%
  group_by(season, year_season) %>%
  summarise(cdh_ave = mean(cdh_count, na.rm = TRUE), .groups ="drop")

recent_points$year_season <- as.character(recent_points$year_season)
recent_points <- recent_points %>%
  mutate(Period = "2001-2025" )


p_box <- ggplot(comparison_df, aes(x = season, y = cdh_ave, color = Period, 
                                   fill = Period)) +
  geom_boxplot( alpha = 0.5, width = 0.6) +
  geom_point(
    data = recent_points, 
    aes(x = as.numeric(factor(season)) + 0.22,, y = cdh_ave, group = Period, color = year_season), # group=Period를 주어야 박스 위치에 맞게 정렬됨
    shape = 18,             # 다이아몬드 모양 (별 모양은 8번)
    size = 4,               
    position = position_dodge(width = 0.6), 
    show.legend = FALSE     
  ) +
  labs(
      x = "Season", y = "Mean CPHW events per season"
  ) +
  scale_color_manual(values = c("1986-2000" ="#1b9e77", "2001-2025" = "#d95f02", "2024" = "red", "2025" = "blue")) +
  scale_fill_manual(values = c("1986-2000" = "#1b9e77", "2001-2025" = "#d95f02")) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size=10, hjust = 0.5),
    plot.subtitle = element_text(size=9, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank()
  ) + 
  guides(color = "none")

p_box
ggsave(paste0(result_path, "/CPHW_box_1215.png"),
       plot = p_box, width = 4.2, height = 4.5, units = "in", dpi = 400)


######################################################
# Calculate Anomaly : Target( 2024-2025) -Base (1986-2023)
########################################################
map_data_combined  <- cphw_base_ave %>%
  left_join(
    cphw_target_ave %>% select(ID, season, target_ave),
    by = c("ID", "season")
  )


map_data_combined <- map_data_combined %>%
  filter(cdh_sd != 0 ) %>%
  mutate(anomaly = (target_ave - base_ave) / cdh_sd) %>%
  drop_na()
 
anomaly_breaks <- c(-4,-2,0,2,4,7)

anomaly_colors <- c(
  "#053061",  # navy
  "#2166ac",
  "#fdae61",
  "#d73027",
  "#7f0000"
)

  map_anomaly <- map_data_combined %>%
      mutate(season = factor(season, levels = c("DJF", "MAM", "JJA", "SON")))
  
  
  # 편차 데이터의 최대/최소 절대값 중 큰 값을 찾아 대칭적인 색상 범위를 설정
  max_abs_anomaly <- max(abs(map_anomaly$anomaly), na.rm = TRUE) # 현재 최대 절대 편차 (약 11.5)
  # 범례 눈금을 4 단위로 설정하기 위해, max_abs_anomaly를 4의 배수로 올림 (ceiling)
  # plot_limit <- ceiling(max_abs_anomaly / 4) * 4 # 예: 11.5 -> 12
  # anomaly_limits <- c(-plot_limit, plot_limit)
  # anomaly_breaks <- seq(anomaly_limits[1], anomaly_limits[2], by = 4)
  
  p_map_anomaly <- ggplot() +
    geom_sf(data = korea_country, fill = "gray20", color = "white", linewidth = 0.3) +
    
    geom_point(data = map_anomaly,
               # 편차 값에 따라 색상과 크기 모두 조정
               aes(x = Long, y = Lat, fill = anomaly),
               color = "white", shape = 21, stroke = 0.3, size = 3
    ) +
    
    # 발산형 색상 스케일 (Diverging Color Scale) 적용
    scale_fill_stepsn(
      colours = anomaly_colors,
      breaks  = anomaly_breaks,
      limits  = c(-4, 6),
      name = "CHWEP\n Anomaly \n(Events/Year)"
    ) +
    facet_wrap(~season, ncol = 2) +
    
    labs(
      title = "Regional Anomaly of Compound Heatwave–Pluvial Events",
      subtitle = "Difference in Mean Event Counts: (2001–2025) minus (1986–2000)",
      x = NULL, y = NULL
    ) +
    
    theme_minimal(base_size = 12) +

    theme(
      plot.background  = element_rect(fill = "grey20", color = NA),
      panel.background = element_rect(fill = "grey20", color = NA),
      legend.background = element_rect(fill = "grey20", color = NA),
      legend.key = element_rect(fill = "grey20", color = NA),
      
     plot.title = element_text( 
        size = 11, face = "bold", hjust = 0.5, color = "white"
        ),
      plot.subtitle = element_text(
        size = 9, color = "gray90", hjust = 0.5
        ),
      strip.text = element_text(
        size = 9, face = "bold", color = "white"
        ),
      
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold", color = "white"),
      legend.text  = element_text(size = 9, color = "white"),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
  )
  
  p_map_anomaly
  
  ggsave(p_map_anomaly, filename = paste0(result_path,"/CP_HW_anomaly_map.png"),
          width = 5.0, height = 5.5, units = "in", dpi = 300)
  
  count_anomaly_gt2 <- map_anomaly %>%
    filter(anomaly > 2) %>%          # 기준 초과
    distinct(ID, season) %>%         # gauge 중복 제거 (안전장치)
    group_by(season) %>%
    summarise(
      n_gauge = n(),
      .groups = "drop"
    )
  
  count_anomaly_gt2
  
  count_anomaly_ratio <- map_anomaly %>%
    group_by(season) %>%
    summarise(
      total_gauge = n_distinct(ID),
      n_anom_gt2  = n_distinct(ID[anomaly > 2]),
      ratio_pct   = 100 * n_anom_gt2 / total_gauge,
      .groups = "drop"
    )
  
  count_anomaly_ratio
  