
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

##### --- 1. 기준 기간 (1986-2023) 연도별 총합 계산 (분포 정의) ---

# cphw_df에서 base_years(1986:2023)에 해당하는 연도별 총합을 계산
base_sum_data <- cphw_df %>%
  filter(year_season %in% base_years) %>%
  group_by(year_season, season) %>%
  summarise(
    total_cphw = sum(num.coincidence, na.rm = TRUE),
    .groups = "drop"
  )

# --- 2. 비교 대상 (2024) 연도별 총합 추출 ---

# cphw_df에서 2024년의 계절별 총합을 계산
target_2024_sum <- cphw_df %>%
  filter(year_season == 2024) %>%
  group_by(year_season, season) %>%
  summarise(
    total_cphw_2024 = sum(num.coincidence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(season, total_cphw_2024)

# --- 3. 통계 지표 계산 (Z-score, Percentile) ---

# 기준 분포의 평균, 표준편차 계산
base_stats <- base_sum_data %>%
  group_by(season) %>%
  summarise(
    mu = mean(total_cphw, na.rm = TRUE),
    sigma = sd(total_cphw, na.rm = TRUE),
    .groups = "drop"
  )

# 2024년 총합 데이터에 기준 통계량 결합
anomaly_analysis <- target_2024_sum %>%
  left_join(base_stats, by = "season") %>%
  # Z-score 계산
  mutate(
    Z_score = (total_cphw_2024 - mu) / sigma
  )

# 백분위수 (Percentile) 계산

base_with_2024 <- base_sum_data %>%
  left_join(target_2024_sum, by = "season")

percentile_calculation <- base_with_2024 %>%
  group_by(season) %>%
  summarise(
    # 1986–2023 값들
    P_value = {
      ecdf_func <- ecdf(total_cphw)
      X_2024 <- unique(total_cphw_2024)   # 한 시즌당 1개 값이라고 가정
      ecdf_func(X_2024) * 100
    },
    Is_Record = {
      X_2024 <- unique(total_cphw_2024)
      X_2024 == max(c(total_cphw, X_2024), na.rm = TRUE)
    },
    .groups = "drop"
  )

percentile_calculation
# 최종 분석 결과 결합
final_anomaly_table <- anomaly_analysis %>%
  left_join(percentile_calculation, by = "season") %>%
  # 해석 열 추가
  mutate(
    P_rank = case_when(
      P_value >= 99 ~ "Extremely High (>99th)",
      P_value >= 95 ~ "Very High (>95th)",
      P_value >= 90 ~ "High (>90th)",
      TRUE ~ "Near Average (<90th)"
    ),
    Record_Status = ifelse(Is_Record, "Record High (100%)", P_rank)
  ) %>%
  select(season, total_cphw_2024, mu, sigma, Z_score, P_value, Record_Status)

# 결과 출력 (논문에 테이블로 제시)
print(final_anomaly_table)

#####################
#Time series with Bootstrapping 

# 1. 지역 총합(Total Sum) 계산 (메인 트렌드 선)
ts_summary_sum <- cphw_seasonal_trend  %>%
  group_by(year_season, season) %>%
  summarise(
    total_cphw = sum(cdh_count, na.rm = TRUE),
    .groups = "drop"
  )

# 3. 부트스트랩을 통한 95% 신뢰 구간 (CI) 추정
# (경고: 이 과정은 계산 시간이 소요될 수 있습니다. B=10000은 통상적인 횟수입니다.)

B <- 10000 # 부트스트랩 반복 횟수

# 연도-계절 조합별 95% CI를 저장할 데이터 프레임
ci_df <- cphw_seasonal_trend %>%
  group_by(year_season, season) %>%
  group_modify(~ {
    current_data <- .x$cdh_count
    
    # 10000번 부트스트랩 샘플링 및 총합 계산
    boot_sums <- replicate(B, {
      # 지점 데이터를 복원 추출 (resampling with replacement)
      # n=length(current_data)는 전체 지점 개수만큼 샘플링한다는 의미
      sample_data <- sample(current_data, size = length(current_data), replace = TRUE)
      sum(sample_data, na.rm = TRUE)
    })
    
    # 부트스트랩 총합 분포에서 2.5% 및 97.5% 값 계산 (95% CI)
    ci_values <- quantile(boot_sums, probs = c(0.025, 0.975), na.rm = TRUE)
    
    # 결과 반환
    tibble(
      ci_lower = ci_values[1],
      ci_upper = ci_values[2]
    )
  }) %>%
  ungroup()


# 4. 총합 트렌드와 CI를 결합
final_ts_data <- ts_summary_sum %>%
  left_join(ci_df, by = c("year_season", "season"))

final_ts_data <- final_ts_data %>%
  filter(year_season != 2026)


## 5. 부트스트랩 CI를 사용한 시각화 (논문 스타일)
p_ts_total_ci <- ggplot(final_ts_data, aes(x = year_season, y = total_cphw)) +
  
  # A. 95% 신뢰 구간 (CI)을 리본(Ribbon) 형태로 표시
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    fill = "#b2182b", alpha = 0.15 # 트렌드 선과 같은 색상, 투명하게
  ) +
  
  # B. 지역 총합(Total Sum) Time Series (핵심 트렌드)
  geom_line(color = "#b2182b", linewidth = 1.2) + # Dark Red line for emphasis
  geom_point(color = "#b2182b", size = 2.0, shape = 21, fill = "white", stroke = 1.0) +
  geom_point(data = final_ts_data %>% filter(year_season >= 2024), 
             aes(x = year_season, y = total_cphw),
             color = "#fdbf6f", size = 3.5, shape = 21, fill = "#d95f02", stroke = 1.2) +
  
  geom_smooth(method = "lm", color = "grey50",linetype = "dashed", linewidth = 0.8, se = FALSE) +
  # 계절별 분할
  facet_wrap(~season, ncol = 2, scales = "free_y") +
  
  # 레이블 및 제목
  labs(
    title = "Regional Total Compound Heatwave–Pluvial Event Exposure",
    subtitle = sprintf("Trend of total event counts summed across all stations (Shading: 95%% Bootstrap Confidence Interval)", min(base_years), max(base_years)),
    x = "Year", 
    y = "Total CPHW Events per Season (Sum across all stations)"
  ) +
  
  # 깔끔한 theme_bw 적용
  theme_bw(base_size = 10) +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5, margin = margin(b = 10)),
    axis.title    = element_text(size = 10, face = "bold"),
    axis.text     = element_text(size = 8.5),
    strip.text    = element_text(size = 10, face = "bold", margin = margin(t = 5, b = 5)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border  = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
    plot.margin   = margin(10, 20, 10, 20)
  ) +
  # X축 눈금을 깔끔하게 정리 (5년 단위)
  scale_x_continuous(breaks = seq(min(final_ts_data$year_season), max(final_ts_data$year_season), by = 5))

p_ts_total_ci

ggsave(paste0(result_path, "/CPHW_TS_Total_Risk_Bootstrap_CI_1214.png"),
       plot = p_ts_total_ci, width = 7.2, height = 5.2, units = "in", dpi = 400)

#Print out coefficient
lm_by_season <- final_ts_data %>%
  group_by(season) %>%
  group_modify(~ {
    fit <- lm(total_cphw ~ year_season, data = .x)
    tibble(
      slope = coef(fit)[2],
      intercept = coef(fit)[1],
      p_value = summary(fit)$coefficients[2, 4],
      r2 = summary(fit)$r.squared
    )
  })

lm_by_season

##################################

# 2. 계절별 Mann-Kendall 검정 수행 및 결과 정리
mk_results <- ts_summary_sum %>%
  group_by(season) %>%
  # group_modify 대신 summarize를 사용하여 결과를 깔끔하게 정리
  summarise(
    # Mann-Kendall 검정 수행 (데이터는 total_cphw, 시간에 따른 순서는 year)
    mk_test = list(trend::mk.test(total_cphw)),
    .groups = 'drop'
  ) %>%
  # 검정 결과(list)에서 P-value와 트렌드 방향(Tau) 추출
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


