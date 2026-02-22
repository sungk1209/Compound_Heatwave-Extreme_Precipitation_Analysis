
###########################################################
#  1.filename: 1.hwdr_analyis_frequency.R
#  2.Developer: Kyungmin Sung
#  3.Date: Oct.10.2025
#  4.Description: Calculate Heatwave-Drought Coincidence  
#             
##########################################################

library(tidyverse)
require(zoo)
require(ggplot2)
require(lubridate)
require(data.table)


select <- dplyr::select

file_path <- "./files/"
result_path <- "./result/"

# Function to compute Event Coincidence Rate (ECR)

  ecr <- function(event_x, event_y, max_lag = 7 ) {
  
  n <- length(event_x)
  coincidence_count <- 0
  coincidence_flag <- rep(0,n)
  
  for (i in 1:n) {
    if (event_x[i] == 1) {
      # Check if event_y occurs within the lag period
      for (lag in 1:max_lag) {
        j <- i + lag
        if (j > 0 && j <= n && event_y[j] == 1) {
          coincidence_count <- coincidence_count + 1
          coincidence_flag[i]  <- 1
          break  # Avoid counting multiple coincidences for the same event_x
        }
      }
    }
  }
  #return(coincidence_count / sum(event_x))
   total_events <- sum(event_x) # heatwave
   ecr_value <- if (total_events > 0) coincidence_count / total_events else NA
  
  return(list(
    ecr = ecr_value,
    flag = coincidence_flag
    )
  )

}

file_list <- list.files(path = file_path, pattern = "asos_*", full.names = TRUE)
stn_info <- read.csv("D:/Research_journals/compound_event/기상청_api/Station_coordi.csv")

cphw_df  <- data.frame()
clim_df <- data.frame()

for (i in c(1:length(file_list))){
  
  
  data_df <- fread(file_list[i], skip = 5, encoding = "UTF-8", fill = TRUE)
  # Fix the index number, this is the best I can do 
  target_cols <- c(1,2, 12, 39)
  
  # 필요한 열만 선택 + 첫 두 행(#, KST 행) 제거
  present_selected <- data_df[, ..target_cols]
  
  #  컬럼 이름 정리
  colnames(present_selected) <- c("Date","ID", "TA", "RN")
  
  #Remove the last row
  asos_df <- present_selected[-nrow(present_selected),]
  
  asos_df  <- asos_df %>%
    mutate(date = as.Date(Date, format = "%Y%m%d")) %>%
    mutate(jdate = yday(date)) %>%
    select(-Date) %>%
    as_tibble()
  
  asos_df$RN <- as.numeric(asos_df$RN)
  asos_df$RN[asos_df$RN == -9] <- 0
  
  asos_df$TA <- as.numeric(asos_df$TA)
  
  
  if (min(year(asos_df$date)) <= 1986){
    
    asos_df$TA[asos_df$TA == -99.0] <- NA
    asos_df$RN[asos_df$RN == -9] <- 0
    
    asos_df <- asos_df%>%
      mutate(jdate = yday(date)) 
    
     heatwave_thresholds <- asos_df %>%
       mutate(roll_temp = rollmean(TA, k = 15, fill= NA, align = "center"))
     
     heatwave_thresholds <- heatwave_thresholds %>%
       filter(year(date) >= 1991  & year(date) <= 2020)
    
     heatwave_thresholds <- heatwave_thresholds %>%
       group_by(jdate) %>%
       summarize(threshold = quantile(roll_temp, 0.90, na.rm = TRUE))

    colnames(heatwave_thresholds) <- c("jdate", "hw_thrsld")
    
     data_df <- asos_df%>%
       left_join(heatwave_thresholds, by = c("jdate" = "jdate")) %>%
       mutate(heatwave = ifelse(TA > hw_thrsld, 1, 0)) %>%
       arrange(date)
    
     # data_df <- asos_df%>%
     #   mutate(heatwave = ifelse(TA > heatwave_thresholds, 1, 0)) %>%
     #   arrange(date)
     # 
    data_df$heatwave <- with(rle(data_df$heatwave), rep(values & lengths >= 2, lengths))
    
 # Define Extreme Precipitation Events
     wet_days <- data_df %>% 
       filter(RN > 0 & year(date) >= 1991  & year(date) <= 2020)
     
     precip_threshold <- quantile(wet_days$RN, 0.95, na.rm = TRUE) 
     
     data_df <- data_df %>% 
       mutate(
         pl_thrsld = precip_threshold,
         extreme_precip = ifelse(RN > precip_threshold, 1, 0))
        
# Compute Event Coincidence Rate with a lag of 7 days
     
     data_df <- data_df %>% 
       mutate(year = year(date)) %>%
       group_by(year) %>%
       group_split() %>%
       
       lapply(function(df) {
         result <- ecr(df$heatwave, df$extreme_precip, max_lag = 7)
         df$hw_pluvial <- result$flag 
         df$ecr <- result$ecr
         df
         })%>%
       bind_rows()
     
     data_df <- data_df %>%
       mutate(month = month(date))

# 연도별 결과를 이름 붙여서 정리
     result_ecr <- data_df %>%
       group_by(ID, year, month) %>%
       summarize(num.coincidence = sum(hw_pluvial, na.rm = TRUE), .groups = "drop")
     
     # 저장
     cphw_df <- bind_rows(cphw_df, result_ecr)
     clim_df <- bind_rows(clim_df, data_df)
 }
}
# saveRDS(cphw_df, file = paste0(result_path, "/hwpl_coincidence_df.rds"))
# saveRDS(clim_df, file = paste0(result_path, "/hwpl_clim.rds"))

#clim_df<- readRDS(paste0(result_path, "/hwpl_clim.rds"))

cphw_df <- cphw_df %>% 
  left_join(stn_info, by = c("ID" = "STN_ID"))

clim_df <- clim_df%>%
  left_join(stn_info, by = c("ID" = "STN_ID"))

clim_df <- clim_df %>% arrange(date)
clim_df <- clim_df %>% mutate(date = as.Date(date))

saveRDS(cphw_df, file = paste0(result_path, "/hwpl_coincidence_df.rds"))
saveRDS(clim_df, file = paste0(result_path, "/hwpl_clim.rds"))

