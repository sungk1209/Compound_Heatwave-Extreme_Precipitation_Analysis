# download data set from 기상청 API hub

require(ncdf4)

start_date <- "19860101"
end_date <- "20251201"

api_key <- "izHIvgvrTvOxyL4L677ztQ"
stn_info <- read.csv("./기상청_api/Station_coordi.csv")
stn_num <- stn_info$STN_ID

for (i in 1:length(stn_num)) {
  
 file_path <- paste0("./files/asos_",stn_num[i], ".csv")
 
 #dir.create(file_path, recursive = FALSE)
 
   
 url <- sprintf(
   "https://apihub.kma.go.kr/api/typ01/url/kma_sfcdd3.php?tm1=%s&tm2=%s&stn=%s&help=0&authKey=%s",
   start_date, end_date, stn_num[i], api_key
 )

download.file(url, file_path, mode = "wb") # URL에서 파일 다운로드

}

