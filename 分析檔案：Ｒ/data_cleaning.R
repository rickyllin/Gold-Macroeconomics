##data import
library(readr)
library(readxl)
library(tidyverse)
library(zoo)
library(tseries)
AU <- read_csv("~/政大課堂/計量經濟學（二）/期末報告/原始資料集/LBMA-gold_D-gold_D_EUR_AM.csv")
OIL <- read_csv("~/政大課堂/計量經濟學（二）/期末報告/原始資料集/DCOILWTICO.csv")

# Step 1: 轉換日期欄位格式
AU <- AU %>% mutate(Date = as.Date(period))
OIL <- OIL %>% mutate(Date = as.Date(observation_date))

# Step 2: 建立完整日期序列（1968-01-01 到 2025-05-10）
full_dates <- tibble(Date = seq.Date(from = as.Date("2010-01-01"),
                                     to   = as.Date("2025-01-11"),
                                     by = "day"))

# Step 3: 對 AU 和 OIL 進行 full_join 補齊缺失日期，填上 NA
AU_full <- full_dates %>%
  left_join(AU, by = "Date")

OIL_full <- full_dates %>%
  left_join(OIL, by = "Date")

# Step 4: 合併為一個表格（可選）
AU_OIL <- AU_full %>%
  left_join(OIL_full, by = "Date")
colnames(AU_OIL)<- c("Date", "period", "GoldPrice", "Date2", "OilPrice")

# 資料預處理
# Step 5: 補齊與插補 GoldPrice 與 OilPrice（請確認欄位名稱）

AU_OIL <- AU_OIL %>%
  arrange(Date) %>%
  mutate(GoldPrice_interp = na.approx(GoldPrice, na.rm = FALSE)) %>%
  mutate(GoldPrice_interp = na.locf(GoldPrice_interp, na.rm = FALSE)) %>%
  mutate(GoldPrice_interp = na.locf(GoldPrice_interp, fromLast = TRUE)) %>%
  mutate(OilPrice_interp = na.approx(OilPrice, na.rm = FALSE)) %>%
  mutate(OilPrice_interp = na.locf(OilPrice_interp, na.rm = FALSE)) %>%
  mutate(OilPrice_interp = na.locf(OilPrice_interp, fromLast = TRUE))
head(AU_OIL)


write.csv(AU_OIL, "整理過後的資料.csv")


Data_daily <- read_excel("~/政大課堂/計量經濟學（二）/期末報告/原始資料集/dailydata.xlsx",  sheet = "Daily")


colnames(Data_daily)
head(Data_daily)
Data_daily$Date <- as.Date(Data_daily$observation_date)
Data_daily1  <- subset(Data_daily, Date >= as.Date("2010-01-01"))



# 使用線性插值補值「10年國債市場價格」
# 合併兩個資料框
# 以 AU_OIL 為主體進行合併
merged_data <- AU_OIL %>%
  left_join(Data_daily, by = "Date")

merged_data_interp <- cbind(merged_data$GoldPrice_interp, merged_data$`10年國債市場價格`)
colnames(merged_data_interp) <- c("Gold", "Bond")
merged_data_interp <-as.data.frame(merged_data_interp)
merged_data_interp$Bond <- na.approx(merged_data_interp$Bond, na.rm=F)
#

merged_data_interp <- merged_data_interp[which( complete.cases(merged_data_interp)==T ), ]
