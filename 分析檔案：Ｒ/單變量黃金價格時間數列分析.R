##
source("data_cleaning.R")

##library
library(tseries)
library(aTSA)


# 時間向量與價格
dates <- AU_OIL$Date
gold <- AU_OIL$GoldPrice_interp

# 繪圖（線圖，不畫 x 軸）
plot(dates, gold, type = "l",
     main = "Gold Price Time Series",
     ylab = "Gold Price",
     xlab = "Date",
     xaxt = "n")  # 不畫預設 x 軸

# 建立每年 1 月 1 日的日期向量
years <- seq(from = as.Date("2010-01-01"), to = as.Date("2025-01-01"), by = "year")

# 標上年份作為 x 軸刻度
axis(side = 1, at = years, labels = format(years, "%Y"))
















###########################
log_gold <- log(AU_OIL$GoldPrice_interp)

model1 <- arima(diff(log_gold), order = c(1,0,1), include.mean = FALSE)
fit1 <- fitted(model1)
log_gold_fitted_1 <- cumsum(c(log_gold[1], fit1))  # ✅ 正確還原

model2 <- arima(log_gold, order = c(1,1,1), include.mean = FALSE)
fit2 <- fitted(model2)
log_gold_fitted_2 <- cumsum(c(log_gold[1], fit2))  # ✅ 正確還原

min_len <- min(length(log_gold_fitted_1), length(log_gold_fitted_2))
diff_fitted <- log_gold_fitted_1[1:min_len] - log_gold_fitted_2[1:min_len]

summary(diff_fitted)
max(abs(diff_fitted))  # 預期在 1e-12 以內




log_gold <- log(AU_OIL$GoldPrice_interp)
model <- arima(log_gold, order = c(1,1,1), include.mean = FALSE)
fit <- fitted(model)
length(fit)  # 應該少於 length(log_gold)
resid <- residuals(model)

# 預測值 + 殘差 ≈ 差分後 log_gold
diff_log_gold <- diff(log_gold)
check <- fit + resid
all.equal(check, diff_log_gold[tail(seq_along(diff_log_gold), length(fit))])



