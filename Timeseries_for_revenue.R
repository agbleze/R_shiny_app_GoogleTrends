### load libraries  ######
library(readr)
library(forecast)
library(tidyverse)
library(ggplot2)
library(fpp2)
library(lubridate)
library(GGally)

GA_Dataset <- read_csv("Downloads/R_shiny_app_GoogleTrends/Users\\lin\\Downloads\\R_shiny_app_GoogleTrends\\GA_clean_Dataset.csv", 
                       col_types = cols(`Avg. Session Duration` = col_time(format = "%H:%M:%S"), 
                                        Date = col_date(format = "%m/%d/%Y")))
View(GA_Dataset)
data_subset <- select(GA_Dataset, Revenue, `Ecommerce Conversion Rate`, Date)
data_ts <- as.ts(data_subset)
forecast::autoplot(data_subset$Revenue, start = 2014, frequency = 12)

gropdata <- group_by(data_subset, by = Date)
View(gropdata)
revenue_data <- select(GA_Dataset, Revenue, Date)
year_re <- lubridate::year(revenue_data$Date)
month_re <- lubridate::month(revenue_data$Date, label = T)
revenue_data_tm <- revenue_data%>%
  mutate(Year = year_re, Month = month_re)
# 
# rev2014 <- revenue_data_tm%>%
#   filter(Year == 2014)%>%
#   group_by(Month)%>%
#   summarise(Total_Revenue = sum(Revenue))
# 
# rev2015 <- revenue_data_tm%>%
#   filter(Year == 2015)%>%
#   group_by(Month)%>%
#   summarise(Total_Revenue = sum(Revenue))

revenue_total <- revenue_data_tm%>%
  group_by(Year, Month)%>%
  summarise(Total_revenue = sum(Revenue))

revenue_ts <- as.ts(revenue_total)
autoplot(revenue_ts, fre)
forecast::autoplot(revenue_ts, frequency = 12)

xlim_val <- reve_spread$Year

reve_spread <- revenue_total%>%
  filter(Year > 2014 & Year < 2021) %>%
  spread(key = Month, value = Total_revenue)
reve_spread_ts <- as.ts(reve_spread)

reve_spread <- revenue_total%>%
  filter(Year > 2014 & Year < 2021) %>%
  spread(key = Month, value = Total_revenue)

reve_spread_ts <- as.ts(reve_spread)

autoplot(reve_spread_ts, xlab = "years") + xlim("2015", "2016", "2017", "2018", "2019", "2020")

# ## seasonal plots
# reve_spread %>%
#   select(-Year) %>%
#   as.ts() %>%
#   ggseasonplot()
# 
ggAcf(reve_spread_ts, lag = 5)

ggsubseriesplot(reve_spread_ts[, c(2)])


### change column (Year) to rownames
again <- textshape::column_to_rownames(reve_spread, loc = 1)
View(again)  
again_ts <- ts(again, frequency = 2)
ggseasonplot(again_ts)

autoplot(again_ts)
ggseason

revlon <- revenue_total
# %>%
#   select(Total_revenue)

revlon_ts <- ts(revlon, start = c(2014,11), frequency = 12)
ggseasonplot(revlon_ts[,3], year.labels = T, year.label.left = T, year.label.right = T)
# the seasonal plots shows revenue is at its maximum of the year in December
ggseasonplot(revlon_ts[,3], polar = T)
revlon

# Time series plot
autoplot(revlon_ts[,3]) ### The graph does not show any visible clear trend, seasonality in 
## revenue can be deduce upon a closer inspection. Revenue ususally reaches the max of the year
## at the later months probably during the festive seasons of Christmas. Revenue also falls to 
## the minimum during the early months on a new year

(auto_corplot <- ggAcf(revlon_ts[,3])) ## plots of autocorrelation

(auto_cor <- ggAcf(revlon_ts[,3], plot = F)) ## values of autocorrelation indicates correlation between time lags
## the results show a consistent trend of decreasing autocorrelation co-efficient.This shows the data has
# a trend and seasonality. lag 1 to 12 
# and lag 19 to 24 are statistically significant. Lag 13 to 18 are not different from 0

ggsubseriesplot(revlon_ts[,3])
gglagplot(revlon_ts[,3])  ## lag 1,2,3,4 shows a positive relationship indicating the strong
## element of seasonality in the observation.

Box.test(revlon_ts[,3], type = "Lj", fitdf = 0)

## 
ggpairs(GA_Dataset[,c(2, 4:9)])

############################## transform and adjust data  ###############################
############## calendar adjustment
revlon_transform_ts <- cbind(revlon_ts,
                       avgmonthly_rev = revlon_ts[,3] / monthdays(revlon_ts[,3]))
  
autoplot(revlon_transform_ts[,4])  ## values of autocorrelation indicates correlation between time lags
## the results show a consistent trend of decreasing autocorrelation co-efficient.This shows the data has
# a trend and seasonality. lag 1 to 12 
# and lag 19 to 24 are statistically significant. Lag 13 to 18 are not different from 0

(season_revtransform <- ggseasonplot(revlon_transform_ts[,4], year.labels = T, year.label.left = T, year.label.right = T))
# the seasonal plots shows revenue is at its maximum of the year in December
(season_revtransform_polar <- ggseasonplot(revlon_transform_ts[,4], polar = T))

(autocorplot_revtransform <- ggAcf(revlon_transform_ts[,4])) ## plots of autocorrelation

(subseries_revtransform_ts <- ggsubseriesplot(revlon_transform_ts[,4]))
(lag_revtransform_ts <- gglagplot(revlon_transform_ts[,4]))  ## lag 1,2,3,4 shows a positive relationship indicating the strong
## element of seasonality in the observation.

## test the the “overall randomness” based on a number of lags. 
# the result is p-value < 0.001 hence data are probably not white noise. hence seasonal and trend is present
(Ljungtest <- Box.test(revlon_transform_ts[,4], type = "Lj", lag = 24))

######### forecasting   #############
(train_revtransform_forecast <- window(revlon_transform_ts, start = c(2014, 11), end = c(2018, 6)))
(test_revtransform_forecast <- window(revlon_transform_ts, start = c(2018, 7)))

### forecasting method of meanf(), naive(), snaive(), rwf()
(rev_meanf <- meanf(train_revtransform_forecast[,4], h = 10))
(rev_naive <- naive(train_revtransform_forecast[,4], h = 10))
(rev_snaive <- snaive(train_revtransform_forecast[,4], h = 10))
(rev_rwfdrift <- rwf(train_revtransform_forecast[,4], h = 10, drift = TRUE))
#(rev_rwf <- rwf(train_revtransform_forecast[,4], h = 10, drift = FALSE))

## plot forecast
autoplot(train_revtransform_forecast[,4]) + theme_dark() +
  autolayer(rev_meanf, series = "Mean", PI = FALSE) +
  autolayer(rev_naive, series = "Naive", PI = T, ) +
  autolayer(rev_snaive, series = "Seasonal Naive", PI = T, alpha = 0.3) +
  autolayer(rev_rwf, series = "rwf", PI = FALSE) +
  autolayer(rev_rwfdrift, series = "Drift", PI = FALSE) +
  ggtitle("Various forecast for Revenue") +
  guides(colour = guide_legend(title = "Forecast")) + ylab("Average Monthly Revenue")
  

##### evaluate forecast accurancy
(accur_meanf <- accuracy(rev_meanf, test_revtransform_forecast[,4]))
(accur_naive <- accuracy(rev_naive, test_revtransform_forecast[,4])) ## have a better accuracy compared to other methods
(accur_snaive <- accuracy(rev_snaive, test_revtransform_forecast[,4]))
(accur_rwf <- accuracy(rev_rwf, test_revtransform_forecast[, 4]))
(accur_drift <- accuracy(rev_rwfdrift, test_revtransform_forecast[, 4]))

#### cal residuals for various forecasts
(res_meanf <- residuals(rev_meanf))
(res_naivef <- residuals(rev_naive))
#residuals(rev_rwf)
(res_drift <- residuals(rev_rwfdrift))
(res_snaive <- residuals(rev_snaive))

## results of residuals
summary(res_meanf)
summary(res_naivef)
summary(res_snaive)
summary(res_drift)

## checkresiduals for portmanteau test
(rev_meanf_checkresiduals <- checkresiduals(rev_meanf)) ##  mean model shows an 
## apparent pattern in the residual time series plot, the ACF plot shows several lags exceeding 
## the 95% confidence interval, and the Ljung-Box test has a statistically significant p-value 
## suggesting the residuals are not purely white noise. Thus, not all signals in the data are adequately 
## captured in this model 


(rev_naive_checkresiduals <- checkresiduals(rev_naive))
(rev_rwfdrift_checkresiduals <- checkresiduals(rev_rwfdrift))

(rev_snaive_checkresiduals <- checkresiduals(rev_snaive))  ## better model than others
# residuals from seasonal naive plot appear to be white noise with no clear pattern
# the lagplot has only a couple of lags that exceeding the 95% confidence interval, 
# residual histogram plot shows approximately normally residuals distribution
# Ljung-Box test results give a p-value of 0.1296 hence residuals have no statistically significant 
# difference at 0.01 and 0.05 sig level compared to white noise. This model captures all (or most) of the available signal in the data.

### time series cross validation 
(error_naive <- tsCV(revlon_transform_ts[,4], forecastfunction = naive, h = 10))
(RMSE_naive <- sqrt(mean(error_naive^2, na.rm = T))) ## Naive forecast model has least RMSE hence better accuracy compared to others

(error_snaive <- tsCV(revlon_transform_ts[,4], forecastfunction = snaive, h = 10))
(RMSE_snaive <- sqrt(mean(error_snaive^2, na.rm = TRUE)))

(error_meanf <- tsCV(revlon_transform_ts[,4], forecastfunction = meanf, h = 10))
(RMSE_meanf <- sqrt(mean(error_meanf^2, na.rm = TRUE)))

summary(RMSE_naive)
summary(RMSE_snaive)

## compare RMSE of residual (of full dataset) to RMSE of tsCV 
## for demonstration purposes, we realized that RMSE  of residuals in naive forecast are > RMSE of tsCV with naive forecast
(allData_naive_residual <- residuals(naive(revlon_transform_ts[,4])))
(RMSE_naive_allData <- sqrt(mean(allData_naive_residual^2, na.rm = T)))

####### Conclusion of models   #####
### snaive forecast model captures all variation in the data hence not statistically diff from white noise
## contary to naive which is does not capture on the variability in the data
## Nonetheless, naive forecast has a better accuracy for prediction compared to snaive



