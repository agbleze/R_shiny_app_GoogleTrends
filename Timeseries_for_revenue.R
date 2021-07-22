### load libraries  ######
library(readr)
library(forecast)
library(tidyverse)
library(ggplot2)
library(fpp2)
library(lubridate)

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
ggseasonplot(revlon_ts[,3])
ggseasonplot(revlon_ts[,3], polar = T)
revlon
autoplot(revlon_ts[,3])
ggAcf(revlon_ts[,3])
ggsubseriesplot(revlon_ts[,3])
gglagplot(revlon_ts[,3])

Box.test(revlon_ts[,3], type = "Lj", fitdf = 0)
