####### forecasting with time series decomposition

library(seasonal)
library(forecast)
library(magrittr)
library(bfast)
library(foreach)
library(ggplot2)

## time series decomposition
data_calendAdjust_ts
(revdecompose_add <- decompose(data_calendAdjust_ts[,15], type = "additive")%>%
  autoplot())

(revdecompose_mult <- decompose(data_calendAdjust_ts[,15], type = "multiplicative") %>%
  autoplot()) 

## moving averages
ma(data_calendAdjust_ts[,15], order = 5, centre = T) %>%  #USE ma to extract trend
  autoplot()

## stl decompose
rev_stl <- stl(data_calendAdjust_ts[,15], t.window = 5, s.window = "periodic", robust = T)

## using stl decomposing for naive forecasting
 rev_stl%>%
   seasadj() %>%  ## seasonal adjustement to remove seasonal component from the data
   naive() %>% ## naive forecast on seasonally adjusted component
   autoplot() + ylab("Revenue") + ggtitle("Seasonally adjsuted time series of Revenue")
 
 #### stlf() forecasting
(rev_stlf_meth <-  stlf(data_calendAdjust_ts[,15]) %>%
  autoplot() + ylab("Revenue"))
 
 ## decomposing with X11 method
 rev_x11 <- seas(data_calendAdjust_ts[,15])

 ## ploting seasonal adjusted and trendcycle from the x11 method
autoplot(data_calendAdjust_ts[,15], series = "Data") +
  autolayer(trendcycle(rev_x11), series = "Trend") +
  autolayer(seasadj(rev_x11), series = "Seasonal adjusted") +ylab("Revenue") +
  ggtitle("Trend and seasonal plots of revenue") + 
  scale_colour_manual(values = c("green", "blue", "red"),
                      breaks = c("Data", "Seasonal adjusted", "Trend"))


##### BFAST 
## timeseries is decomposed into trend, seasonality and remainder and breaks are detected
## dashboad allows for users to select the parameter of interest and BFAST is run

(data_bfast <- bfast::bfast(data_calendAdjust_ts[,15], decomp = "stl"))

plot(data_bfast)

(data_bfast01 <- bfast01(data_calendAdjust_ts[,15]))  ## for deciding on which model to select

(data_bfast01classify <- bfast01classify(data_bfast01))  ## A function to determine the change type

(data_bfastmonitor <- bfastmonitor(data_calendAdjust_ts[,15], start = c(20014, 11), plot = T))  ### Monitoring disturbances in time series models (with trend/season/regressor terms) at the end of time
#series



# bfast::bfastpp(data_calendAdjust_ts) %>%
#   bfast()%>%
#   bfast::bfast01classify()
