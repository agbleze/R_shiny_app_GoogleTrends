####################### Combine Date and Averages of data   ##############
dataframe_calendAdjust <- data.frame(data_calendAdjust_ts)
selectavg <- dplyr::select(dataframe_calendAdjust, Avg_revenue, Avg_ECR, Avg_Users, Avg_bounce_rate, Avg_session_duration, 
                           Avg_sessionPer_user, Avg_Pg_session, Avg_session)

select_yr_mth <- select(data_select_sum, Year, Month)
dataset_avg <- cbind.data.frame(select_yr_mth, selectavg)


test <- dataset_avg%>%
  filter(Year == 2017 & Month == "Dec")%>%
  select(Avg_revenue)

trygauge <- googleVis::gvisGauge(test, options=list(min=0, max=800, greenFrom=500,
                                                    greenTo=800, yellowFrom=300, yellowTo=500,
                                                    redFrom=0, redTo=300))

plot(trygauge)
Gauge1 <- gvisGauge(CityPopularity[2,], options=list(min=0, max=800, greenFrom=500,
                                                     greenTo=800, yellowFrom=300, yellowTo=500,
                                                     
                                                     redFrom=0, redTo=300))
####### preparing single display for google gauge
Name_col <- ""
column_combine <- cbind(Name_col, test)

gauge2 <- gvisGauge(column_combine, options=list(min=0, max=20000, greenFrom=5000,
                                                 greenTo=20000, yellowFrom=3000, yellowTo=5000,
                                                 redFrom=0, redTo=3000))
plot(gauge2)
plot(Gauge1)
#########################
