#### Data transformation for traffic channels


library(readr)
TrafficType_Revenue_Transaction <- read_csv("Downloads/R_shiny_app_GoogleTrends/Seleted dataset/TrafficType_Revenue_Transaction.csv", 
                                            col_types = cols(`Day Index` = col_character()), 
                                            skip = 5)
View(TrafficType_Revenue_Transaction)

traffic <- TrafficType_Revenue_Transaction
#
traffic$Revenue <- substring(traffic$Revenue, first = 2) # subset characters beginning from 1nd string to take out $ sign
View(traffic)


traffictype_revenue_spread <- traffic%>%
  select(-2, -5)%>%
  spread(key = Segment, value = Revenue)%>%
  rename("Direct_traf_revenue" = "Direct Traffic", "Organic_traf_revenue" = "Organic Traffic", 
         "Paid_traf_revenue" = "Paid Traffic", "Referral_traf_revenue" = "Referral Traffic")%>%
  na.omit()

#traffictype_revenue_spread[, "Date"] = usertype_data_union[,1]



######## clean the data and remove symbols from the rows and leave only numbers and characters
# usertype_revenue$Revenue <- substring(usertype_revenue$Revenue, first = 2) # subset characters beginning from 1nd string to take out $ sign
# View(usertype_revenue)

# Dataset$`Ecommerce Conversion Rate` <- substr(Dataset$`Ecommerce Conversion Rate`, 1, nchar(Dataset$`Ecommerce Conversion Rate`) - 1) # retains 1st to 4th characters and leaveout % sign
# Dataset$`Bounce Rate` <- substr(Dataset$`Bounce Rate`, 1, nchar(Dataset$`Bounce Rate`) - 1) # retains 1st to 4th characters and leaveout % sign
# View(Dataset)
########

#extract first 5 chars
Tchar_first5 <- substr(traffictype_revenue_spread$`Day Index`, 1, 5)  ## use for Day Index col chracters of length 7

# extarct first 6 chars
Tchar_first6 <- substr(traffictype_revenue_spread$`Day Index`, 1, 6)  ## use for Day index col characters of length 8

#extract last 2 chars
Tchar_last2 <- substring(traffictype_revenue_spread$`Day Index`, first = nchar(traffictype_revenue_spread$`Day Index`) - 1) ## concatenate at the end of values in Day Index colume
Tchar_last2

################ Formating Day Index col with 7 characters into Date format ####################
Tfilter_char7 <- filter(traffictype_revenue_spread, nchar(traffictype_revenue_spread$`Day Index`) == 7) ## filter Day Index col with 7 strings
Tfilter_char7_first5 <- substr(Tfilter_char7$`Day Index`, 1, 5)  ## Extract 1st 5 strings from col 
Tfilter_char7_last2 <- substring(Tfilter_char7$`Day Index`, first = nchar(Tfilter_char7$`Day Index`) - 1) #Extract last 2 strings of dataset in Day Index colume

Tfilter_char7Re <- mutate(Tfilter_char7, fill_char7Date = paste0("0", Tfilter_char7_first5, "20", Tfilter_char7_last2)) #create col and concatenate strings into it to form Date
#colnames(Tfilter_char7Re)[5] <- "Date" ## rename col as Date
View(Tfilter_char7Re)

View(Tfilter_char7)
View(Tfilter_char7_last2)

################ Formating Day Index col with 8 characters into Date format ####################
Tfilter_char8 <- filter(traffictype_revenue_spread, nchar(traffictype_revenue_spread$`Day Index`) == 8) ## filter Day Index col with 8 strings
Tfilter_char8_first6 <- substr(Tfilter_char8$`Day Index`, 1, 6)  ## Extract 1st 6 strings from col 
Tfilter_char8_last2 <- substring(Tfilter_char8$`Day Index`, first = nchar(Tfilter_char8$`Day Index`) - 1) #Extract last 2 strings of dataset in Day Index colume

Tfilter_char8Re <- mutate(Tfilter_char8, fill_char8Date = paste0(Tfilter_char8_first6, "20", Tfilter_char8_last2)) #create col and concatenate strings into it to form Date
#colnames(Tfilter_char8Re)[5] <- "Date" ## rename col as Date

View(Tfilter_char8)
View(Tfilter_char8Re)
#colnames(filter_char)

################ Formating Day Index col with 10 characters into Date format ####################
# Tfilter_char10 <- filter(traffictype_revenue_spread, nchar(traffictype_revenue_spread$`Day Index`) == 10) ## filter Day Index col with 10 strings
# Tfilter_char10["Date"] <- Tfilter_char10$`Day Index` ## create Date col and assign the values of Day Index col to it

########binding rows of datasets  ##############################################
GA_Traffic_revenue_Dataset <- bind_rows(Tfilter_char7Re, Tfilter_char8Re) ## bind rows of all datasets filtered
View(GA_Traffic_revenue_Dataset)
