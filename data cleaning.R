############################### full dataset for export #############################
user_year_month <- usertypedata_mean[, c(1,2)]
tryial <-  dataframe_calendAdjust%>%
  mutate("Year" = user_year_month[,1], "Month" = user_year_month[,2])

colnames(tryial[, 19]) <- "Year" 
colnames(tryial[, 20]) <- "Month"
full_data <- tryial[, c(3:20)]%>%
  relocate(Year, .before = ..Total_Users)%>%
  relocate(Month, .after = Year)
write.csv(full_data, file = "full_data.csv")
###################################################################################

