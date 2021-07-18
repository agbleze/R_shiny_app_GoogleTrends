library(readxl)
library(tidyverse)
library(ggplot2)

library(readr)
Dataset<- read_csv("Seleted dataset/Dataset_GoogleAnalytics .csv", 
                                     col_types = cols(`Day Index` = col_character(), 
                                                      Users = col_number(), `Avg. Session Duration` = col_time(format = "%H:%M:%S"), 
                                                      `Number of Sessions per User` = col_double(), 
                                                      `Pages / Session` = col_double(), 
                                                      Revenue = col_character(), `Ecommerce Conversion Rate` = col_character()))
View(Dataset)

######## clean the data and remove symbols from the rows and leave only numbers and characters
Dataset$Revenue <- substr(Dataset$Revenue, 2, 1000000L) # subset numerical characters beginning from 2nd string and take out $ sign
Dataset$`Ecommerce Conversion Rate` <- substr(Dataset$`Ecommerce Conversion Rate`, 1, 4) # retains 1st to 4th characters and leaveout % sign
Dataset$`Bounce Rate` <- substr(Dataset$`Bounce Rate`, 1, 4) # retains 1st to 4th characters and leaveout % sign
View(Dataset)
########

#extract first 5 chars
char_first5 <- substr(Dataset$`Day Index`, 1, 5)  ## use for Day Index col chracters of length 7

# extarct first 6 chars
char_first6 <- substr(Dataset$`Day Index`, 1, 6)  ## use for Day index col characters of length 8

#extract last 2 chars
char_last2 <- substring(Dataset$`Day Index`, first = nchar(Dataset$`Day Index`) - 1) ## concatenate at the end of values in Day Index colume
char_last2

################ Formating Day Index col with 7 characters into Date format ####################
filter_char7 <- filter(Dataset, nchar(Dataset$`Day Index`) == 7) ## filter Day Index col with 7 strings
filter_char7_first5 <- substr(filter_char7$`Day Index`, 1, 5)  ## Extract 1st 5 strings from col 
filter_char7_last2 <- substring(filter_char7$`Day Index`, first = nchar(filter_char7$`Day Index`) - 1) #Extract last 2 strings of dataset in Day Index colume
  
filter_char7Re <- mutate(filter_char7, fill_char7Date = paste0("0", filter_char7_first5, "20", filter_char7_last2)) #create col and concatenate strings into it to form Date
colnames(filter_char7Re)[10] <- "Date" ## rename col as Date
View(filter_char7Re)

View(filter_char7)
View(filter_char7_last2)

################ Formating Day Index col with 8 characters into Date format ####################
filter_char8 <- filter(Dataset, nchar(Dataset$`Day Index`) == 8) ## filter Day Index col with 8 strings
filter_char8_first6 <- substr(filter_char8$`Day Index`, 1, 6)  ## Extract 1st 6 strings from col 
filter_char8_last2 <- substring(filter_char8$`Day Index`, first = nchar(filter_char8$`Day Index`) - 1) #Extract last 2 strings of dataset in Day Index colume

filter_char8Re <- mutate(filter_char8, fill_char8Date = paste0(filter_char8_first6, "20", filter_char8_last2)) #create col and concatenate strings into it to form Date
colnames(filter_char8Re)[10] <- "Date" ## rename col as Date


View(filter_char8)
View(filter_char8Re)
##########################

################ Formating Day Index col with 8 characters into Date format ####################
filter_char10 <- filter(Dataset, nchar(Dataset$`Day Index`) == 10) ## filter Day Index col with 10 strings
filter_char10["Date"] <- filter_char10$`Day Index` ## create Date col and assign the values of Day Index col to it

########binding rows of datasets  ##############################################
Complete_Dataset <- bind_rows(filter_char7Re, filter_char8Re, filter_char10) ## bind rows of all datasets filtered
View(Complete_Dataset)

#### Exporting dataset as csv
write.csv(Complete_Dataset, file = "Users\\lin\\Downloads\\R_shiny_app_GoogleTrends\\complete_clean_Dataset.csv")

View(filter_char10)
