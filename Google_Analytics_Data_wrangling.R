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

new <- Dataset[, 1] 
View(new)
head(new)
again <- select(Dataset, `Day Index`)
View(again)
repl2014 <- gsub(pattern = "[2014]", "14", again)
repl2014 <- data.frame(repl2014)
View(repl2014)
#############
valgr <- grep(pattern = "\\$", Dataset)
Dataset["Revenue"] [Dataset["Revenue"] == valgr ]<- ""
View(Dataset)
Data_vec <- as.vector(Dataset)
Data_Vec <- sub("$", "", Data_vec)
Data_Vec <- sub("%", "", Data_Vec)
Data_Vec_frame <- as.data.frame(Data_Vec["Revenue"])
View(Data_Vec_frame)
df["Column Name"][df["Column Name"] == "Old Value"] <- "New Value"


df$filname <- substr(df$filname, 0, 3)
########
Dataset$Revenue <- substr(Dataset$Revenue, 2, 1000000L)
Dataset$`Ecommerce Conversion Rate` <- substr(Dataset$`Ecommerce Conversion Rate`, 1, 4)
Dataset$`Bounce Rate` <- substr(Dataset$`Bounce Rate`, 1, 4)
View(Dataset)
########
# Day_Index = if(nchar(Dataset$`Day Index`) > 8){
#   substring(Dataset$`Day Index`, )
# }

##
## Add colume for Day n if number of string is more than 8
## past

#extract first 5 chars
char_first5 <- substr(Dataset$`Day Index`, 1, 5)  ## use for chracters of length 7

# extarct first 6 chars
char_first6 <- substr(Dataset$`Day Index`, 1, 6)  ## use for characters of length 8

#extract last 2 chars
char_last2 <- substring(Dataset$`Day Index`, first = nchar(Dataset$`Day Index`) - 1) ## concatenate at the end of values in Day Index colume
char_last2

## statement to find char length and apply diffent function

Dataset_formatedn <- if(any(nchar(Dataset$`Day Index`)) == 7){
  mutate(Dataset, New_Date = paste0(char_first5 + "20" + char_last2))
} 
# else if(nchar(Dataset$`Day Index`) == 8){
#   mutate(Dataset, New_Date = paste0(char_first6 + "20" + char_last2))
# } else {
#   mutate(Dataset, New_Data = Dataset$`Day Index`)
# }

loop_data <- for(i in nrow(Dataset)){
  if(nchar(Dataset$`Day Index`) == 7){
    mutate(Dataset, New_Date = paste0(char_first5 + "20" + char_last2))
  } else if(nchar(Dataset$`Day Index`) == 8){
    mutate(Dataset, New_Date = paste0(char_first6 + "20" + char_last2))
  } else {
    mutate(Dataset, New_Data = Dataset$`Day Index`)
  }
}


x <- Dataset$`Day Index`
format_Day_Index <- function(x){if(nchar(x) == 7){
  mutate(Dataset, New_Date = paste0(char_first5 + "20" + char_last2))
} else if(nchar(x) == 8){
  mutate(Dataset, New_Date = paste0(char_first6 + "20" + char_last2))
} else {
  mutate(Dataset, New_Data = x)
}
}
new_lappy_data <- lapply(Dataset, function(x) if(nchar(x) == 7){
  mutate(Dataset, New_Date = paste0(char_first5, "20", char_last2))
} else if(nchar(x) == 8){
  mutate(Dataset, New_Date = paste0(char_first6, "20", char_last2))
} else {
  mutate(Dataset, New_Data = x)
})

##################### char 7
filter_char7 <- filter(Dataset, nchar(Dataset$`Day Index`) == 7)
filter_char7_first5 <- substr(filter_char7$`Day Index`, 1, 5)  ## use for chracters of length 7
filter_char7_last2 <- substring(filter_char7$`Day Index`, first = nchar(filter_char7$`Day Index`) - 1) ## concatenate at the end of values in Day Index colume

  
filter_char7Re <- mutate(filter_char7, fill_char7Date = paste0("0", filter_char7_first5, "20", filter_char7_last2))
colnames(filter_char7Re)[10] <- "Date"
View(filter_char7Re)

View(filter_char7)
View(filter_char7_last2)
###################################

########################## char 8
filter_char8 <- filter(Dataset, nchar(Dataset$`Day Index`) == 8)
filter_char8_first6 <- substr(filter_char8$`Day Index`, 1, 6)  ## use for characters of length 8
filter_char8_last2 <- substring(filter_char8$`Day Index`, first = nchar(filter_char8$`Day Index`) - 1) ## concatenate at the end of values in Day Index colume

filter_char8Re <- mutate(filter_char8, fill_char8Date = paste0(filter_char8_first6, "20", filter_char8_last2))
colnames(filter_char8Re)[10] <- "Date"


View(filter_char8)
View(filter_char8Re)
##########################

################# char 10
filter_char10 <- filter(Dataset, nchar(Dataset$`Day Index`) == 10)
filter_char10["Date"] <- filter_char10$`Day Index`

########binding rows of dataset
Complete_Dataset <- bind_rows(filter_char7Re, filter_char8Re, filter_char10)
View(Complete_Dataset)

write.csv(Complete_Dataset, file = "Users\\lin\\Downloads\\R_shiny_app_GoogleTrends\\complete_clean_Dataset.csv")

write.csv(df,"C:\\Users\\Ron\\Desktop\\Test\\People.csv", row.names = FALSE)
#add_column(filter_char10, )
View(filter_char10)

# Dataset_fun <- apply(Dataset, Dataset$`Day Index`, function(x){
#   if(nchar(Dataset$`Day Index`) == 7){
#     mutate(Dataset, New_Date = paste0(char_first5 + "20" + char_last2))
#   } else if(nchar(Dataset$`Day Index`) == 8){
#     mutate(Dataset, New_Date = paste0(char_first6 + "20" + char_last2))
#   } else {
#     mutate(Dataset, New_Data = Dataset$`Day Index`)
#   }
# } )

## check if the number of strings are more than 8 then remove last 2 strings
# if first string == 0 then remove
# mutate a colume and in it paste all strings until the last 2 + paste 20 + the last 2

chartr(old = "2014", new = "14", again)
str_ends(again[["Day Index"]], pattern = "2014")

again2014 <- as.data.frame(str_replace_all(again[["Day Index"]], pattern = "2014", replacement = "14"))
#col.names(again2014) <- "Day_Index"
#View(as.data.frame(again2014))
View(again2014)
again2015 <- as.data.frame(str_replace_all(again[["Day Index"]], pattern = "2015", replacement = "15"))
View(again2015)
#newdata <- for(i in again[["Day Index"]]){
datp <- as.data.frame(str_replace_all(again2014, pattern = "2015", replacement = "15"))
View(datp) 
#}
View(as.data.frame(datp))
filtDat <- filter(again, again["Day Index"] == "2015")
View(filtDat)

remove_cur <- as.data.frame(gsub(pattern = "\\$", replacement = "", Dataset$Revenue))
View(remove_cur)
colnames(remove_cur) = "Revenue"
View(remove_cur)
Bounce_R <- as.data.frame(gsub(pattern = "\\%", "", Dataset))
View(Bounce_R)
ggplot(Bounce_R, mapping = aes(x = "Day Index", y = "Revenue")) + geom_line()

Dataset<-  str_replace_all(Dataset, pattern = "\\%", "")

View(Dataset)
###########
datasublet <- select(Dataset, "Users", Sessions)
ggplot(datasublet, mapping = aes(x = log(Users), y = log(Sessions))) + geom_point()
ggplot(Dataset) + geom_violin(mapping = aes(x = "Day Index", y = "Sessions"))
