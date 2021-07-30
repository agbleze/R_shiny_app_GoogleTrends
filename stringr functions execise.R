####### using stringr for string manipulation
# detecting pattern
library(stringr)
str_detect(state.name, pattern = "New")
str_count(state.name, pattern = "New")
sum(str_detect(state.name, pattern = "New")) ## count total number of matches

## locate occurance of patterns
x <- c("abcd", "a22bc1d", "ab3453cd46", "a1bc44d")
str_locate(x, "[0-9]+") # loacte the first instance of numbers
str_locate_all(x, "[0-9]+")

## extracting patterns 
simp <- c("gameon", "chamelone", "23jersey")
str_extract(simp, pattern = "me{1,}") ## extract the first occurance of the pattern
str_extract_all(simp, pattern = "[[:alpha:]]|[3+]") ## extract all occurance of the pattern

## replacing patterns in string
daten <- c("12/2/2012", "12/3/2019", "23/09/2012", "1/01/2001")
reptn <- c("12")
str_replace(daten, pattern = "2012", replacement = reptn) ## replace first instance of pattern
str_replace_all(simp, pattern = "e*", replacement = "!!") ## replace all instnace of the pattern

## spliting patterns in string
ans <- str_split(simp, pattern = "[aeiou]") 
unlist(ans) ## atomic vector
