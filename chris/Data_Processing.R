rm(list = ls())
setwd("~/Documents/GitHub/stat440-Covid-19/chris")
library(lubridate)

train = read.csv("train2.txt",sep = ",",header = T)
test = read.csv("test2.txt",sep = ",",header = T)

clean_data = function(df){
  # 1.Exclude outcome column
  df$outcome = NULL
  # 2.Exclude age,city,province for now.
  df$city = NULL
  df$province = NULL           #cannot use province in Random Forest because there're more than 53 categories
  # 3.class(confirmed) <- Date
  df$confirmed <- as.Date(df$confirmed, format = "%d.%m.%y")
  # 4.fill the NA in confirmed with the mean
  df$confirmed[is.na(df$confirmed)] <- mean(df$confirmed,na.rm = TRUE)
  
  # Age column processing from Lloyd
  df$age = as.character(df$age)
  ages = c(test$age, df$age)
  ages = ages[!grepl('-', ages)]
  ages = as.numeric(ages)
  mu = mean(ages[!is.na(ages)])
  
  for (i in 1:dim(df)[1]) {
    x = df$age[i]
    if (x == '') {
      x = mu
      df$age[i] = x
    } else if (grepl('-', x, fixed = TRUE)) {
      z = strsplit(as.character(x), '-')
      x = mean(as.numeric(c(z[[1]][1], z[[1]][2])))
      df$age[i] = x
    } 
  }
  df$age = as.numeric(df$age)
  
  df$symptoms_number = NULL
  df$is_fever = NULL
  df$is_cough = NULL
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  df$symptoms = trim(df$symptoms)
  for (i in 1:dim(df)[1]) {
    x = df$symptoms[i]
    if(x == ''){
      df$symptoms_number[i] = 0
    }else {
      df$symptoms_number[i] = lengths(regmatches(x, gregexpr(";", x))) + 1
    }
    if(grepl("fever",x)){
      df$is_fever[i] = TRUE
    }else {
      df$is_fever[i] = FALSE
    }
    if(grepl("cough",x)){
      df$is_cough[i] = TRUE
    }else {
      df$is_cough[i] = FALSE
    }
    if((df$is_cough[i] == TRUE) & (df$is_fever[i] == FALSE)){
      df$is_cough_and_fever[i] = TRUE
    }else{
      df$is_cough_and_fever[i] = FALSE
    }
      
  }
  df$symptoms = NULL

  # Create a month column
  df$month = month(as.POSIXct(df$confirmed))
  return(df)
}

# train$symptoms_number = NULL
# train$is_fever = NULL
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# train$symptoms = trim(train$symptoms)
# for (i in 1:dim(train)[1]) {
#   x = train$symptoms[i]
#   if(x == ''){
#     train$symptoms_number[i] = 0
#   }else {
#     train$symptoms_number[i] = 1
#   }
#   if(grepl("fever",x)){
#     print(x)
#     train$is_fever[i] = TRUE
#   }else {
#     train$is_fever[i] = FALSE
#   }
# }
# 
# train$symptoms_number = as.factor(train$symptoms_number)


cleaned_train = clean_data(train)
cleaned_test = clean_data(test)

write.csv(cleaned_train,"cleaned_train.csv",row.names=FALSE)
write.csv(cleaned_test,"cleaned_test.csv",row.names=FALSE)
