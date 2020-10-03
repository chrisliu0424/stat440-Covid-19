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
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  df$symptoms = trim(df$symptoms)
  for (i in 1:dim(df)[1]) {
    x = df$symptoms[i]
    if(x == ''){
      df$symptoms_number[i] = 0
    }else {
      df$symptoms_number[i] = lengths(regmatches(x, gregexpr(";", x))) + 1
    }
  }
  df$symptoms = NULL

  # Create a month column
  df$month = month(as.POSIXct(df$confirmed))
  return(df)
}


cleaned_train = clean_data(train)
cleaned_test = clean_data(test)

write.csv(cleaned_train,"cleaned_train.csv",row.names=FALSE)
write.csv(cleaned_test,"cleaned_test.csv",row.names=FALSE)


initial.1 <- lm(data=cleaned_train, formula = duration~ 1)
final.1 <- lm(data=cleaned_train, formula=duration~.)

step1 <- step(object=initial.1, scope=list(upper=final.1), 
              k = log(nrow(cleaned_train)))
