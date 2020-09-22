rm(list = ls())
setwd("~/Documents/GitHub/stat440-Covid-19/chris")
library(gdata)

train = read.csv("cleaned_train.csv",sep = ",",header = T)
test = read.csv("cleaned_test.csv",sep = ",",header = T)
train$confirmed <- as.Date(train$confirmed, format = "%d.%m.%y")
test$confirmed <- as.Date(test$confirmed, format = "%d.%m.%y")

# code taken from:https://www.kaggle.com/c/bluebook-for-bulldozers/discussion/4066deal with new levels in test data
uLevels <- function(names){
  for(c in names){
    print(c)
    train[,c] <<- factor(as.character(train[,c]))
    test[,c] <<- factor(as.character(test[,c]))
    map <- mapLevels(x=list(train[,c], test[,c]), codes=FALSE, combine=TRUE)
    mapLevels(train[,c]) <<- map
    mapLevels(test[,c]) <<- map
  }
}

train$country = uLevels('country')
train$V1 = uLevels('V1')

# levels(train$country) <- c(levels(test$country),levels(train$country))
# levels(train$V1) <- c(levels(test$V1),levels(train$V1))

train
fit = lm(duration ~ .,data = train)
predicted = predict(fit,newdata = test)

write.csv(predicted,"predicted.csv")
