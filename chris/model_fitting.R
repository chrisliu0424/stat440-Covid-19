rm(list = ls())
setwd("~/Documents/GitHub/stat440-Covid-19/chris")
library(gdata)
library(MASS)
library(rpart)
library(rpart.plot)
library(randomForest)

train = read.csv("cleaned_train.csv",sep = ",",header = T)
test = read.csv("cleaned_test.csv",sep = ",",header = T)
predict = read.csv("baseline2.txt",sep = ",",header = T)


# Make confirmed a Datetime object
train$confirmed <- as.Date(train$confirmed, format = "%Y-%m-%d")
test$confirmed <- as.Date(test$confirmed, format = "%Y-%m-%d")

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
uLevels(c('country','V1','sex'))

# Train-valid split
train_index = sample(1:nrow(train),0.7*nrow(train))
train_data = train[train_index,]
valid_data = train[-train_index,]

# Fit model
model.linear = lm(duration ~ .,data = train_data)
# MSE of the train data
mean((predict(model.linear,data = train_data) - train_data$duration)^2)
# MSE of the valid data
mean((predict(model.linear,newdata = valid_data) - valid_data$duration)^2)
predict$duration = predict(model.linear,newdata = test)
# write.csv(predict,"predicted.csv",row.names=FALSE)

model.tree <- rpart(duration ~ ., data = train_data)
# MSE of the train data
mean((predict(model.tree,data = train_data) - train_data$duration)^2)
# MSE of the valid data
mean((predict(model.tree,newdata = valid_data) - valid_data$duration)^2)

predict$duration = predict(model.tree,newdata = test)
# write.csv(predict,"predicted.csv",row.names=FALSE)




# Random Forest Regression
model.forest = randomForest(duration ~ ., data = train_data)
# MSE of the train data
mean((predict(model.forest,data = train_data) - train_data$duration)^2)
# MSE of the valid data
mean((predict(model.forest,newdata = valid_data) - valid_data$duration)^2)

predict$duration = predict(model.forest,newdata = test[,])
# write.csv(predict,"predicted.csv",row.names=FALSE)
