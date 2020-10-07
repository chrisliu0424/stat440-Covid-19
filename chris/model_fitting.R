rm(list = ls())
setwd("~/Documents/GitHub/stat440-Covid-19/chris")
library(gdata)
library(dplyr)
library(MASS)
library(rpart)
library(randomForest)
library(glmnet)

train = read.csv("~/Documents/GitHub/stat440-Covid-19/chris/cleaned_train.csv",sep = ",",header = T)
test = read.csv("~/Documents/GitHub/stat440-Covid-19/chris/cleaned_test.csv",sep = ",",header = T)
predict = read.csv("~/Documents/GitHub/stat440-Covid-19/chris/baseline2.txt",sep = ",",header = T)

# Stepwise comparison
initial.1 <- lm(data=train, formula = duration~ 1)
final.1 <- lm(data=train, formula=duration~.)

step1 <- step(object=initial.1, scope=list(upper=final.1), 
              k = log(nrow(train)))


MSE.matrix = matrix(NA,nrow = 500,ncol = 1)
# colnames(MSE.matrix) <- c("MSE","MSPE")
train$confirmed <- as.Date(train$confirmed, format = "%Y-%m-%d")
test$confirmed <- as.Date(test$confirmed, format = "%Y-%m-%d")

# Train-valid split
for (r in 1:500) {
  print(paste0(r," in ",500))
  train_index = sample(1:nrow(train),0.8*nrow(train))
  train_data = train[train_index,]
  valid_data = train[-train_index,]
  # Fit model
  # model.linear = lm(duration ~ symptoms_number + confirmed + age,data = train_data)
  # MSE of the train data
  # MSE.matrix[r,1] = mean((exp(predict(model.linear,data = train_data)) - train_data$duration)^2)
  # # MSE of the valid data
  # MSE.matrix[r,2] = mean((exp(predict(model.linear,newdata = valid_data)) - valid_data$duration)^2)
  # predict$duration = predict(model.linear,newdata = test)
  # write.csv(predict,"predicted.csv",row.names=FALSE)
  # Age: 1
  # confirmed: 25
  # symptoms_number: 26
  # Sex: 2:5
  # country: 7:16
  # V1: 17:24
  # c(1,25,26,27,17:24,7:16)
  # Random Forest Regression
  # model.forest = randomForest(duration ~ ., data = train_data[,c(1,25,26,27,2:5)])
  # # MSE of the train data
  # MSE.matrix[r,1] = mean((predict(model.forest,data = train_data) - train_data$duration)^2)
  # # MSE of the valid data
  # MSE.matrix[r,2] = mean((predict(model.forest,newdata = valid_data) - valid_data$duration)^2)
  # # print(MSE.matrix[r,])
  variable_selected = c(1,25,26,17:24)
  MSE.matrix[r,1] = mean((as.numeric(knn(train = as.data.frame(train_data[,variable_selected]), test = as.data.frame(valid_data[,variable_selected]),cl = train_data[,'duration',drop=TRUE], k=20)) - valid_data$duration)^2)
}

print(paste0("mean = ", apply(MSE.matrix,2,mean),", sd = ",apply(MSE.matrix,2,sd)))

print(apply(MSE.matrix,2,sd))
colnames(train)

# Choosing the best k for knn
Knn_vector = rep(NA,500)
Knn_matrix = matrix(NA,nrow = 100,ncol = 3)
for (i in 1:100) {
  print(paste0(i," of ",100))
  for(r in 1:500){
    train_index = sample(1:nrow(train),0.8*nrow(train))
    train_data = train[train_index,]
    valid_data = train[-train_index,]
    Knn_vector[r] = mean((as.numeric(knn(train = as.data.frame(train_data[,variable_selected]), test = as.data.frame(valid_data[,variable_selected]),cl = train_data[,'duration',drop=TRUE], k = i)) - valid_data$duration)^2)
  }
  Knn_matrix[i,] = c(i,mean(Knn_vector),sd(Knn_vector))
}
Knn_matrix


model.linear = lm(duration ~ confirmed + age + symptoms_number,data = train)
model.forest = model.forest = randomForest(duration ~ ., data = train)
predict$duration = predict(model.linear, newdata = test)/2 + predict(model.forest, newdata = test)/2
write.csv(predict,"predicted.csv",row.names=FALSE)

predict$duration = 0.5*lm(duration~confirmed + age + symptoms_number,data = train)+0.5*model.forest = model.forest = randomForest(duration ~ ., data = train)
predict$duration = as.numeric(knn(train = as.data.frame(train[,26]), test = as.data.frame(test[,26]),cl = train[,'duration',drop=TRUE], k=10))-1
write.csv(predict,"predicted1.csv",row.names=FALSE)
###########################knn approach, need to convert factors to numeric#######################
# knn(train = train_data,test = valid_data,cl = train_data[,'duration',drop=TRUE],k=10)

