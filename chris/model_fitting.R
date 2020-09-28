rm(list = ls())
setwd("~/Documents/GitHub/stat440-Covid-19/chris")
library(gdata)
library(dplyr)
library(MASS)
library(rpart)
library(randomForest)
library(glmnet)

train = read.csv("cleaned_train.csv",sep = ",",header = T)
test = read.csv("cleaned_test.csv",sep = ",",header = T)
predict = read.csv("baseline2.txt",sep = ",",header = T)

# train$is_cough = NULL
train$is_fever = NULL

# test$is_cough = NULL
test$is_fever = NULL

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
# uLevels('country')
colnames(test)
uLevels(c('country','V1','sex'))

MSE.matrix = matrix(NA,nrow = 200,ncol = 4)
colnames(MSE.matrix) = c("MSE.tree","MSPE.tree","MSE.forest","MSPE.forest")
# Train-valid split
for (r in 1:200) {
  train_index = sample(1:nrow(train),0.7*nrow(train))
  train_data = train[train_index,]
  valid_data = train[-train_index,]
  
  # Fit model
  # model.linear = lm(duration ~ .,data = train_data)
  # MSE of the train data
  # mean((predict(model.linear,data = train_data) - train_data$duration)^2)
  # MSE of the valid data
  # mean((predict(model.linear,newdata = valid_data) - valid_data$duration)^2)
  # predict$duration = predict(model.linear,newdata = test)
  # write.csv(predict,"predicted.csv",row.names=FALSE)
  
  model.tree <- rpart(duration ~ ., data = train_data)
  # MSE of the train data
  MSE.matrix[r,1] = mean((predict(model.tree,data = train_data) - train_data$duration)^2)
  # MSE of the valid data
  MSE.matrix[r,2] = mean((predict(model.tree,newdata = valid_data) - valid_data$duration)^2)

  # Random Forest Regression
  # model.forest = randomForest(duration ~ ., data = train_data,xtest = select(valid_data,-"duration"),ytest =select(valid_data,"duration"))
  model.forest = randomForest(duration ~ ., data = train_data)
  # MSE of the train data
  MSE.matrix[r,3] = mean((predict(model.forest,data = train_data) - train_data$duration)^2)
  # MSE of the valid data
  MSE.matrix[r,4] = mean((predict(model.forest,newdata = valid_data) - valid_data$duration)^2)
}
print(apply(MSE.matrix,2,mean))

model.forest = randomForest(duration ~ ., data = train)
predict$duration = predict(model.forest, newdata = test)
write.csv(predict,"predicted.csv",row.names=FALSE)




###########################knn approach, need to convert factors to numeric#######################
# knn(train = train_data,test = valid_data,cl = train_data[,'duration',drop=TRUE],k=10)




###########################LASSO approach#######################
# train$V1 = NULL
# test$V1 = NULL
# 
# lambda_seq <- 10^seq(2, -2, by = -.1)
# set.seed(86)
# x_vars <- model.matrix(duration~. , train)[,-1]
# y_var <- train$duration
# x_vars_test <- model.matrix(~.,select(test,-"Id"))[,-1]
# 
# lambda_seq <- 10^seq(2, -2, by = -.1)
# cv_output <- cv.glmnet(x_vars, y_var,
#                        alpha = 1, lambda = lambda_seq, 
#                        nfolds = 5)
# 
# 
# # identifying best lamda
# best_lam <- cv_output$lambda.min
# best_lam
# lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = best_lam)
# pred <- predict(lasso_best, s = best_lam, newx = x_vars_test)
# predict$duration = predict(lasso_best, s = best_lam, newx = x_vars_test)
# write.csv(predict,"predicted.csv",row.names=FALSE)
# lasso_best$beta

