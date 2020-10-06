rm(list = ls())
library(randomForest)
library(class)
library(dplyr)

#using AoTang's data
train = read.csv("~/Documents/GitHub/stat440-Covid-19/chris/cleaned_train_AoTang.txt",sep = ",",header = T)
test = read.csv("~/Documents/GitHub/stat440-Covid-19/chris/cleaned_test_AoTang.txt",sep = ",",header = T)
predict = read.csv("~/Documents/GitHub/stat440-Covid-19/chris/baseline2.txt",sep = ",",header = T)


MSE.matrix = matrix(NA,nrow = 200,ncol = 7)
colnames(MSE.matrix) = c("MSE.Regression","MSPE.Regression","MSE.forest","MSPE.forest","MSPE.Knn","MSE.Ensemble","MSPE.Ensemble")

Ensemble_parameter_matrix = matrix(NA,nrow = 66,ncol = 10)
colnames(Ensemble_parameter_matrix) = c("i",'j',"1-i-j","MSE.Regression","MSPE.Regression","MSE.forest","MSPE.forest","MSPE.Knn","MSE.Ensemble","MSPE.Ensemble")
k=1
for (i in seq(from = 0, to = 1, by = 0.1)){
  for (j in seq(from = 0, to = 1-i, by = 0.1)) {
    for (r in 1:200) {
      print(paste0(r," of ",200))
      train_index = sample(1:nrow(train),0.8*nrow(train))
      train_data = train[train_index,]
      valid_data = train[-train_index,]
      
      # Regression model
      model.Regression <- lm(duration ~ confirmed + symptoms_number, data = train_data)
      # MSE of the train data
      MSE.matrix[r,1] = mean((predict(model.Regression,data = train_data) - train_data$duration)^2)
      # MSE of the valid data
      MSE.matrix[r,2] = mean((predict(model.Regression,newdata = valid_data) - valid_data$duration)^2)
      
      # Random Forest Regression
      model.forest = randomForest(duration ~ ., data = train_data[,c(1,25,26,27,2:5)])
      # MSE of the train data
      MSE.matrix[r,3] = mean((predict(model.forest,data = train_data) - train_data$duration)^2)
      # MSE of the valid data
      MSE.matrix[r,4] = mean((predict(model.forest,newdata = valid_data) - valid_data$duration)^2)
      
      
      # Knn
      predict.knn = knn(train = train_data[,c(1,25,26,17:24)], test = valid_data[,c(1,25,26,17:24)],cl = train_data[,'duration',drop=TRUE], k=23)
      # MSE of the valid data
      MSE.matrix[r,5] = mean((as.numeric(predict.knn) - valid_data$duration)^2)
      
      # MSE of the train data 0.5*Regression + 0.5*Forest
      MSE.matrix[r,6] = mean((predict(model.Regression, newdata = train_data)*i + predict(model.forest, newdata = train_data)*j + as.numeric(predict.knn)*(1-i-j) - train_data$duration)^2)
      # MSE of the valid data 0.5*Regression + 0.5*Forest
      MSE.matrix[r,7] = mean((predict(model.Regression, newdata = valid_data)*i + predict(model.forest, newdata = valid_data)*j + as.numeric(predict.knn)*(1-i-j) - valid_data$duration)^2) 
    }
    Ensemble_parameter_matrix[k,] = c(i,j,1-i-j,apply(MSE.matrix,2,mean))
    k=k+1
    print(Ensemble_parameter_matrix)
  }
}
temp = as.data.frame(Ensemble_parameter_matrix)
write.csv(temp,"temp_Ensemble_data.csv",row.names=FALSE)

# Test for Knn
Knn_vector = rep(NA,200)
Knn_matrix = matrix(NA,nrow = 100,ncol = 2)
for (i in 1:100) {
  print(paste0(i," of ",100))
  for(r in 1:200){
    train_index = sample(1:nrow(train),0.8*nrow(train))
    train_data = train[train_index,]
    valid_data = train[-train_index,]
    Knn_vector[r] = mean((as.numeric(knn(train = train_data, test = valid_data,cl = train_data[,'duration',drop=TRUE], k=10)) - valid_data$duration)^2)
  }
  Knn_matrix[i,] = c(i,mean(Knn_vector))
}
Knn_matrix

model.Regression <- lm(duration ~ age + confirmed, data = train)
model.forest = randomForest(duration ~ ., data = train)
predict$duration = predict(model.Regression, newdata = test)*0.5 + predict(model.forest, newdata = test)*0.5 #+ as.numeric(knn(train = select(train,-c('duration')), test = test,cl = train[,'duration',drop=TRUE], k=10))*0.3
write.csv(predict,"predicted.csv",row.names=FALSE)






