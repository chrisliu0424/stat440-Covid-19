rm(list = ls())
setwd("~/Desktop/Stat440/Module/module1/chris")

train = read.csv("train2.txt",sep = ",",header = T)

# 1.Make date column to datetime Object
train$confirmed <- as.Date(train$confirmed, format = "%d.%m.%y")

# 2.Exclude outcome column
train$outcome = NULL

# 3.Exclude city,province,symptoms for now.
train$city = NULL
train$symptoms = NULL
train$province = NULL

