rm(list = ls())
train = read.table('train2.txt', sep =',', header =T, row.names = NULL)
test = read.table('test2.txt', sep =',', header =T, row.names = NULL)

train$age = as.character(train$age)
test$age = as.character(test$age)
ages = c(test$age, train$age)
ages = ages[!grepl('-', ages)]
ages = as.numeric(ages)
mu = mean(ages[!is.na(ages)])

age.max = c()
age.min = c()
age.range = c()
age.missing = c()

for (i in 1:dim(train)[1]) {
  x = train$age[i]
  if (x == '') {
    x = mu
    train$age[i] = x
    age.min = c(age.min, x)
    age.max = c(age.max, x)
    age.range = c(age.range, 0)
    age.missing = c(age.missing, 1)
  } else if (grepl('-', x, fixed = TRUE)) {
    z = strsplit(as.character(x), '-')
    x = mean(as.numeric(c(z[[1]][1], z[[1]][2])))
    train$age[i] = x
    age.min = c(age.min, z[[1]][1])
    age.max = c(age.max, z[[1]][2])
    age.range = c(age.range, 1)
    age.missing = c(age.missing, 0)
  } else {
    age.max = c(age.max, x)
    age.min = c(age.min, x)
    age.range = c(age.range, 0)
    age.missing = c(age.missing, 0)
  }
}

train$age.max = as.numeric(age.max)
train$age.min = as.numeric(age.min)
train$age.range = as.numeric(age.range)
train$age = as.numeric(train$age)
train$age.missing = as.numeric(age.missing)

age.max = c()
age.min = c()
age.range = c()
age.missing = c()

for (i in 1:dim(test)[1]) {
  x = test$age[i]
  if (x == '') {
    x = mu
    test$age[i] = x
    age.min = c(age.min, x)
    age.max = c(age.max, x)
    age.range = c(age.range, 0)
    age.missing = c(age.missing, 1)
  } else if (grepl('-', x, fixed = TRUE)) {
    z = strsplit(x, '-')
    x = mean(as.numeric(c(z[[1]][1], z[[1]][2])))
    test$age[i] = x
    age.min = c(age.min, z[[1]][1])
    age.max = c(age.max, z[[1]][2])
    age.range = c(age.range, 1)
    age.missing = c(age.missing, 0)
  } else {
    age.max = c(age.max, x)
    age.min = c(age.min, x)
    age.range = c(age.range, 0)
    age.missing = c(age.missing, 0)
  }
}

test$age.max = as.numeric(age.max)
test$age.min = as.numeric(age.min)
test$age.range = as.numeric(age.range)
test$age = as.numeric(test$age)
test$age.missing = as.numeric(age.missing)

RMSE = function(a, b){
  return(sqrt(mean((a - b)^2)))
}

truth = train$duration

print("Test set errors:")

print("LM with age")
pred = predict(lm(duration ~ age + 1, train), new = test)

kaggle$duration = pred

write.table(kaggle, file = 'kaggle0.txt', sep = ',', quote = F, row.name = FALSE)

print("LM with age + augment")
pred = predict(lm(duration ~ age + age.min + age.max + age.range + age.missing + 1, train), new = test)


kaggle = read.table(file = 'baseline2.txt', sep = ',', header = T, row.names = NULL)

kaggle$duration = pred

write.table(kaggle, file = 'kaggle1.txt', sep = ',', quote = F, row.name = FALSE)