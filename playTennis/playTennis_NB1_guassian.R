library(caret)
setwd("C:\\RAVI\\R_proj\\NaiveBayes")

data = read.csv(file = "playtennis1.csv",sep = ",",header = T)
dim(data)
str(data)

ctrl = trainControl(method = "LOOCV")
tennis_model = train(data,data$playtennis,method = "nb",trControl = ctrl)

tennis_model$finalModel$apriori
tennis_model$finalModel$tables
tennis_model

test1 = data.frame("sunny","26","high","strong")
tennis_predict = predict(tennis_model,test1,type = "prob")

tennis_predict
