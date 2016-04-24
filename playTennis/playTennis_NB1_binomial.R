
library(caret)

##seting working directory
setwd("C:\\RAVI\\R_proj\\NaiveBayes")
data = read.csv(file = "playtennis.csv",sep = ",",header = TRUE)

##preprocessing and feature selection not required for given data

##using leave one out cross validation 
ctrl = trainControl(method = "LOOCV")

##Training throufh Naive bayes model
tennis_model = train(data,data$playtennis,
                     method = "nb",
                     trControl = ctrl)

##Displaying likelyhoods
tennis_model$finalModel$tables

##Displaying prior probabilities
tennis_model$finalModel$apriori

##predicting hypothesis
x1 = "sunny"
x2 = "hot"
x3 = "high"
x4 = "strong"
test1 = data.frame(x1,x2,x3,x4)
tennis_predict = predict(tennis_model,test1,type = "prob")
tennis_predict

if (tennis_predict$yes > tennis_predict$no) {
  
  cat ("[OUTPUT]::playtennis = yes(probability=",tennis_predict$yes,")",
       "when outlook=",x1,", ",
       "temperature=",x2,", ",
      "humidity=",x3,", ",
      "wind=",x4,".")

} else {
  
  cat ("[OUTPUT]::playtennis = yes(probability=",tennis_predict$yes,")",
       "when outlook=",x1,", ",
       "temperature=",x2,", ",
       "humidity=",x3,", ",
       "wind=",x4,".")
  
}

print("Run Finished, Bye Ravi...")


