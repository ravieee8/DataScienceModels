library(caret)
library(pROC)
library(car)

setwd("C:\\RAVI\\R_proj\\SCS")

traindata = read.csv(file = "train.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
trainIds = traindata[,1]
train_target = as.factor(traindata[,ncol(traindata)])
traindata = traindata[,-1]
traindata = traindata[,-ncol(traindata)]

testdata = read.csv(file = "test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
testIds = testdata[,1]
testdata = testdata [,-1]

##Removing constant features
for (feature in names(traindata)) {
  if(length(unique(traindata[[feature]])) == 1){
    traindata[[feature]] = NULL
    testdata[[feature]] = NULL
  }
}

##Removing identical features
features_pair = combn(names(traindata), 2, simplify = F)
toRemove = c()
for(pair in features_pair) {
  f1 = pair[1]
  f2 = pair[2]
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(traindata[[f1]] == traindata[[f2]])) {
      toRemove = c(toRemove, f2)
    }
  }
}

feature.names = setdiff(names(traindata), toRemove)

traindata = traindata [,feature.names]
testdata = testdata[,feature.names]

df = cbind.data.frame(traindata,train_target)
feature.names=names(df)
for (f in feature.names) {
  if (class(df[[f]])=="factor") {
    levels = unique(c(df[[f]]))
    df[[f]] = factor(df[[f]],labels=make.names(levels))
  }
}


##10-fold cross validation
ctrl = trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

##Training model through random forest with 2 Tree branches.
model_rf = train(train_target ~., 
                 data = df,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = data.frame(mtry = 2),
                 trControl = ctrl)

model_rf

testTarget = predict(model_rf, newdata = testdata, type = 'prob')

##submission preparation
submission = data.frame(ID=testIds,TARGET=testTarget[,"X2"])
head(submission)
write.csv(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")


