library(caret)
library(car)
library(pROC)

setwd("C:\\RAVI\\R_proj\\SCS")
set.seed(100)

traindata = read.csv("train.csv",header = TRUE, sep = ',', stringsAsFactors = FALSE)

ids = traindata[,1]
target = as.factor(traindata[,371])
#target_recode = recode(target," '1' = 'YES';'0' = 'NO' ")

traindata = traindata[,-371]
traindata = traindata[,-1]
str(traindata)

##Removing zerovariance data
zvdata = nearZeroVar(traindata,saveMetrics = TRUE)
traindata1 = traindata[,zvdata$zeroVar==FALSE]

##Applying PCA 
preobj = preProcess(traindata1,method = c("pca"),thresh = 0.99)
traindata2 = predict(preobj,traindata1)
df = cbind.data.frame(traindata2,target)

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
model_rf = train(target ~. , 
                 data = df,
                 method = "rf",
                 tuneGrid = data.frame(mtry = 2),
                 metric = "ROC",
                 trControl = ctrl)


model_rf

##predicting testdata
testdata = read.table("test.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
dim(testdata)
testIds = testdata[,1]
testdata1 = testdata[,-1]

##Removing zero variance data in Test data
testdata2 = testdata1[,zvdata$zeroVar==FALSE]
dim(testdata2)

##Applying PCA on Test data
testdata3 = predict(preobj,testdata2)

##Predicting TARGET from Trained model
testTarget = predict(model_rf, newdata = testdata3, type = 'prob')


##submission preparation
submission = data.frame(testIds,testTarget)
names(submission) = c("ID","TARGET")
head(submission)
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
table(submission$TARGET)

