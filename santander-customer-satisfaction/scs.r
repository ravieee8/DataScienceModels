library(caret)
setwd("C:\\RAVI\\R_proj\\SCS")
set.seed(100)

traindata = read.csv("train.csv",header = TRUE, sep = ',', stringsAsFactors = FALSE)
#traindata = traindata[sample(1:nrow(traindata),1000),]

ids = traindata[,1]
target = as.factor(traindata[,371])
traindata = traindata[,-371]
traindata = traindata[,-1]


preobj = preProcess(traindata,method = c("BoxCox","medianImpute"))
traindata1 = predict(preobj,traindata)

zvdata = nearZeroVar(traindata1,saveMetrics = TRUE)
traindata2 = traindata[,zvdata$zeroVar==FALSE]
dim(traindata2)

preobj1 = preProcess(traindata2,method = c("pca"))
traindata3 = predict(preobj1,traindata2)

df = cbind.data.frame(traindata3,target)
ctrl = trainControl(method = "cv", number = 10)
model_rf = train(target ~. , 
                  data = df,
                  method = "rf",
                  tuneGrid = data.frame(mtry = 2),
                  trControl = ctrl)

model_rf



##predicting testdata
testdata = read.table("test.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
dim(testdata)
testIds = testdata[,1]
testdata1 = testdata[,-1]

names(testdata1)
dim(testdata1)
testNonnzvdata = testdata1[,nzvdata$zeroVar==FALSE]
testdata_std = predict(preobj,testNonnzvdata)
testTarget = predict(model_rf, newdata = testdata_std)
submission = data.frame(testIds,testTarget)
names(submission) = c("ID","TARGET")
head(submission)
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
table(submission$TARGET)

