library(caret)
library(klaR)

setwd("C:\\RAVI\\R_proj\\digit_recognizer")

traindata = read.csv("train.csv",header = TRUE)
class(traindata)
dim(traindata)
str(traindata)
summary(traindata)
names(traindata)
traindata$label = as.factor(traindata$label)
str(traindata)

labels = traindata$label
traindata = traindata[,-1]
head(traindata)

nzvdata = nearZeroVar(traindata,saveMetrics = TRUE)
traindata1 = traindata[,nzvdata$nzv==FALSE]
dim(traindata1)

pcaobj = preProcess(traindata1,method = c("pca"))
pcadata = predict(pcaobj,traindata1)

train_cv = trainControl(method = "cv",number = 10)
model_knn = train(pcadata,labels,method = "knn",trControl = train_cv)
#model_knn = train(pcadata,labels,method = "nb",trControl = train_cv)


test_data = read.csv("test.csv",header=TRUE)
test_data1 = test_data[,nzvdata$nzv == FALSE]
test_pcadata = predict(pcaobj,test_data1)
test_data$label = predict(model_knn, newdata=test_pcadata)
test_data$ImageId = 1:nrow(test_data)
submission = test_data[,c("ImageId","label")]
write.table(submission,file="submision.csv",row.names = FALSE,col.names = TRUE, sep = ',')




