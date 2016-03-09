library(caret)
library(car)

set.seed(100)
setwd("C:\\RAVI\\R_proj\\Titanic")

titanicdata = read.csv("train.csv",header = TRUE, sep=',', stringsAsFactors = FALSE)
titanicdata$Survived = as.factor(titanicdata$Survived)
titanicdata$Pclass = as.factor(titanicdata$Pclass)
#titanicdata$Sex = as.factor(titanicdata$Sex)
#titanicdata$Embarked = as.factor(titanicdata$Embarked)
str(titanicdata)



##recode
#recode_Sex = recode(titanicdata$Sex,"'male' = '1';'female' = '2'")
recode_embark = recode(titanicdata$Embarked,"'S'= '1';'C'= '2';'Q' = '3'")
df_recode = data.frame(recode_Sex,recode_embark)
names(df_recode) = c("Sex","Embarked")
dummyObj = dummyVars(~Sex+Embarked,df_recode,fullRank = TRUE)
recodedata = predict(dummyObj,df_recode)
df_recodedata = data.frame(recodedata)
names(df_recodedata) = c("Sex","Embarked1","Embarked2","Embarked3")
summary(df_recodedata)


#sibsp_parch = as.numeric(as.character(titanicdata$SibSp)) + as.numeric(as.character(titanicdata$Parch))
#sibsp_parch = data.frame(sibsp_parch)
#titanicdata = data.frame(titanicdata,sibsp_parch)
#class(sibsp_parch)

names(titanicdata)
subdata = data.frame(titanicdata$Age,titanicdata$SibSp,titanicdata$Parch,titanicdata$Fare)
preobj = preProcess(subdata,method = c("bagImpute","center","scale"))
miss_impute = predict(preobj, subdata)


newdata = data.frame(titanicdata$PassengerId,titanicdata$Survived,miss_impute,df_recodedata,titanicdata$Pclass)
names(newdata) = c("PassengerId","Survived","Age","SibSp","Parch","Fare","Sex","Embarked1","Embarked2","Embarked3","Pclass")
names(newdata)

ctrl = trainControl(method = "cv", number = 10)
model_rf = train(Survived ~ Age + SibSp + Parch + Fare + Sex + Pclass,
                 data=newdata, 
                 method = "rf",
                 trControl = ctrl)
model_rf



testSet = read.table("test.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
str(testSet)
testSet$Pclass = as.factor(testSet$Pclass)
testSet$Sex = as.factor(testSet$Sex)
testSet$Embarked = as.factor(testSet$Embarked)

subtestdata = data.frame(testSet$Age,testSet$SibSp,testSet$Parch,testSet$Fare)
preobj1 = preProcess(subtestdata,method = c("bagImpute","center","scale"))
test_missimpute = predict(preobj1, subtestdata)


test_newdata = data.frame(testSet$PassengerId,test_missimpute,testSet$Pclass,testSet$Sex)
names(test_newdata) = c("PassengerId","Age","SibSp","Parch","Fare","Pclass","Sex")



test_newdata$Survived = predict(model_rf, newdata = test_newdata)

submission = test_newdata[,c("PassengerId", "Survived")]

write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")



