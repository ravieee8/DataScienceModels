library("caret")

midcity=read.csv("MidCity.csv")
head(midcity)
dim(midcity)
str(midcity)

midcity1 = midcity[,c("Bedrooms","Bathrooms","SqFt")]
head(midcity1)

midcity1Obj = preProcess(midcity1,method = c("center","scale"))
midcity2 = predict(midcity1Obj,midcity1)


train_cv = trainControl(method = "cv",number = 10)
model_knn = train(midcity2,midcity$Price,method = "knn",trControl = train_cv)

test_midcity = predict(midcity1Obj,midcity2[120:128,])
test_midcity$Price = predict(model_knn, newdata=test_midcity)
model_knn$finalModel
