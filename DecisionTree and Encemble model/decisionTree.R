library(rpart)

tree.learn = function (train.data,target,weights = rep(1,nrow(train.data)),maxdepth = 30) {
  predictors = paste(names(train.data), collapse = "+")
  form = as.formula(paste(target,"~",predictors,"-",target))
  tree = rpart(form,train.data,weights = weights,control = rpart.control(xval = 1,maxdepth = maxdepth))
  return(tree) 
}

tree.predict = function (model,test.data) {
  testdata = test.data[,-1]
  test.pred = predict(model,testdata, type = "class")
  test.accuracy = sum(test.pred == test.data[,"target"])/nrow(test.data)
  list(test.pred,test.accuracy)
}


setwd("C:\\RAVI\\R_proj\\DesitionTree")
data = read.csv(file = "letterCG.csv",sep=',',header=T)
dim(data)
names(data)
str(data)

train.data = data[1:500,]
test.data = data[-(1:500),]
model = tree.learn(train.data,"target")
model
out = tree.predict(model,test.data)
out




