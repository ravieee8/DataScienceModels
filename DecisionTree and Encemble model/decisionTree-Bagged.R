library(rpart)

bootstrap_train = function (train.data) {
  s = sample(1:nrow(train.data),nrow(train.data),rep=T)
  bootData = train.data[unique(s),]
  return(bootData)
}

bagged.tree.learn = function (train.data,target,numTrees = 1,maxdepth = 30) {
  predictors = paste(names(train.data), collapse = "+")
  form = as.formula(paste(target,"~",predictors,"-",target))
  models = list()
  for (i  in 1:numTrees) {
    bootdata = bootstrap_train(train.data)
    weights = rep(1,nrow(bootdata))
    tree = rpart(form,bootdata,weights = weights,control = rpart.control(xval = 1,maxdepth = maxdepth))
    models = c(models,list(tree))
  }
  return(models)
}

bagged.tree.predict = function (models,test.data) {
  testdata = test.data[,-1]
  mx = matrix(NA,nrow=nrow(test.data),ncol=length(models))
  for (model in 1:length(models)){
    test.pred = predict(models[[model]],testdata, type = "class")
    mx[,model] = test.pred
  }
  outcome = numeric(length = nrow(testdata))
  for (i in 1:nrow(testdata)){
    df = as.data.frame(table(mx[i,]))
    outcome[i] = as.numeric(as.character(df[which.max(df$Freq),1]))
  }
  mx = cbind(mx,outcome)
  test.accuracy = sum(mx[,length(models)+1] == as.numeric(test.data[,"target"]))/nrow(test.data)
  list(mx,test.accuracy)
}


setwd("C:\\RAVI\\R_proj\\DesitionTree")
data = read.csv(file = "letterCG.csv",sep=',',header=T)
dim(data)
names(data)
str(data)

train.data = data[1:500,]
test.data = data[-(1:500),]

models = bagged.tree.learn(train.data,"target",51)
models
out = bagged.tree.predict(models,test.data)
out




