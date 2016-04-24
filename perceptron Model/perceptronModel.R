
hinge_loss = function (w,x,y){
    total_missclass = 0
    total_error = 0
    for ( i in 1:nrow(x)){
      tmp = y[i] * w %*% x[i,]
      if(tmp<=0){
        total_error = total_error - tmp
        total_missclass = total_missclass + 1
      }
    }
    total_error = total_error/nrow(x)
    c(total_error,total_missclass)
}

batch_gradient = function (w,x,y) {
  delta = rep(0,ncol(x))
  for (i in 1:nrow(x)){
    tmp = y[i] * w %*%x[i,]
    if (tmp <= 0){
      delta = delta - y[i] * x[i,]
    }
  }
  delta = delta/nrow(x)
}

batch_gradient_decent = function (x,y,learningrate,iterations){
  w = rep(0,ncol(x))
  E = matrix(0,nrow=iterations,ncol=3)
  for (iter in 1:iterations){
    gradient = batch_gradient(w,x,y)
    w = w - (learningrate * gradient)
    error_current = hinge_loss(w,x,y)
    E[iter,1] = iter
    E[iter,2] = error_current[2]
    E[iter,3] = error_current[1]
    
  }
  E
}

setwd("C:\\RAVI\\R_proj")
data = read.csv(file = "perceptronData.csv",sep = ",")
target = data[,5]
data = data[,-5]
class(data)
dim(data)
data = as.matrix(data)
class(target)
iterations = 1000
learninrate = 1

sloution = batch_gradient_decent(data,target,learninrate,iterations)


































