library(caret)
library(pixmap)
setwd("C:\\RAVI\\R_proj\\face_recognizer")

r = 40 * 10
c = 112 * 92
mx = matrix(rep(1,(r*c)), nrow = r, ncol = c, byrow = TRUE)

idx = 0
for (subject in 1:40) {
  for (pic in 1:10) {
    img = read.pnm(file=sprintf("att_faces\\s%s\\%s.pgm",subject,pic))
    lable = sprintf("s%s",subject)
    df = getChannels(img)
    idx = idx + 1
    mx[idx,] = c(t(df))
  }
}

traindata = as.data.frame(mx)
dim(traindata)
str(traindata)

cat("[INFO]::Removing Near zero variance data")
nzvobj = nearZeroVar(traindata,saveMetrics = TRUE)
df1 = traindata[,nzvobj$nzv==FALSE]
v = dim(df1)
cat ("[INFO]::Found",v[[1]],"Rows and",v[[2]],"Columns after Near zero variance features Removed")

cat("[INFO]::Normalizing the data")
minmaxobj = preProcess(df1,method=c("range"))
df2 = predict(minmaxobj,df1)

cat("[INFO]::Applying PCA")
pcaobj = preProcess(df1,method=c("pca"))
df3 = predict(pcaobj,df2)
v = dim(df3)
cat ("[INFO]::Found",v[[1]],"Rows and",v[[2]],"Columns after PCA")

##display Eigenfaces


