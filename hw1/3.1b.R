diatetes <- read.csv("~/cs498aml/hw1/3-1.data", header=FALSE)
#changing 0 values to NA
diatetes$V3[diatetes$V3==0] <-NA
diatetes$V4[diatetes$V4==0] <-NA
diatetes$V6[diatetes$V6==0] <-NA
diatetes$V8[diatetes$V8==0] <-NA
trainingRowNum<-createDataPartition(diatetes$V9,1,0.8)
training<-diatetes[trainingRowNum$Resample1,]
test<-diatetes[-trainingRowNum$Resample1,]
trainpos<-training[training$V9==1,]
trainneg<-training[training$V9==0,]
ppos=dim(trainpos)[1]/dim(training)[1]
pneg=dim(trainneg)[1]/dim(training)[1]
posmeanval = c()
possdval = c()
for (y in trainpos[,1:8]) {
  posmeanval <- c(posmeanval,mean(y,na.rm=TRUE))
  possdval <- c(possdval, sd(y, na.rm=TRUE))
}
negmeanval = c()
negsdval = c()
for (y in trainneg[,1:8]) {
  negmeanval <- c(negmeanval,mean(y, na.rm=TRUE))
  negsdval <- c(negsdval, sd(y,na.rm=TRUE))
}
posheight <- vector(mode="numeric",length=length(test))
for (i in 1:8) {
  temp <- vector(mode="numeric",length=dim(test)[1])
  for (j in 1:dim(test)[1]){
  if (!is.na(test[j,i])){
    temp[j] <- dnorm(test[j,i],posmeanval[i],possdval[i],log=TRUE)
  }
    else{
      temp[j] <- 0
    }
  }
  posheight = posheight + temp
}
posheight = posheight + log(ppos)
negheight <- vector(mode="numeric",length=length(test))
for (i in 1:8) {
  temp <- vector(mode="numeric",length=dim(test)[1])
  for (j in 1:dim(test)[1]){
    if (!is.na(test[j,i])){
      temp[j] <- dnorm(test[j,i],negmeanval[i],negsdval[i],log=TRUE)
    }
    else{
      temp[j] <- 0
    }
  }
  negheight = negheight + temp
}
negheight = negheight + log(pneg)
correct = 0
prediction = vector(mode="numeric", length=length(negheight))
for (i in 1:length(negheight)) {
  if (negheight[i] < posheight[i]){
    prediction[i] = 1
  }
  else {
    prediction[i] = 0
  }
  if (prediction[i] == test$V9[i])
    correct = correct + 1
}
accuracy = correct / length(prediction)
accuracy
