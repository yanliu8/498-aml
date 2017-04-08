diatetes <- read.csv("~/cs498aml/hw1/3-1.data", header=FALSE)
#splitting training and testing data
trainingRowNum<-createDataPartition(diatetes$V9,1,0.8)
training<-diatetes[trainingRowNum$Resample1,]
test<-diatetes[-trainingRowNum$Resample1,]
#seperating postive and negative samples in training samples
trainpos<-training[training$V9==1,]
trainneg<-training[training$V9==0,]
#calculating prior probability
ppos=dim(trainpos)[1]/dim(training)[1]
pneg=dim(trainneg)[1]/dim(training)[1]
posmeanval = c()
possdval = c()
#calculating std and mean of postive and negative samples
for (y in trainpos[,1:8]) {
  posmeanval <- c(posmeanval,mean(y))
  possdval <- c(possdval, sd(y))
}
negmeanval = c()
negsdval = c()
for (y in trainneg[,1:8]) {
  negmeanval <- c(negmeanval,mean(y))
  negsdval <- c(negsdval, sd(y))
}
#using dnorm to calculate density function of normal distribution 
posheight <- vector(mode="numeric",length=length(test))
for (i in 1:8) {
  posheight = posheight + dnorm(test[,i],posmeanval[i],possdval[i],log=TRUE)
}
posheight = posheight + log(ppos)
negheight <- vector(mode="numeric",length=length(test))
for (i in 1:8) {
  negheight = negheight + dnorm(test[,i],negmeanval[i],negsdval[i],log=TRUE)
}
negheight = negheight + log(pneg)
#negheight and posheight are posterior
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
#calculating accuracy
accuracy = correct / length(prediction)
accuracy
