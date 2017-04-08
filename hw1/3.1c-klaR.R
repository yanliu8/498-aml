library("klaR")
library("caret")
trainingRowNum<-createDataPartition(diatetes$V9,1,0.8)
training<-diatetes[trainingRowNum$Resample1,]
test<-diatetes[-trainingRowNum$Resample1,]

cols<-c("V1","V2","V3","V4","V5","V6","V7","V8")
predictors<-training[cols]
nb <- NaiveBayes(predictors, as.factor(training$V9))

test1<-test[cols]
result<-predict(nb,test1)
result<-result$class
confusionMatrix(result, test$V9)$overall["Accuracy"]
