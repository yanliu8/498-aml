diatetes <- read.csv("~/cs498aml/hw1/3-1.data", header=FALSE)
trainingRowNum<-createDataPartition(diatetes$V9,1,0.8)
training<-diatetes[trainingRowNum$Resample1,]
test<-diatetes[-trainingRowNum$Resample1,]
model <- svmlight(training[1:8], training$V9, temp.dir="~/cs498aml/hw1", pathsvm="/Users/liu/cs498aml/hw1/svm_light_osx.8.4_i7")
prediction <- predict(model, test[1:8])$class
cm<-confusionMatrix(data=prediction, test$V9)
