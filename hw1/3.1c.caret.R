library(caret)
trainingRowNum<-createDataPartition(diatetes$V9,1,0.8)
training<-diatetes[trainingRowNum$Resample1,]
test<-diatetes[-trainingRowNum$Resample1,]
predictors <- training[1:8]
model<-train(x=predictors,y=as.factor(training$V9), method="nb",trControl = trainControl(method = "cv", number = 10, na.omit))
result <- predict(model, newdata=test[1:8])
cm<-confusionMatrix(data=result, test$V9)
