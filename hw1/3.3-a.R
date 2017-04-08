heart <- read_csv("~/cs498aml/hw1/3-3.data", col_names = FALSE)
library(caret)
#dropping rows with NA values
heart<-na.omit(heart)
#change class 1, 2, 3, 4 to 1
heart$X14[heart$X14>0] <- 1
#doing cross validation 10 times
rowNums <- createDataPartition(heart$X14, 10, 0.85)
accuracy=vector(mode="numeric", length=0)
for (i in rowNums) {
  training <- heart[i,]
  test <- heart[-i,]
  model<-train(x=training[1:13],y=as.factor(training$X14), method="nb",trControl=trainControl(method = "cv", number = 10))
  prediction <- predict(model, newdata =test[1:13])
  #storing the accuracy of each fold into a vector
  acc <- confusionMatrix(data=prediction, test$X14)$overall['Accuracy']
  accuracy = c(accuracy, acc)
}
sdacc = sd(accuracy)
meanacc = mean(accuracy)