heart <- read_csv("~/cs498aml/hw1/3-3.data", col_names = FALSE)
library(caret)
heart<-na.omit(heart)
rowNums <- createDataPartition(heart$X14, 10, 0.85)
accuracy=vector(mode="numeric", length=0)
for (i in rowNums) {
  training <- heart[i,]
  test <- heart[-i,]
  model<-train(x=training[1:13],y=as.factor(training$X14), method="nb",trControl=trainControl(method = "cv", number = 10))
  prediction <- predict(model, newdata =test[1:13])
  acc <- confusionMatrix(data=prediction, test$X14)$overall['Accuracy']
  accuracy = c(accuracy, acc)
}
sdacc = sd(accuracy)
meanacc = mean(accuracy)