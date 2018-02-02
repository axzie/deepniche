dataset <- rbind(neutral_m,niche_m)

ggplot() +
  geom_boxplot(data=dataset,aes(x=model,y=beta_mean,col=model))

ggplot() +
  geom_point(data=dataset,aes(x=hill1_mean,y=hill2_mean,col=model))

library(caret)

#data partioning
partitionIndex <- createDataPartition(dataset[,1],p=.7,list = F)
train_set <- dataset[partitionIndex,]
test_set <- dataset[-partitionIndex,]

preP <- preProcess(train_set,method = c("center","scale"))

train_set <- predict(preP,train_set)
test_set <- predict(preP,test_set)

##rpart
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 5, 
                           classProbs = TRUE)

nnetFit <- train(model ~ ., 
                 data = train_set,
                 method = "glm",
                 trControl = fitControl)

plot(nnetFit)
varImp(nnetFit)

predictions <- predict.train(nnetFit,newdata=test_set[,-which(colnames(test_set)=="model")],type = "prob")
confusionMatrix(predictions,test_set[,which(colnames(test_set)=="model")])











##nnet
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 5, 
                           classProbs = TRUE)

nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 5.1, by = 0.5))

nnetFit <- train(model ~ ., 
                 data = train_set,
                 method = "nnet",
                 metric = "Accuracy",
                 trControl = fitControl,
                 tuneGrid = nnetGrid,
                 verbose = FALSE)

plot(nnetFit)
varImp(nnetFit)

predictions <- predict.train(nnetFit,newdata=test_set[,-which(colnames(test_set)=="model")],type = "prob")
confusionMatrix(predictions,test_set[,which(colnames(test_set)=="model")])


