setwd("E:/R/datasets")
#Reading data 
ctg <- read.csv("CTG.csv", na.strings = c("","NA"))

#checking null values
sapply(ctg, function(df)
{
  sum(is.na(df)==T)/length(df)
})

#data type conversion 
ctg$NSP <- as.factor(ctg$NSP)
ctg$Tendency <- as.factor(ctg$Tendency)

#Data partition 
library(caret)
index_ctg <- createDataPartition(ctg$NSP,p=0.7, list = FALSE)
ctg_train <- ctg[index_ctg,]
ctg_test <- ctg[-index_ctg,]

#model 
library(randomForest)
model_rf <-randomForest(ctg_train$NSP ~., ctg_train, ntree=300, mtry=8)

#Prediction 
pred_rf <- predict(model_rf, ctg_test, type = "response")
pred_rf

#Confusion matirx 
confusionMatrix(ctg_test$NSP, pred_rf)

#Tuning mtry
t<-tuneRF(ctg_train[,-22],ctg_train[,22]
          ,stepFactor = 0.5,
          plot=TRUE,
          ntree = 300,
          trace = TRUE,
          improve = 0.5)
print(t)


#Variables importance 
varImpPlot(model_rf, sort = TRUE )

importance(model_rf)

varUsed(model_rf)



