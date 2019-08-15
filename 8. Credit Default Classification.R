data=read.csv('Credit_Default_final.csv')
data=na.omit(data) # since only 2 missing values, in the same row, its better to skip the row.


nums <- sapply(data, is.numeric)
nums['Type']=FALSE
data[nums]=scale(data[nums])
fac <- sapply(data, is.factor)

myFun_FacToNum= function(x){
  return(as.numeric(sub(",","",x)))
};
data[fac]=sapply(data[fac], myFun_FacToNum)


library(caTools)

type(data)
set.seed(100)
split=sample.split(data$Type, SplitRatio = 0.8)
train=subset(data, split==TRUE)
test=subset(data, split==FALSE)


model_lm=glm(formula = train$Type~., family = binomial, data=train)

result_lm = predict.glm(model_lm,test,type="response")
confusionMatrix_lm=table(test$Type, result_lm > 0.5)

confusionMatrix_lm





# Random Forest


#install.packages('randomForest')
library(randomForest)
model_RF = randomForest(train[,"Type"] ~ ., 
                     data = train ,
                     method='class',
                     importance = TRUE,
                     mtry = 6,
                     ntree = 100)

result_RF=predict(model_RF,test,type = 'response')

confusionMatrix_RF=table(test$Type, result_RF > 0.5)
confusionMatrix_RF


# LETS COMPARE LM and RF result


ModelEvalMeasure_classification = function(confusionMatrix)
{
  TN=confusionMatrix[1]
  FN=confusionMatrix[2]
  FP=confusionMatrix[3]
  TP=confusionMatrix[4]
  
  accuracy=(TN+TP)/(TN+FN+FP+TP)
  sensitivity=TP/(TP+FN)
  specificity=TN/(FP+TN)
  precision = TP/(TP+FP)
  
  measure=data.frame(accuracy,sensitivity,specificity,precision,TN,FN,FP,TP)
  return(measure)
}
ModelEvalMeasure_classification(confusionMatrix_lm)
ModelEvalMeasure_classification(confusionMatrix_RF)

