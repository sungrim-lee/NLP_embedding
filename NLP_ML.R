library(caret)
library(xgboost)
library(randomForest)

s$rtn <-ROC(s$adjusted)

s <- subset(s, select =-logreturn )
s$class <- ifelse(s$rtn < -0.01 , 1, ifelse(s$rtn > 0.01, 4, ifelse(s$rtn > 0,3,2))) #1%
s$class <- as.factor(s$class)

s$class[1:length(s$class)]<-s$class[-1]

s<-s[20:1962,]
#s<-subset(s,select = -sigmaDay60)
s$adjusted<-scale(s$adjusted)
s$betaYearDay<-scale(s$betaYearDay)
s$sigmaDay60<-scale(s$sigmaDay60)
s$pbr<-scale(s$pbr)
s$institutional<-scale(s$institutional)
s$foreigners<-scale(s$foreigners)
s$kodexGBond3Year<-scale(s$kodexGBond3Year)
s$vkospi200<-scale(s$vkospi200)
s$wonPerYen<-scale(s$wonPerYen)
s$wonPerDollar<-scale(s$wonPerDollar)
s$dowjones<-scale(s$dowjones)
s$shanghai<-scale(s$shanghai)
s$goldFuture<-scale(s$goldFuture)
s$wtiNY<-scale(s$wtiNY)
s$kospi<-scale(s$kospi)
s$kospi200IT<-scale(s$kospi200IT)
s$spread<-scale(s$spread)
s$rtn<-scale(s$rtn)

sumAccuracy<-c()
sumDirAcc<-c()
sumThree<-c()
t<-table(c(1:4),c(1:4))-table(c(1:4),c(1:4))

# svm
# prediction window
for(i in 1:479){
  trainS <- s[((1240+i):(1463+i)), ]
  testS <- s[(1464+i), ]

  trainS <- as.data.frame(trainS)
  testS <- as.data.frame(testS)

  test<- subset(testS, select=-dates)
  train<- subset(trainS, select=-dates)

  model <-svm(train$class ~., data=train, degree = 5 , kernel="polynomial", cost=50)
  pred1<-predict(model, test, type="class")

  test[is.na(test)==TRUE]
  train[is.na(train)==TRUE]
}

#XGBoost
runxg<-function(round=100){
  idx<-createDataPartition(dtm_logreturn2$class,p=0.8)

  train<-dtm_logreturn2[idx$Resample1,]
  test<-dtm_logreturn2[-idx$Resample1,]
  table(train$class)
  table(test$class)

  param <- list("objective"="multi:softprob",
                "eval_metric"="mlogloss","num_class"=2)

  x<-as.matrix(train[,-c(ncol(train))])
  y<-as.integer(as.matrix(train[,c("class")]))

  m<-xgboost(param=param, data=x,label=y,nrounds = round)
  pred<-predict(m, as.matrix(test[,-c(ncol(train))]))
  pred_matrix <- matrix(pred,ncol = 2, nrow = length(pred)/2, byrow = TRUE)
  pred_return<-max.col(pred_matrix)
}

#random forest
runrf<-function(round=100){
  #for(i in 1:366){
  idx<-createDataPartition(dtm_logreturn2$class,p=0.7)

  #train<-dtm_logreturn2[i:(i+224),]
  #test<-dtm_logreturn2[(i+225),]

  train<-dtm_logreturn2[idx$Resample1,]
  test<-dtm_logreturn2[-idx$Resample1,]

  table(train$class)
  table(test$class)

  x<-as.matrix(train[,-c(ncol(train))])
  y<-as.integer(as.matrix(train[,c("class")]))

  m<-randomForest(class~.,data=train,ntree=100,proximity=TRUE)
  pred<-predict(m, newdata = test)

  t<-table(pred,test[,ncol(test)])

#  acc<-round(mean(pred == test[,ncol(test)])*100,2)
}
