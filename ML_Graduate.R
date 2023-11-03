#regression for graduate
rm(list=ls())
install.packages("readxl")
install.packages("caret")
install.packages("kernlab")
library(readxl)
library(caret)
library(kernlab)

library(rgdal)
library(tree)
library(randomForest)
library(gbm)

#data
library(dplyr)

cl.df <- read_excel("/Users/kihun/Downloads/cl_union_new.xlsx")
metro.df <- read.csv("/Users/kihun/Downloads/metro_union_final.csv",fileEncoding="euc-kr")
total.df <- rbind(cl.df, metro.df[,c(1:8)])
sample.df <- total.df %>% filter(!is.na(PM))
sample.df <- as.data.frame(sample.df)



sample.df$PM<-scale(sample.df$PM)
sample.df$Land_Diversity<-scale(sample.df$Land_Diversity)
sample.df$Traffic<-scale(sample.df$Traffic)
sample.df$Building<-scale(sample.df$Building)
sample.df$NDVI<-scale(sample.df$NDVI)
sample.df$Land_Mode <- as.factor(sample.df$Land_Mode) #토지이용을 명목변수로로
sample.df$CoolingRoad <- as.factor(sample.df$CoolingRoad)

##
ex <- dummyVars(~., data=sample.df)
ex.df <- data.frame(predict(ex,newdata=sample.df))
n <- nrow(ex.df)
ex.idx <-c(1:10,sample(14:n, 40))
train.ex <- ex.df[sample.idx, ]
test.ex <- ex.df[-sample.idx,]
##
attach(sample.df)

ggplot(sample.df)+geom_point(mapping=aes(x=sample.df$Traffic, y=sample.df$PM))

#divine data
set.seed(1)
set.seed(2)
set.seed(3)
set.seed(4)
set.seed(5)
set.seed(6)
n <- nrow(sample.df)
sample.idx <-c(1:13,sample(14:n, 230))
train.df <- sample.df[sample.idx, ]
test.df <- sample.df[-sample.idx,]

f.seoul <- formula(PM~NDVI+Land_Mode+Land_Diversity+Traffic+Building+CoolingRoad)

##boosting
boost.seoul <- gbm(f.seoul, data = sample.df, distribution = "gaussian", 
                   n.trees = 1000, interaction.depth = 2)
summary(boost.seoul)
##
boost.seoul.cv <- gbm(f.seoul, data = sample.df, distribution = "gaussian", 
                      n.trees = 1000, interaction.depth = 2, cv=10, shrinkage=0.001)  
summary(boost.seoul.cv)
best.iter <- gbm.perf(boost.seoul.cv, method = 'cv')
best.iter
##
boost.seoul <- gbm(f.seoul, data = sample.df, distribution = "gaussian", 
                   n.trees = best.iter, interaction.depth = 2, shrinkage=0.001)
summary(boost.seoul)
#predict
yhat.boost <- predict(boost.seoul, newdata = sample.df, n.trees = best.iter)
cl.data <- sample.df
cl.data$CoolingRoad <- 1
yhat.boost.cl <- predict(boost.seoul, newdata=cl.data, n.trees=best.iter)

plot(sample.df$PM, yhat.boost.cl)
abline(0,1)

accuracy(yhat.boost, sample.df$PM)
cor(yhat.boost,sample.df$PM)^2

accuracy(yhat.boost.cl, sample.df$PM)
cor(yhat.boost.cl,sample.df$PM)^2

plot(boost.seoul, i = "Land_Diversity")
plot(boost.seoul, i = "Land_Mode")
plot(boost.seoul, i = "Building")
plot(boost.seoul, i = "Traffic")
plot(boost.seoul, i = "NDVI")


##SVM regression
#
install.packages("e1071")
library(e1071)
#gamma : 작을수록 모든 점에 대한 영향을 받음 ; 1
#cost : 클수록 더욱더 데이터에 적합. -> overfitting 가능성.. ; 작으면 decision boundary 가 일반화
out <- svm(f.seoul, data=sample.df, kernel="radial",cost=5, gamma=1,subset = c(sample(dim(sample.df)[1],1000,replace=T)))
summary(out)
plot(out$fitted, sample.df$PM,xlab="fitted", ylab="PM10", main="SVM with y=x")
abline(a=0, b=1, col='red')
# => cost 값을 돌려봤을때 5에서 가장 적은 이상치와 y=x에 대해 적합이 잘 됨 / 5이후로는 그래프의 변화 적음.
#
pred.svm <- predict(out, newdata=sample.df[,c(3:7)])
plot(pred.svm, sample.df$PM, xlab="fitted", ylab="PM10", main="Predicted PM10 with y=x")
abline(a=0, b=1, col='red')
mean((pred.svm - sample.df$PM)^2)

###SVM
set.seed(111)
sample.df.1 <- sample.df
sample.df.1$CoolingRoad <- 1

tune.out=tune(svm, f.seoul, data=sample.df, kernel="radial", ranges=list(cost=c(0.01,0.1,1,5,10,50,100,1000), gamma=c(0.5,1,2,3,4)))
tune.out=tune(svm, f.seoul, data=sample.df, kernel="radial", ranges=list(cost=c(1,10), gamma=c(1,5)))
summary(tune.out)
svm_best = tune.out$best.model
pred=predict(svm_best,newdata=sample.df)

plot(sample.df$PM, pred,xlab="PM10", ylab="Fitted", main="SVM")
abline(0,1)

summary(svm_best)
accuracy(pred,sample.df$PM)

cor(pred,sample.df$PM)^2


pred.1 <- predict(svm_best, sample.df.1)
plot(sample.df.1$PM, pred.1, xlab="PM", ylab="Predict", main="SVM - Predict")
abline(0,1, col='red')


diff <- pred.1 - sample.df.1$PM
final_svm <- cbind(sample.df, diff)
final_svm.ord <- final_svm[order(final_svm$diff),]
final_svm.ord[,final_svm.ord$CoolingRoad!=1]
final_svm.ordx <- final_svm.ord %>% filter(CoolingRoad != 1)
final_select <- final_svm.ordx[c(1:34),]
write.csv(final_select,"/Users/kihun/Downloads/svm_select.csv",fileEncoding="euc-kr")

##dummy variable###########################################
tune.out=tune(svm, PM~., data=train.ex, kernel="radial", ranges=list(cost=c(0.01,0.1,1,5,10,50,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
svm_best.ex = tune.out$best.model

pred=predict(svm_best.ex,newdata=test.ex)
mean((pred-mean(test.df$PM))^2)/mean((pred-test.df$PM)^2)
mean((pred-test.df$PM)^2) ##SSE
mean((pred-mean(test.df$PM))^2) ##SSR
mean((test.df$PM-mean(test.df$PM))^2)##SST

mean((pred-mean(test.df$PM))^2)/mean((test.df$PM-mean(test.df$PM))^2)

summary(svm_best)
accuracy(pred,test.ex$PM)

plot(pred,test.ex$PM)
abline(0,1)
lines(pred,col='red')
lines(pred-test.df$PM,col='blue')

##########################################################################
## Land_mode -> weight
smp.df <- sample.df
ldmd <- c(unique(smp.df$Land_Mode))

for(i in 1:nrow(smp.df)) {
  for(j in 1:length(ldmd)){
    if(smp.df$Land_Mode[i] == ldmd[j]){
      smp.df$Land_Mode_weight[i] = sum(smp.df$Land_Mode==ldmd[j])/nrow(smp.df)
    } 
  }
}
smp.df <- smp.df[,-5]
#
n <- nrow(smp.df)
smp.idx <-c(1:13,sample(14:n, 195))
train.smp.df <- smp.df[sample.idx, ]
test.smp.df <- smp.df[-sample.idx,]

###Boosting
f.seoul <- formula(PM~NDVI+Land_Mode_weight+Land_Diversity+Traffic+Building+CoolingRoad)
boost.seoul <- gbm(f.seoul, data = train.smp.df, distribution = "gaussian", 
                   n.trees = 5000, interaction.depth = 2)
summary(boost.seoul)
##
boost.seoul.cv <- gbm(f.seoul, data = train.smp.df, distribution = "gaussian", 
                      n.trees = 1000, interaction.depth = 2, cv=10, shrinkage=0.001)  
summary(boost.seoul.cv)
best.iter <- gbm.perf(boost.seoul.cv, method = 'cv')
best.iter
##
boost.seoul <- gbm(f.seoul, data = train.smp.df, distribution = "gaussian", 
                   n.trees = best.iter, interaction.depth = 2, shrinkage=0.001)
summary(boost.seoul)
#predict
yhat.boost <- predict(boost.seoul, newdata = test.smp.df, n.trees = best.iter)
mean((yhat.boost - test.df$PM)^2)
plot(yhat.boost, test.df$PM)
abline(0,1)
cor(x=yhat.boost,test.smp.df$PM)^2

###SVM
tune.out=tune(svm, PM~., data=train.smp.df, kernel="radial", ranges=list(cost=c(0.01,0.1,1,5,10,50,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
svm_best = tune.out$best.model

pred=predict(svm_best,newdata=test.smp.df)

plot(pred,test.smp.df$PM,xlab="PM",ylab="Predict",main="SVM")
abline(0,1)
cor(x=pred,test.smp.df$PM)

mean((pred - test.smp.df$PM)^2) #모델 정확도2
cor(x=pred, test.smp.df$PM)^2
accuracy(pred,test.smp.df$PM)
######################################################################
#Variance Importance
install.packages("vip")
library(vip)
pfun <- function(object, newdata) predict(object, newdata = newdata)
set.seed(1)
vip(svm_best, method = "permute", train = sample.df, target = "PM", nsim = 50,
    metric = "rmse", pred_wrapper = pfun,
    aesthetics = list(color = "grey50", fill = "grey50"),
    all_permutations = TRUE, jitter = TRUE)

summary(pm.lm)
