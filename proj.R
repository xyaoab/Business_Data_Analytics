install.packages("caTools")
install.packages("pROC")
install.packages("leaps")
library(leaps)
library(Hmisc)
library(ggplot2)
library(caTools)
library(dummies)
library(rsample)     # data splitting 
library(dplyr)       # data wranglingyes
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
setwd("/Users/Abby/Documents/isom3530/proj/")
data <- read.csv("bank_marketing.csv",sep=";") 

#summary: portion of missing values "unknown "availavle########################## 
sapply(1:ncol(data),function(x) paste(names(data)[x],":",typeof(data[,x]),sep=""))
data$age <- as.numeric(as.character(data$age))
data$duration <- as.numeric(as.character(data$duration))
data$campaign <- as.numeric(as.character(data$campaign))
data$pdays <- as.numeric(as.character(data$pdays))
data$previous <- as.numeric(as.character(data$previous))
data <- data[complete.cases(data),]
Hmisc::describe(data) 
head(data)

##missing unknown remove ########################## 
data <- data[data$job!="unknown" & data$marital!="unknown"& data$education!="unknown"& # data$poutcome!="nonexistent"&
             data$default!="unknown" & data$housing!="unknown"& data$loan!="unknown",]
data <- droplevels(data)
# histogram visualize categorical data########################## 
ggplot(data, aes(x=job, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=marital, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=education, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=default, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=housing, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=loan, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=contact, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=month, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=day_of_week, color=y)) +geom_bar(fill="white")
ggplot(data, aes(x=poutcome, color=y)) +geom_bar(fill="white")


#box plot visualize numerical data########################## 
ggplot(data, aes(x=y, y=age, color=y)) + geom_boxplot()
ggplot(data, aes(x=y, y=duration, color=y)) + geom_boxplot()
ggplot(data, aes(y=campaign, x=y, color=y)) + geom_boxplot()
ggplot(data, aes(y=pdays, x=y, color=y)) + geom_boxplot()
ggplot(data, aes(y=previous, x=y, color=y)) + geom_boxplot()
ggplot(data, aes(y=emp.var.rate, x=y, color=y)) + geom_boxplot()
ggplot(data, aes(y=cons.price.idx, x=y, color=y)) + geom_boxplot()
ggplot(data, aes(y=euribor3m, x=y, color=y)) + geom_boxplot()
ggplot(data, aes(y=nr.employed, x=y, color=y)) + geom_boxplot()

#standardize numerical data########################## 
data$age <- scale(data$age,center=TRUE, scale=TRUE)
data$duration  <- scale(data$duration, center=TRUE, scale=TRUE)
data$campaign   <- scale(data$campaign ,center=TRUE, scale=TRUE)
data$pdays   <- scale(data$pdays,center=TRUE, scale=TRUE)
data$previous  <- scale(data$previous,center=TRUE, scale=TRUE)
data$emp.var.rate   <- scale(data$emp.var.rate ,center=TRUE, scale=TRUE)
data$cons.price.idx <- scale(data$cons.price.idx, center=TRUE, scale=TRUE)
data$cons.conf.idx   <- scale(data$cons.conf.idx,center=TRUE, scale=TRUE)
data$euribor3m <- scale(data$euribor3m,center=TRUE, scale=TRUE)
data$nr.employed   <- scale(data$nr.employed,center=TRUE, scale=TRUE)
summary(data)

#density plot visualize numerical data########################## 
ggplot(data) + geom_density(aes(x=age,color = y))
ggplot(data) + geom_density(aes(x=duration,color = y))
ggplot(data) + geom_density(aes(x=campaign,color = y))
ggplot(data) + geom_density(aes(x=pdays,color = y))
ggplot(data) + geom_density(aes(x=previous,color = y))
ggplot(data) + geom_density(aes(x=emp.var.rate ,color = y))
ggplot(data) + geom_density(aes(x=cons.price.idx,color = y))
ggplot(data) + geom_density(aes(x=euribor3m ,color = y))
ggplot(data) + geom_density(aes(x=nr.employed,color = y))




# y = 1/0########################## 
data$y <- ifelse(data$y=="yes",1,0)
data$y<- as.numeric(as.character(data$y))

#dummy variable for categorical data########################## 
data.new <- dummy.data.frame(data=data,c("job","marital","education","default","housing"
                                         ,"loan","contact","month","day_of_week","poutcome"),sep = ".")
#Drop a reference group########################## 
data.new$'job.admin.' <- NULL
data.new$'marital.divorced' <- NULL
data.new$'education.basic.4y' <- NULL
data.new$'default.no' <- NULL
data.new$'housing.no' <- NULL
data.new$'loan.no' <- NULL
data.new$'contact.cellular' <- NULL
data.new$'month.apr' <- NULL
data.new$'day_of_week.mon' <- NULL
data.new$'poutcome.failure' <- NULL
names(data.new)[names(data.new) == "job.blue-collar"] <- "job.blue_collar"
names(data.new)[names(data.new) == "job.self-employed"] <- "job.self_employed"
names(data.new)

####
data.new <- data
data.new$job <- as.numeric(factor(data$job))
data.new$marital <- as.numeric(factor(data$marital))
data.new$education <- as.numeric(factor(data$education))
data.new$default <- as.numeric(factor(data$default))
data.new$housing <- as.numeric(factor(data$housing))
data.new$loan <- as.numeric(factor(data$loan))
data.new$contact <- as.numeric(factor(data$contact))
data.new$day_of_week <- as.numeric(factor(data$day_of_week))
data.new$month <- as.numeric(factor(data$month))
data.new$poutcome <- as.numeric(factor(data$poutcome))

#split into training and testing set##########################  

split <- sample.split(data.new$y, SplitRatio=0.7)
train <- subset(data.new, split == TRUE)
test <- subset(data.new, split == FALSE)
nrow(train)
nrow(test)

#subset selection########################## 
#exhaustive search
head(train)
subsets <- regsubsets(x=as.matrix(train[,1:20]),y=as.matrix(train[,"y"]))
plot(subsets, main="BIC") #month.may, month.mar, month.nov, duration, pdays, emp.var.rate, euribor3m, nr.employed
#contact+month+duration+pdays+ emp.var.rate + euribor3m+cons.price.idx+cons.conf.idx
plot(summary(subsets)$adjr2 ,xlab="Number of Variables ",ylab="adjr2", type="l")
plot(summary(subsets)$bic ,xlab="Number of Variables ",ylab="bic", type="l")
plot(summary(subsets)$cp ,xlab="Number of Variables ",ylab="cp", type="l")

#pairs(y~month.mar + pdays + emp.var.rate+cons.price.idx+cons.conf.idx+ poutcome.success+duration, data=data.new)
model.dual <-step(lm(y~1,data=train),direction="both",scope=~contact+month+duration+
                    pdays+emp.var.rate + euribor3m+cons.price.idx+cons.conf.idx)
summary(model.dual)
#Stepwise########################## 
library(car)
vif(model.dual)
####linear model 
install.packages("EnvStats")
library(EnvStats)
model1 <- lm(y~1, data=train)
anova(model1,model.dual)
#########model
nullmodel<- glm(y ~ 1, data = train) 
log.model <- glm(y~contact+month+duration+pdays+ emp.var.rate +cons.price.idx+cons.conf.idx,family=binomial(),train,maxit=100)
summary(log.model)

library(lmtest)
require(DescTools)
lrtest(log.model, nullmodel)
PseudoR2(log.model, which="all")

prediction <- predict(log.model,newdata = test,type="response")
library(caret)

conMat <- confusionMatrix(data=factor(as.numeric(prediction>0.5)),reference=factor(test$y))
TP <- conMat$table[2,2]
TN <- conMat$table[1,1]
FP <- conMat$table[2,1]
FN <- conMat$table[1,2]

N <- TN + FP
P <- TP + FN

FPRate <- FP/N #False positive rate = 1 - Specificity = Type 1 error
TPRate <- TP/P #Sensitivity OR 1-Type II error
Specif <- TN/N #Specificity
Pos.Pred.value <- TP/(TP+FP) #Precision, or Positive prediction value
Neg.Pred.value <- TN/(TN+FN)
overall.error.rate <- (FP+FN)/(N+P)
Accuracy <- 1-overall.error.rate

library(car)
vif(log.model)
table(test$y,as.numeric(prediction >=0.25))

#Plot a graph of error rate vs different threshold
thresholdList <- seq(0.01,0.87,by=0.01)
i <- 1
errorRateList <- array(dim=87)
SpecList <- array(dim=87)
SensitList <- array(dim=87)
for(t in thresholdList){ #t<-0.5
  temp.confusion.matrix <- table(test$y,prediction >=t)
  temp_FP <- temp.confusion.matrix[1,"TRUE"]
  temp_FN <- temp.confusion.matrix[2,"FALSE"]
  temp_TP <- temp.confusion.matrix[2,"TRUE"]
  temp_TN <- temp.confusion.matrix[1,"FALSE"]
  errorRateList[i] <- (temp_FP+temp_FN)/(temp_FP+temp_FN+temp_TP+temp_TN)
  SpecList[i] <- temp_TN/(temp_FP+temp_TN)
  SensitList[i] <- temp_TP/(temp_TP+temp_FN)
  i<- i+1
}


plot(thresholdList,1-errorRateList,type="l",col="red",xlim=c(0,1),ylim=c(0,1))#accurancy
lines(thresholdList,SpecList,col="blue")
lines(thresholdList,SensitList,col="green")
legend(0.1,0.3,legend=c("Accuracy","Specificity","Sensitivity"),col=c("red","blue","green"),lty=1)

#ROC curve
plot(1-SpecList,SensitList,type="l",xlim=c(0,1),ylim=c(0,1))
abline(0,1)
#using function
library(pROC)
roc_obj <- roc(test$y, prediction)
plot(roc_obj)
auc(roc_obj) #Find the auc

#-------Find the decision rule: cost-sensitive classification-------- t=0.25
#Find the decision rule that minimize the cost.
#e.g. 0-1 Loss
i <- 1
costList <- array(dim=87)
for(t in thresholdList){
  temp.confusion.matrix <- table(test$y,prediction >=t)
  temp_FP <- temp.confusion.matrix[1,2]
  temp_FN <- temp.confusion.matrix[2,1]
  temp_TP <- temp.confusion.matrix[2,2]
  temp_TN <- temp.confusion.matrix[1,1]
  temp_cost <- temp_FN*2 + temp_FP
  costList[i] <- temp_cost
  i<- i+1
}
plot(thresholdList,costList,type="l")
best_threshold <- thresholdList[which.min(costList)]
best_threshold #0.25

#Get the rates using "confusionMatrix" function directly
#Note: You need to put the class you refer as "positive" to the first level.
conMat <- confusionMatrix(data=factor(as.numeric(prediction>0.25),levels=c(1,0)),reference=factor(test$y,levels=c(1,0)))

conMat$byClass #You can get all the rates directly using this function.

#check
TP <- conMat$table[1,1]
TN <- conMat$table[2,2]
FP <- conMat$table[1,2]
FN <- conMat$table[2,1]

#Sensitivity
Sensit <- TP/(TP+FN)
conMat$byClass["Sensitivity"] #same

#Specificity
Spec <- TN/(TN+FP)
conMat$byClass["Specificity"] #same



####################classficiation tree####################
data.new <- dummy.data.frame(data=data,c("job","marital","education","default","housing"
                                         ,"loan","contact","month","day_of_week","poutcome"),sep = ".")
names(data.new)[names(data.new) == "job.blue-collar"] <- "job.blue_collar"
names(data.new)[names(data.new) == "job.self-employed"] <- "job.self_employed"
names(data.new)
par(mfrow=c(1,1))
# grow tree 
fit <- rpart(y~.,method="class", data=train,parms=
               list(loss=matrix(c(0,1,1,0),byrow=TRUE,nrow=2)))
#Default, we use GINI
printcp(fit)
# prune the tree 
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
printcp(pfit) # display the results 
plotcp(pfit) # visualize cross-validation results 
summary(pfit) # detailed summary of splits
# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Subscription")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

plot(pfit$variable.importance)
pfit$variable.importance

pfit.pred <- predict(pfit, test,type="class")
conf <-table(pfit.pred,test$y)
library(caret)
sensitivity(conf)
specificity(conf)
confusionMatrix(conf)
library(InformationValue)
misClassError(pfit.pred,test$y)

roc_obj <- roc(test$y, as.numeric(as.character(pfit.pred)))
plot(roc_obj)
auc(roc_obj)





