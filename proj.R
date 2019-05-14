install.packages("caTools")
install.packages("leaps")
library(leaps)
library(Hmisc)
library(ggplot2)
library(caTools)
library(dummies)
setwd("/Users/Abby/Documents/isom3530/proj/")
data <- read.csv("bank_marketing.csv",sep=";") 
summary(data)

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
data <- data[data$job!="unknown" & data$marital!="unknown"& data$education!="unknown"& data$poutcome!="nonexistent"&
             data$default!="unknown" & data$housing!="unknown"& data$loan!="unknown",]
data <- droplevels(data)


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
names(data.new)
names(data.new)[names(data.new) == "job.blue-collar"] <- "job.blue_collar"
names(data.new)[names(data.new) == "job.self-employed"] <- "job.self_employed"

#split into training and testing set##########################  
split <- sample.split(data.new$y, SplitRatio=0.7)
train <- subset(data.new, split == TRUE)
test <- subset(data.new, split == FALSE)
nrow(train)
nrow(test)

#pairs(y~age+job.blue_collar+job.entrepreneur+job.housemaid+job.management+job.retired+
#        job.self_employed+job.services+job.student+job.technician+job.unemployed
#        +marital.married+marital.single+education.basic.6y+education.basic.9y+education.high.school+
#        education.illiterate+education.professional.course+education.university.degree+default.yes
#        +housing.yes + loan.yes+contact.telephone+month.may+day_of_week.tue+poutcome.success+age+duration, data=data.new)

#subset selection########################## 
#exhaustive search
subsets <- regsubsets(x=as.matrix(train[,1:46]),y=as.matrix(train[,"y"]))
plot(subsets, main="BIC") #month.may, campaign, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m
plot(subsets,scale="Cp",main="Cp") #job.retired, job.technician, month.may, campaign, previous, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed
pairs(y~month.mar + pdays + emp.var.rate+cons.price.idx+cons.conf.idx+ poutcome.success+duration, data=data.new)
#model.dual <-step(lm(y~1,data=train),direction="both",scope=~month.may + campaign+ emp.var.rate+cons.price.idx+cons.conf.idx+ euribor3m)
#summary(model.dual)
#Stepwise########################## 
nullmodel<- glm(y ~ 1, data = train) #only for the intercept
fullmodel<- glm(y ~ ., data = train) #includes all variables
model.step<- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel),direction = "both")


summary(model.step)
########################## 
#pdays, duration, cons.conf.idx, poutcome.success, cons.price.idx, month.mar, emp.var.rate
