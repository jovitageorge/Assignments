data <- read.csv("F:\\assignment\\rcodes\\logistic regression\\bank-full.csv")
library(corrplot)
library(ggplot2)
library(corrplot)
install.packages("ResourceSelection")
library(ResourceSelection)
install.packages("pscl")
library(pscl)
install.packages("pROC")
library(pROC)
install.packages("e1071")
install.packages("pastecs")
install.packages("leaps")
install.packages("ISLR")
install.packages("glmnet")
library(e1071)
library(pastecs)
library(leaps)
library(ISLR)
library(glmnet)
install.packages("rpart")
install.packages("randomForest")
library(rpart)
library(randomForest)
install.packages("rpart.plot")
library(rpart.plot)
summary(data)
str(data)
#make all variables numeric and scale the data and 
#show that our data has no missing values and run a correlation plot 
data1 <- data.frame(lapply(data,as.numeric))
str(data1)
attach(data)
library(caret)
install.packages("caretEnsemble")
library(caretEnsemble)
install.packages("ROSE")
library(ROSE)
install.packages("mlbench")
library(mlbench)
install.packages("DMwR")
library(DMwR)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("RColorBrewer")
library(RColorBrewer)
data2 <- data.frame(scale(data1))
str(data2)
sum(is.na(data2))
library(corrplot)
BANKDATA <- cor(data2, method = "pearson")
corrplot(BANKDATA, method = "circle", type = "lower",tl.col = "black",tl.srt = 45)
xyz <-lm(y~duration, data = data2)
summary(xyz)
xyz1 <- lm(y~job,data = data2)
summary(xyz1)
xyz2 <- lm(y~education, data=data2)
summary(xyz2)
xyz3 <- lm(y~age,data=data2)
summary(xyz3)
xyz4 <- lm(y~housing, data = data2)
summary(xyz4)
xyz5 <- lm(y~ balance, data = data2)
summary(xyz)
#This result shows that duration, job, education, age, housing and balance are all highly significant
#when it comes to predicting a sign up during a marketing campaign. 
#This explains the variance in the dependent variable.
#We will develop a ROC curve and convert the coeficients to percentages.
head(data1)
str(data1)
logistic <- glm(y~age+job+martial,family = binomial(link = "logit",data = data1))
summary(logistic)
exp(coef(logistic))
library(ResourceSelection)
#divide data set into test and train
#crave off 80% of the data and 20% of data
library(ISLR)
attach(data1)
head(data1)
str(data1)
table(data1$y)
tail(Default,2000)
str(data1)
#So let's try to predict who will sign up by using a machine learning process of splitting the data into train and test segmenets. We are going to to go with a 80 train 20% test appoarch.
train <- data1[1:8000, ]
test <- data1[8001:10000, ]
#Next we will run our logistic regression model, I'm adding the logit phrase but binomial does this by default so it is not necessary. 
logmodel <- glm(y~.,family="binomial",train)
summary(logmodel)
#We drop balance, job, poutcome
logmodel2 <- glm(y~age+marital+education+default+contact+duration+campaign,family="binomial"(link = "logit"),train)
summary(logmodel2)
#We will stick with logmodel 2. The output is express in log odds, but we can convert to % via exp() function
model2.ouput <- exp(coef(logmodel2))
model2.ouput
prediction.model <- predict.glm(logmodel2,test,type='response')
#We are classifying the output if greater than .05 label as 1 if not lable as a 0
head(prediction.model)
library(dplyr)
test %>% group_by(default) %>%
  summarise(no_rows = length(default))
head(test$signed.up)
model3hit.1 <- mean(prediction.model!=as.numeric(test$signed.up))
model3hit.1
#We got it right 16 percent of the time, not great. 
#Now let's make a roc curve, just to confirm our model, using a different package:
install.packages("ROCR")
library(ROCR)
#set the prediction
newpred <- prediction(prediction.model,test$y)
#to measure true positives which is "tpr" and false positives"fpr"
newpred.performance <- performance(newpred, measure = "tpr",x.measure = "fpr")
#plot these  to measures
plot(newpred.performance)
#get the AUC again using the performance function
AUC <- performance(newpred, measure = "auc")
AUC
