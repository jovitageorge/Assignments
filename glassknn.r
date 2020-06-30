install.packages("caTools")
#for train and test data split
install.packages("dplyr")
#for data manipulation
install.packages("ggplot2")
#for data visualization
install.packages("class")
#knn
install.packages("caret")
#confusion matrix
install.packages("corrplot")
#correlation plot
library(caTools)
library(dplyr)
library(ggplot2)
library(class)
library(caret)
library(corrplot)
glass <- read.csv("F:\\assignment\\rcodes\\knn\\glass.csv")
View(glass)
standard.features <-scale(glass[,1:9])
#join the standardized data with the target column
data <- cbind(standard.features,glass[10])
#check if there are missing values to impute 
anyNA(data)
head(data)



corrplot(cor(data))


set.seed(101)
sample <-sample.split(data$Type,SplitRatio = 0.7)
train <-subset(data,sample==TRUE)
test <-subset(data,sample==FALSE)


predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
#error in prediction
error <- mean(predicted.type!=test$Type)
#confusion matrix
confusionMatrix(table(predicted.type,test$Type))



predicted.type<- NULL
error.rate <- NULL
for(i in 1:10){
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
}
knn.error <- as.data.frame(cbind(k=1:10,error.type=error.rate))
 ggplot(knn.error,aes(k,error.type))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = 1:10)+
  theme_bw()+
  xlab("valu of K")+
  ylab('error')

 
  predicted.type <- knn(train [1:9],test[1:9],train$Type,k=3)
error <- mean(predicted.type!=test$Type)  
confusionMatrix(table(predicted.type,test$Type))

