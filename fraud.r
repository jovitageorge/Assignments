install.packages("C50")
install.packages("tree",)
install.packages("caret",)
install.packages("gmodels",)
install.packages("party",)
install.packages("knitr",)
install.packages("png",)
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
FraudCheck <-read.csv("F:\\assignment\\rcodes\\Decision tree\\Fraud_check.csv")
View(FraudCheck)
#splitting data into training and testing
#splitting the data based on sales
hist(FraudCheck$Taxable.Income)
 Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)
#CD <-CompanyData[,2:12]
#view (CD)
FC_train <- FC[1:300,]
#view (CD_train)
FC_test <- FC[301:600,]
#view (CD_test)

png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
plot(opall_tree)
#from the above tree, it looks like the data has 20% risky patients and 80% of good patients

#using the training data 
png(file = "decision_tree.png")

op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)
plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)
mean(pred_test_df==FC_test$Risky_Good)#Accuracy =82%
CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)
