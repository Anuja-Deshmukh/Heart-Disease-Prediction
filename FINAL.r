##MACHINE LEARNING ASSIGNMENT  CE  

##Name:Geeta Deshmukh       Roll No:11007 

## Prajkta Bendre           Roll No:11011 



install.packages('tidyr')
library(tidyr)

install.packages("corrplot")
library(corrplot)

install.packages('ggplot2')
library(ggplot2)

heartdis=read.csv(file.choose())
heartdis
dim(heartdis)

library(caTools)

set.seed(99)
split = sample.split(heartdis$target, SplitRatio = 0.7)
train = subset(heartdis, split==TRUE)
head(train)
dim(train)
test = subset(heartdis, split==FALSE)
head(test)
dim(test)


#Logistic Regression Model
LogReg1 = glm(target~., data=train, family='binomial')
LogReg1

summary(LogReg1)

#Checking Multicollinearity

cor(train)

abs(cor(train))>0.7

ggplot(melted_cormat, aes(x = Var1, y=Var2, fill=as.numeric(value))) + geom_tile() +
  geom_text(aes(Var1, Var2, label=as.numeric(value)),color='black',size=2)+
  scale_color_gradient(low='blue',high='red') +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


#No columns are highly correlated with each other. Let us view the above matrix as a heatmap.

library(reshape2)
melted_cormat = melt(abs(cor(train))>0.7)
head(melted_cormat)



#first we check correlation of target with all columns
LogReg2 = glm(target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,
              data=train, family='binomial')
summary(LogReg2)

#here we will remove columns that are not correlated

lo=step(LogReg1,heartdis)
lo


#Predictions on Training Set
predictTrain = predict(lo, type='response')

#Confusion matrix using threshold of 0.5
table(train$target, predictTrain>0.5)

#Accuracy on training set
(80+104)/nrow(train)


#Predictions on Test set
predictTest = predict(lo, newdata=test, type='response')

#Confusion matrix using threshold of 0.5
table(test$target, predictTest>0.5)


#Accuracy
(28+45)/(nrow(test))



