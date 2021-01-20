#Import the data
Data=read.csv(file.choose(), header = T)
Data
summary(Data)
attach(Data)
str(Data)


#Load the library 
library(ggplot2)
library(plotly)

###Plot
p=ggplot(Data,aes(Hours,Scores))+
geom_point(col='blue')
p
ggplotly(p)



####splitting the data set
data.sample=sample(nrow(Data),0.8*nrow(Data))
train_set=Data[data.sample,]
train_set
test_set=Data[-data.sample,]
test_set


X_train=train_set$Hours
X_test=test_set$Hours
y_train=train_set$Scores
y_test=test_set$Scores


model=lm(y_train~X_train,data=Data)
summary(model)



###regression line on model
q=ggplot(Data,aes(Hours,Scores))+
  geom_point(col='blue')+geom_smooth(method='lm',se=FALSE,col='red')
q

###Predication for 9.25 Hours
coef=model$coefficients
coef
pred=coef[1]+coef[2]*9.25
pred


###Predict Score
model1=lm(y_test~X_test)
predict_Scores=predict(model1,data.frame(Hourse=X_test))
predict_Scores
table=data.frame(y_test,predict_Scores)
table


#Mean Absolute Error
library(MLmetrics)
MAE(y_test,predict_Scores)

