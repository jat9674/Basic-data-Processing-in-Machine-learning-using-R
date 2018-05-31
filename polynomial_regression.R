dataset=read.csv('Position_Salaries.csv')
dataset=dataset[2:3]
#fitting linear regression model
linreg=lm(formula=Salary~Level,data=dataset)
summary(linreg)
#fitting polynomial regression model
dataset$Level2=dataset$Level^2
dataset$Level3=dataset$Level^3
dataset$Level4=dataset$Level^4
polyreg=lm(formula=Salary~.,data=dataset)
summary(polyreg)
#visualising linear regression graph
library(ggplot2)
ggplot()+
  geom_point(aes(x = dataset$Level,  y =dataset$Salary),colour='red')+
  geom_line(aes( x = dataset$Level, y = predict(linreg, newdata = dataset)),colour='blue')+
  ggtitle('Truth vs BLuff')+
  xlab('Level')+
  ylab('salary')
#visualising polynomial linear regression graph
ggplot()+
  geom_point(aes(x = dataset$Level,  y =dataset$Salary),colour='red')+
  geom_line(aes( x = dataset$Level, y = predict(polyreg, newdata = dataset)),colour='blue')+
  ggtitle('Truth vs BLuff')+
  xlab('Level')+
  ylab('salary')
y_pred=predict(linreg,data.frame(Level=6.5))
y_pred=predict(polyreg,data.frame(Level=6.5,Level2=6.5^2,Level3=6.5^3,Level4=6.5^4))