dataset=read.csv('Position_Salaries.csv')
dataset=dataset[2:3]
#install.packages('rpart')
#fitting decision tree regression
library(rpart)
regressor=rpart(formula=Salary ~ .,data=dataset,control=rpart.control(minsplit=1))
y_pred=predict(regressor,data.frame(Level=6.5))
#visualising decision tree graph
library(ggplot2)
xgrid=seq(min(dataset$Level),max(dataset$Level),0.01)
ggplot()+
  geom_point(aes(x = dataset$Level,  y =dataset$Salary),colour='red')+
  geom_line(aes( x = xgrid, y = predict(regressor, newdata = data.frame(Level=xgrid))),colour='blue')+
  ggtitle('Truth vs BLuff(DecisioN TREE)')+
  xlab('Level')+
  ylab('salary')