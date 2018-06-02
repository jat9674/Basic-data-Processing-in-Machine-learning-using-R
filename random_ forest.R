dataset=read.csv('Position_Salaries.csv')
dataset=dataset[2:3]
 
#installing random forest package
#install.packages('randomForest')
#fitting random forest regressor
library(randomForest)
set.seed(1234)
regressor=randomForest(x=dataset[1],y=dataset$Salary,ntree=500)
y_pred=predict(regressor,data.frame(Level=6.5))
#VISULALISING RANDOM FOREST TREE
library(ggplot2)
xgrid=seq(min(dataset$Level),max(dataset$Level),0.01)
ggplot()+
  geom_point(aes(x = dataset$Level,  y =dataset$Salary),colour='red')+
  geom_line(aes( x = xgrid, y = predict(regressor, newdata = data.frame(Level=xgrid))),colour='blue')+
  ggtitle('Truth vs BLuff(RANDOM FOREST)')+
  xlab('Level')+
  ylab('salary')