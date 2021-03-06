dataset=read.csv('Data.csv')
#missing data concept is used when some data's are not present in given dataset,so we replace that missing data by mean of remaining data's..below code explain this. 
dataset$Age=ifelse(is.na(dataset$Age),ave(dataset$Age,FUN=function(x) mean(x,na.rm=TRUE)),dataset$Age)
dataset$Salary=ifelse(is.na(dataset$Salary),ave(dataset$Salary,FUN=function(x) mean(x,na.rm=TRUE)),dataset$Salary)
#categoriacl data:**it is used to convert string dat to certain numerical constants and replace them with those constants.
dataset$Country=factor(dataset$Country,levels=c('France','Spain','Germany'),labels=c(1,2,3))
dataset$Purchased=factor(dataset$Purchased,levels=c('Yes','No'),labels=c(1,0))
#splitting dataset
#install.packages('caTools')
library(caTools)
set.seeds(123)
split=sample.split(dataset$Purchased,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
#feature scaling
training_set[,2:3]=scale(training_set[,2:3])
test_set=scale(test_set[,2:3])
