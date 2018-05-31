dataset=read.csv('50_Startups.csv')
#categorical data
dataset$State=factor(dataset$State,levels=c('New York','Florida','California'),labels=c(1,2,3))

split=sample.split(dataset$Profit,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)

test_set=subset(dataset,split==FALSE)
regressor=lm(formula=Profit~ .,data=training_set)
y_pred=predict(regressor,newdata=test_set)
ggplot()+
  geom_point(aes(x=training_set$R.D.Spend,y=training_set$Profit),colour='green')+
  geom_line(aes(x=training_set$R.D.Spend,y=predict(regressor,n_dat=training_set)),colour='blue')+
  ggtitle('PROFIT VS R.D')+
  xlab('R.D')+
  ylab('Profit')
#test_set
ggplot()+
  geom_point(aes(x=test_set$R.D.Spend,y=test_set$Profit),colour='green')+
  geom_line(aes(x=training_set$R.D.Spend,y=predict(regressor,n_dat=training_set)),colour='blue')+
  ggtitle('PROFIT VS R.D')+
  xlab('R.D')+
  ylab('Profit')
 #Backward elimination method
regressor=lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,data=dataset)
summary(regressor)
#remove state
regressor=lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend ,data=dataset)
summary(regressor)
#remove administration
regressor=lm(formula = Profit~ R.D.Spend  + Marketing.Spend ,data=dataset)
summary(regressor)