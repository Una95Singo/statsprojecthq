
colSums(is.na(as.matrix(data)))[colSums(is.na(as.matrix(data)))>700]
nrow(na.omit(data[,-c(21,22,23,32,33,34,35,42,43,44,45,46)]))

nrow(data[data$Race=="Coloured",])
nrow(data[data$Race=="White"&data$Wealth.Indicator.2==5,])
nrow(data[data$Race=="Non Applicable/Unknown"&data$Wealth.Indicator.2==4,])
nrow(data[data$Race=="Black"&data$Wealth.Indicator.2==2,])
nrow(data[data$Wealth.Indicator.2==2,])

output_summary<-function(perf1,perf2) {
  return(list(mean(c(perf1$AUC,perf2$AUC)),mean(c(perf1$accuracy,perf2$accuracy)),
              mean(c(perf1$specificity,perf2$specificity)),
              mean(c(perf1$falsefails,perf2$falsefails))))
}

####****** MISSING DATA REMOVED ******####
##### SETUP ################################################################

require(readxl)
data<-read_excel("data.xlsx")
data<-data.frame(data)

#### CONVERTING TO FACTORS/NUMERIC ####
data$Year<-as.factor(data$Year)
data$Gender<-as.factor(data$Gender)
data$Race<-as.factor(data$Race)
data$Course<-as.factor(data$Course)
data$Accounting<-as.factor(data$Accounting)
data$AP.Maths<-as.factor(data$AP.Maths)
data$Physics<-as.factor(data$Physics)
data$Life.Sciences<-as.factor(data$Life.Sciences)
data$Home.Language.2<-as.factor(data$Home.Language.)
data$Wealth.Indicator.2<-as.factor(data$Wealth.Indicator.2)
data$Test1<-as.numeric(data$Test1)
data$Final<-as.numeric(data$Final)
data[,18:46]<-as.numeric(as.character(unlist(data[,18:46])))

#### HANDLING MISSING DATA ####

# fill in zeroes
data[,18:46][is.na(data[,18:46])]<-0
data<-na.omit(data[,-c(21,22,23,32,33,34,35,42,43,44,45,46)])
#### HANDLE RESPONSE ####
data$Final<-ifelse(data$Final>=50,1,0)
data$Final<-as.factor(data$Final)

data<-na.omit(data)

#### SUBSETTING AND REMOVING YEARS ####

# subset the different years
data2015<-data.frame(subset(data,data$Year==2015))
data2016<-data.frame(subset(data,data$Year==2016))
data2017<-data.frame(subset(data,data$Year==2017))

# remove year as a predictor
data2015<-data2015[,-4]
data2016<-data2016[,-4]
data2017<-data2017[,-4]
data<-data[,-4]

data_train_years <- rbind(data2015,data2016)

#### MODIFIED CHRONOLOGICAL FUNCTION ####

chronological_log_reg2<-function(dat,newdat1,pi=0.5,prior=FALSE) {
  
  # demographic features only
  f1<-glm(Final~Gender+Race+Home.Language.2+Wealth.Indicator.2,family="binomial",data = dat)
  s1<-stepAIC(f1,direction = "both",trace=0)
  r1<-log_reg_triple(s1,dat = dat,newdat1 = newdat1,pi=pi,prior=prior)
  appearances<-var_imp(s1)
  #test_model(s1,newdat2,newdat2)
  
  # demographic and pre-course cognitive
  f2<-glm(Final~Gender+Race+Home.Language.2+Wealth.Indicator.2+NBT.AL.Score+
            NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
            Accounting+AP.Maths+Physics+Life.Sciences,family="binomial",data = dat)
  s2<-stepAIC(f2,direction = "both",trace=0)
  r2<-log_reg_triple(s2,dat = dat,newdat1 = newdat1,pi=pi,prior=prior)
  appearances<-rowSums(cbind(appearances,var_imp(s2)))
  #test_model(s2,newdat2,newdat2)
  
  N<-8
  AICs<-rep(0,N+2)
  AICs[1]<-s1$aic
  AICs[2]<-s2$aic
  results <- vector("list", N+2)
  results[[1]]<-r1
  results[[2]]<-r2
  
  # during semester data: iteration i is week i+2
  features1<-features[-(9:12)]
  features1[8]<-"A8"

  for(i in 1:N) { 
    new_features<-features1[1:i]
    reg_formula<-formula(paste("Final~Gender+Race+Home.Language.2+Wealth.Indicator.2+
                                NBT.AL.Score+NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
                               Accounting+AP.Maths+Physics+Life.Sciences",paste(new_features,collapse="+"),sep="+"))
    #print(reg_formula)
    f<-glm(reg_formula,family="binomial",data = dat)
    s<-stepAIC(f,direction = "both",trace=0)
    AICs[i+2]<-s$aic
    results[[i+2]]<-log_reg_triple(s,dat = dat,newdat1 = newdat1,pi=pi,prior=prior)    
    appearances<-rowSums(cbind(appearances,var_imp(s)))
    #test_model(s,newdat2,newdat2)
  }
  
  return(list(results=results,AICs=AICs,lastmodel=s,appearances=appearances))
}


#### MODIFIED PERFORMANCE FUNCTION ####
performance_logistic2<-function(models,newdat,model=7) {
  N<-8
  par(mfrow=c(2,2))
  
  # AIC
  plot(y=models$AICs,x=1:(N+2),type='l',main="AIC",ylab="",xlab="Week")
  
  # accuracy, sensitivity, precision
  cv<-rep(0,N+2)
  test<-matrix(nrow=5,ncol=N+2)
  for(i in 1:(N+2)) {
    cv[i]<-models$results[[i]]$cv
    test[1,i]<-models$results[[i]][2][[1]][1] # accuracy
    test[2,i]<-models$results[[i]][4][[1]][1] # sensitivity
    test[3,i]<-models$results[[i]][5][[1]][1] # precision
    test[4,i]<-models$results[[i]]$AUC$auc[1] # AUC
    
    FN<-ifelse(nrow(models$results[[i]]$confusions)>1,models$results[[i]]$confusions[1,2],0)
    FP<-ifelse(nrow(models$results[[i]]$confusions)>1,models$results[[i]]$confusions[2,1],models$results[[i]]$confusions[1])
    TN<-ifelse(nrow(models$results[[i]]$confusions)>1,models$results[[i]]$confusions[1,1],0) 
    TP<-ifelse(nrow(models$results[[i]]$confusions)>1,models$results[[i]]$confusions[2,2],models$results[[i]]$confusions[2])
    
    #test[5,i]<-FP/(FP+TN) # FPR
    test[5,i]<-1-FN/(FN+TP) # sensitivity
  }
  plot(y=cv,x=1:(N+2),col='orange',type='l',ylim=c(0,1),
       main="Predictive performance",ylab="",xlab="Week")
  lines(y=test[1,],x=1:(N+2),col='blue')
  lines(y=test[2,],x=1:(N+2),col='green')
  lines(y=test[3,],x=1:(N+2),col='red')
  lines(y=test[4,],x=1:(N+2),col='brown')
  lines(y=test[5,],x=1:(N+2),col='purple') 
  legend("bottomright",legend=c('CV error','Accuracy','Specificity','NPV','AUC','Sensitivity'),
         col=c('orange','blue','green','red','brown','purple'),lty=rep('solid',5))
  abline(h=0.9,lty=14)
  # deprecated:
  # ROC and youdin
  #chosen_pi<-test_pi(models$lastmodel,newdat = newdat)
  plot.roc(models$results[[model]]$AUC,main="ROC curve")
  y<-models$results[[model]]$AUC$sensitivities+models$results[[model]]$AUC$specificities-1
  x<-models$results[[model]]$AUC$thresholds
  plot(y~x,type='l',main="Youden's statistic",xlab="Threshold",ylab="sensitivity+specificity-1")
  
  par(mfrow=c(1,2))
  # false fails
  falseFails<-sapply(models$results,FUN=function(l) l$confusions[1,2])
  #falseFails[1]<-0
  if(nrow(models$results[[1]]$confusions)==1) falseFails[1]<-0
  plot(y=falseFails,x=1:10,type='b',col='green',main='False fails',xlab='Week',ylab="",ylim=c(0,100))
  
  FP<-sapply(models$results,FUN=function(l) ifelse(nrow(l$confusions)>1,l$confusions[2,1],l$confusions[1]))
  lines(y=FP,x=1:10,col='blue',type='b')
  
  # variable importance
  var_imp_plot(models)
  
  return(list(AUC=mean(test[4,]),accuracy=mean(test[1,]),specificity=mean(test[2,]),sensitivity=mean(test[5,]),falsefails=mean(falseFails),falsePasses=mean(FP)))
}


#### THE MODELSSSS ####
model1.1<-chronological_log_reg2(data2015,data2016)
model1.2<-chronological_log_reg2(data2015,data2017)
model2.1<-chronological_log_reg2(data2016,data2015)
model2.2<-chronological_log_reg2(data2016,data2017)
model3.1<-chronological_log_reg2(data2017,data2015)
model3.2<-chronological_log_reg2(data2017,data2016)

performance_logistic2(model1.1)
performance_logistic2(model1.2)
performance_logistic2(model2.1)
performance_logistic2(model2.2)
performance_logistic2(model3.1)
performance_logistic2(model3.2)


####****** MISSING DATA REPLACED WITH ZEROS ******####
##### SETUP ################################################################

require(readxl)
data<-read_excel("data.xlsx")
data<-data.frame(data)

#### HANDLING MISSING DATA ####

# fill in zeroes
data[,18:46][is.na(data[,18:46])]<-0

#### CONVERTING TO FACTORS/NUMERIC ####
data$Year<-as.factor(data$Year)
data$Gender<-as.factor(data$Gender)
data$Race<-as.factor(data$Race)
data$Course<-as.factor(data$Course)
data$Accounting<-as.factor(data$Accounting)
data$AP.Maths<-as.factor(data$AP.Maths)
data$Physics<-as.factor(data$Physics)
data$Life.Sciences<-as.factor(data$Life.Sciences)
data$Home.Language.2<-as.factor(data$Home.Language.)
data$Wealth.Indicator.2<-as.factor(data$Wealth.Indicator.2)
data$Test1<-as.numeric(data$Test1)
data$Final<-as.numeric(data$Final)
data[,18:46]<-as.numeric(as.character(unlist(data[,18:46])))

#### HANDLE RESPONSE ####
data$Final<-ifelse(data$Final>=50,1,0)
data$Final<-as.factor(data$Final)

data<-na.omit(data)

#### SUBSETTING AND REMOVING YEARS ####

# subset the different years
data2015<-data.frame(subset(data,data$Year==2015))
data2016<-data.frame(subset(data,data$Year==2016))
data2017<-data.frame(subset(data,data$Year==2017))

# remove year as a predictor
data2015<-data2015[,-4]
data2016<-data2016[,-4]
data2017<-data2017[,-4]
data<-data[,-4]

data_train_years <- rbind(data2015,data2016)

#### THE MODELSSSS ####
model4.1<-chronological_log_reg(data2015,data2016)
model4.2<-chronological_log_reg(data2015,data2017)
model5.1<-chronological_log_reg(data2016,data2015)
model5.2<-chronological_log_reg(data2016,data2017)
model6.1<-chronological_log_reg(data2017,data2015)
model6.2<-chronological_log_reg(data2017,data2016)

performance_logistic(model4.1)
performance_logistic(model4.2)
performance_logistic(model5.1)
performance_logistic(model5.2)
performance_logistic(model6.1)
performance_logistic(model6.2)

####****** CONSIDERING A PASS AS 55% ******####
##### SETUP ################################################################

require(readxl)
data<-read_excel("data.xlsx")
data<-data.frame(data)

#### HANDLING MISSING DATA ####

# fill in zeroes
data[,18:46][is.na(data[,18:46])]<-0

#### CONVERTING TO FACTORS/NUMERIC ####
data$Year<-as.factor(data$Year)
data$Gender<-as.factor(data$Gender)
data$Race<-as.factor(data$Race)
data$Course<-as.factor(data$Course)
data$Accounting<-as.factor(data$Accounting)
data$AP.Maths<-as.factor(data$AP.Maths)
data$Physics<-as.factor(data$Physics)
data$Life.Sciences<-as.factor(data$Life.Sciences)
data$Home.Language.2<-as.factor(data$Home.Language.)
data$Wealth.Indicator.2<-as.factor(data$Wealth.Indicator.2)
data$Test1<-as.numeric(data$Test1)
data$Final<-as.numeric(data$Final)
data[,18:46]<-as.numeric(as.character(unlist(data[,18:46])))

#### HANDLE RESPONSE ####
data$Final<-ifelse(data$Final>=55,1,0)
data$Final<-as.factor(data$Final)

data<-na.omit(data)

#### SUBSETTING AND REMOVING YEARS ####

# subset the different years
data2015<-data.frame(subset(data,data$Year==2015))
data2016<-data.frame(subset(data,data$Year==2016))
data2017<-data.frame(subset(data,data$Year==2017))

# remove year as a predictor
data2015<-data2015[,-4]
data2016<-data2016[,-4]
data2017<-data2017[,-4]
data<-data[,-4]

data_train_years <- rbind(data2015,data2016)

#### THE MODELSSSS ####
model7.1<-chronological_log_reg(data2015,data2016)
model7.2<-chronological_log_reg(data2015,data2017)
model8.1<-chronological_log_reg(data2016,data2015)
model8.2<-chronological_log_reg(data2016,data2017)
model9.1<-chronological_log_reg(data2017,data2015)
model9.2<-chronological_log_reg(data2017,data2016)

performance_logistic(model7.1)
performance_logistic(model7.2)
performance_logistic(model8.1)
performance_logistic(model8.2)
performance_logistic(model9.1)
performance_logistic(model9.2)

# LINEAR REGRESSION



####****** LINEAR REGRESSION ******####
##### SETUP ################################################################

require(readxl)
data<-read_excel("data.xlsx")
data<-data.frame(data)

#### HANDLING MISSING DATA ####

# fill in zeroes
data[,18:46][is.na(data[,18:46])]<-0

#### CONVERTING TO FACTORS/NUMERIC ####
data$Year<-as.factor(data$Year)
data$Gender<-as.factor(data$Gender)
data$Race<-as.factor(data$Race)
data$Course<-as.factor(data$Course)
data$Accounting<-as.factor(data$Accounting)
data$AP.Maths<-as.factor(data$AP.Maths)
data$Physics<-as.factor(data$Physics)
data$Life.Sciences<-as.factor(data$Life.Sciences)
data$Home.Language.2<-as.factor(data$Home.Language.)
data$Wealth.Indicator.2<-as.factor(data$Wealth.Indicator.2)
data$Test1<-as.numeric(data$Test1)
data$Final<-as.numeric(data$Final)
data[,18:46]<-as.numeric(as.character(unlist(data[,18:46])))
data<-na.omit(data)

#### SUBSETTING AND REMOVING YEARS ####

# subset the different years
data2015<-data.frame(subset(data,data$Year==2015))
data2016<-data.frame(subset(data,data$Year==2016))
data2017<-data.frame(subset(data,data$Year==2017))

# remove year as a predictor
data2015<-data2015[,-4]
data2016<-data2016[,-4]
data2017<-data2017[,-4]
data<-data[,-4]

data_train_years <- rbind(data2015,data2016)

#### THE MODELSSSS ####

model20.1<-chronological_reg(data2015,newdat1=data2016,newdat2=data2016)
model20.2<-chronological_reg(data2015,newdat1=data2017,newdat2=data2016)
model21.1<-chronological_reg(data2016,newdat1=data2015,newdat2=data2016)
model21.2<-chronological_reg(data2016,newdat1=data2017,newdat2=data2016)
model22.1<-chronological_reg(data2017,newdat1=data2015,newdat2=data2016)
model22.2<-chronological_reg(data2017,newdat1=data2016,newdat2=data2016)

performance_linear(model20.1,data2016)
performance_linear(model20.2,data2017)
performance_linear(model21.1,data2015)
performance_linear(model21.2,data2017)
performance_linear(model22.1,data2015)
performance_linear(model22.2,data2016)



performance_logistic<-function(models,newdat,model=14) {
  N<-12
  par(mfrow=c(1,2))
  
  # AIC
  plot(y=models$AICs,x=1:(N+2),type='l',main="AIC",ylab="",xlab="Time point",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
  box(lwd=1.5)
  
  # accuracy, sensitivity, precision
  cv<-rep(0,N+2)
  test<-matrix(nrow=5,ncol=N+2)
  for(i in 1:(N+2)) {
    cv[i]<-models$results[[i]]$cv
    test[1,i]<-models$results[[i]][2][[1]][1] # accuracy
    test[2,i]<-models$results[[i]][4][[1]][1] # sensitivity
    test[3,i]<-models$results[[i]][5][[1]][1] # precision
    test[4,i]<-models$results[[i]]$AUC$auc[1] # AUC
    
    FN<-ifelse(nrow(models$results[[i]]$confusions)>1,models$results[[i]]$confusions[1,2],0)
    FP<-ifelse(nrow(models$results[[i]]$confusions)>1,models$results[[i]]$confusions[2,1],models$results[[i]]$confusions[1])
    TN<-ifelse(nrow(models$results[[i]]$confusions)>1,models$results[[i]]$confusions[1,1],0) 
    TP<-ifelse(nrow(models$results[[i]]$confusions)>1,models$results[[i]]$confusions[2,2],models$results[[i]]$confusions[2])
    
    #test[5,i]<-FP/(FP+TN) # FPR
    test[5,i]<-1-FN/(FN+TP) # sensitivity
  }
  plot(y=cv,x=1:(N+2),col='orange',type='l',ylim=c(0,1),cex.lab=1.5,cex.axis=1.5,cex.main=1.5,
       main="Predictive Performance",ylab="Proportion",xlab="Time point")
  lines(y=test[1,],x=1:(N+2),col='blue')
  lines(y=test[2,],x=1:(N+2),col='green')
  lines(y=test[3,],x=1:(N+2),col='red')
  lines(y=test[4,],x=1:(N+2),col='brown')
  lines(y=test[5,],x=1:(N+2),col='purple') 
  legend("bottomright",legend=c('CV error','Accuracy','Specificity','NPV','AUC','Sensitivity'),
         col=c('orange','blue','green','red','brown','purple'),lty=rep('solid',5),cex=1.5)
  abline(v=7,lty=14)
  abline(h=0.9,lty=14)
  box(lwd=1.5)
  # deprecated:
  # ROC and youdin
  #plot.roc(models$results[[model]]$AUC,main="ROC curve")
  #y<-models$results[[model]]$AUC$sensitivities+models$results[[model]]$AUC$specificities-1
  #x<-models$results[[model]]$AUC$thresholds
  #plot(y~x,type='l',main="Youden's statistic",xlab="Threshold",ylab="sensitivity+specificity-1")
  
  # variable importance
  #var_imp_plot(models)
  
  return(list(AUC=mean(test[4,]),accuracy=mean(test[1,]),specificity=mean(test[2,]),sensitivity=mean(test[5,]),falsefails=mean(falseFails),falsePasses=mean(FP)))
}




