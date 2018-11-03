library(e1071)
library(readxl)

#### SETUP ################################################################

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

#### SCALING NUMERICS ####
for(i in 1:ncol(data)) {
  if(is.numeric(data[,i])) data[,i]<-scale(data[,i])
}

#### SUBSETTING AND REMOVING YEARS ####

# subset the different years
data2015<-data.frame(subset(data,data$Year==2015))
data2016<-data.frame(subset(data,data$Year==2016))
data2017<-data.frame(subset(data,data$Year==2017))

# remove year as a predictor
data2015<-data2015[,-4]
data2016<-data2016[,-4]
data2017<-data2017[,-4]

#### ONE HOT ENCODING ####
xdata2015<-data.frame(model.matrix(Final~.,data2015)[,-1])
xdata2016<-data.frame(model.matrix(Final~.,data2016)[,-1])
xdata2017<-data.frame(model.matrix(Final~.,data2017)[,-1])

data2015<-data.frame(cbind(xdata2015,data2015$Final))
names(data2015)[ncol(data2015)]<-"Final"
data2016<-data.frame(cbind(xdata2016,data2016$Final))
names(data2016)[ncol(data2016)]<-"Final"
data2017<-data.frame(cbind(xdata2017,data2017$Final))
names(data2017)[ncol(data2017)]<-"Final"

#### USEFUL FUNCTIONS ####

test_model<-function(model,newdat1) {
  
  # test on dataset
  pred1  <- predict(model, newdata=newdat1,probability = TRUE)
  confusion1<-table(pred=pred1,true=newdat1[,ncol(newdat1)]) 
  accuracy1<-sum(diag(confusion1))/sum(confusion1)
  sensitivity1<-confusion1[1,1]/as.numeric(colSums(confusion1)[1])
  precision1<-confusion1[1,1]/as.numeric(rowSums(confusion1)[1])
  
  pi1<-attr(pred1,"probabilities")[,1]
  auc<-roc(newdat1$Final,pi1)

  return(list(confusions=confusion1,accuracies=accuracy1,sensitivities=sensitivity1,precisions=precision1,AUC=auc))
}

chronological_svm<-function(tunedmodel,dat,year1="",newdat1,wF=1,wP=1) {
  
  # demographic features only
  m1<-svm(Final~GenderM+RaceChinese+RaceColoured+RaceIndian+RaceNon.Applicable.Unknown+
            RaceWhite+Home.Language.2Other+Wealth.Indicator.23+Wealth.Indicator.24+
            Wealth.Indicator.25+Wealth.Indicator.26,scale=FALSE,data=dat,
          gamma=tunedmodel$best.parameters[1],cost=tunedmodel$best.parameters[2],probability=TRUE,
          class.weights=c('0'=wF,'1'=wP),cross=10)
  r1<-test_model(m1,newdat1)
  
  # demographic and pre-course cognitive
  m2<-svm(Final~GenderM+RaceChinese+RaceColoured+RaceIndian+RaceNon.Applicable.Unknown+
            RaceWhite+Home.Language.2Other+Wealth.Indicator.23+Wealth.Indicator.24+
            Wealth.Indicator.25+Wealth.Indicator.26+NBT.AL.Score+
            NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
            Accounting1+AP.Maths1+Physics1+Life.Sciences1,scale=FALSE,data=dat,
          gamma=tunedmodel$best.parameters[1],cost=tunedmodel$best.parameters[2],probability=TRUE,
          class.weights=c('0'=wF,'1'=wP),cross=10)
  r2<-test_model(m2,newdat1)
  
  N<-12
  results <- vector("list", N+2)
  results[[1]]<-r1
  results[[2]]<-r2
  
  # during semester data: iteration i is week i+2
  for(i in 1:N) { 
    new_features<-features[1:i]
    reg_formula<-formula(paste("Final~GenderM+RaceChinese+RaceColoured+RaceIndian+RaceNon.Applicable.Unknown+
                               RaceWhite+Home.Language.2Other+Wealth.Indicator.23+Wealth.Indicator.24+
                               Wealth.Indicator.25+Wealth.Indicator.26+NBT.AL.Score+
                               NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
                               Accounting1+AP.Maths1+Physics1+Life.Sciences1",paste(new_features,collapse="+"),sep="+"))
    #print(reg_formula)
    m<-svm(reg_formula,scale=FALSE,data=dat,
           gamma=tunedmodel$best.parameters[1],cost=tunedmodel$best.parameters[2],probability=TRUE,
           class.weights=c('0'=wF,'1'=wP),cross=10)
    results[[i+2]]<-test_model(m,newdat1)
    
  }
  
  return(results)
}

test_pi<-function(model,newdat,main="") {
  
  pis<-seq(0,1,0.01)
  accuracy<-c()
  sensitivity<-c()
  precision<-c()
  specificity<-c()
  youdin<-c()
  i<-1
  
  for(pi in pis) {
    probs<-predict.glm(model,newdata = newdat,type="response")
    newdatpred<-cbind(newdat,probs)
    newdatpred<-newdatpred[order(newdatpred$probs),]
    pred<-ifelse(newdatpred$probs<pi,0,1)
    newdatpred<-cbind(newdatpred,pred)
    confusion<-table(pred=pred,true=newdat$Final)
    accuracy<-c(accuracy,sum(diag(confusion))/sum(confusion))
    specificity<-c(specificity,confusion[1,1]/as.numeric(colSums(confusion)[1]))
    precision<-c(precision,confusion[1,1]/as.numeric(rowSums(confusion)[1]))
    if(nrow(confusion)==2) 
      sensitivity<-c(sensitivity,confusion[2,2]/as.numeric(colSums(confusion)[2]))
    else sensitivity<-c(sensitivity,0)
    youdin<-c(youdin,specificity[i]+sensitivity[i]-1)
    i<-i+1
  }
  
  # the usual metrics
  plot(accuracy~pis,type='l',ylim=c(0,1),col='blue',main=main)
  lines(sensitivity~pis,col='green')
  lines(precision~pis,col='red')
  lines(specificity~pis,col='orange')
  legend('center',legend = c('accuracy','sensitivity','precision','specificity'),
         col=c('blue','green','red','orange'),lty=rep('solid',4))
  
  # ROC curve
  one_minus_specificity<-1-specificity
  plot(sensitivity~one_minus_specificity,type='l',main=main)
  
  # Youdin's test
  plot(youdin~pis,type='l',main=main)
  abline(h=0,col='red')
}

test_pi_svm<-function(model,newdat,main="") {
  
  pis<-seq(0,1,0.01)
  accuracy<-c()
  sensitivity<-c()
  precision<-c()
  specificity<-c()
  youdin<-c()
  i<-1
  
  for(pi in pis) {
    probs<-attr(predict(model,newdat,probability = TRUE),"probabilities")[,1]
    newdatpred<-cbind(newdat,probs)
    newdatpred<-newdatpred[order(newdatpred$probs),]
    pred<-ifelse(newdatpred$probs<pi,0,1)
    newdatpred<-cbind(newdatpred,pred)
    confusion<-table(pred=pred,true=newdat$Final)
    accuracy<-c(accuracy,sum(diag(confusion))/sum(confusion))
    specificity<-c(specificity,confusion[1,1]/as.numeric(colSums(confusion)[1]))
    precision<-c(precision,confusion[1,1]/as.numeric(rowSums(confusion)[1]))
    if(nrow(confusion)==2) 
      sensitivity<-c(sensitivity,confusion[2,2]/as.numeric(colSums(confusion)[2]))
    else sensitivity<-c(sensitivity,0)
    youdin<-c(youdin,specificity[i]+sensitivity[i]-1)
    i<-i+1
  }
  
  # the usual metrics
  #  plot(accuracy~pis,type='l',ylim=c(0,1),col='blue',main=main)
  #  lines(sensitivity~pis,col='green')
  #  lines(precision~pis,col='red')
  #  lines(specificity~pis,col='orange')
  #  legend('center',legend = c('accuracy','sensitivity','precision','specificity'),
  #         col=c('blue','green','red','orange'),lty=rep('solid',4))
  
  # ROC curve
  #one_minus_specificity<-1-specificity
  #plot(sensitivity~one_minus_specificity,type='l',main="ROC curve")
  roc()
  
  # Youdin's test
  plot(youdin~pis,type='l',main="Youdin's statistic")
  abline(h=0,col='red')
  
  return(pis[which.max(youdin)])
}

performance_svm<-function(models,newdat,model=14,weightings=12) {
  par(mar=c(5,6,4,2)+0.1,mgp=c(4,1,0))
  par(mfrow=c(1,2))
  par(las=1)
  # accuracy, sensitivity, precision
  N<-weightings
  test<-matrix(nrow=5,ncol=N+2)
  for(i in 1:(N+2)) {
    test[1,i]<-models[[i]]$accuracies # accuracy
    test[2,i]<-models[[i]]$sensitivities # sensitivity
    test[3,i]<-models[[i]]$confusions[2,2]/as.numeric(rowSums(models[[i]]$confusions)[2]) # precision
    test[4,i]<-models[[i]]$AUC$auc[1] # AUC
    
    FN<-models[[i]]$confusions[1,2]
    FP<-models[[i]]$confusions[2,1]
    TN<-models[[i]]$confusions[1,1]
    TP<-models[[i]]$confusions[2,2]
    
    #test[5,i]<-FP/(FP+TN) # FPR
    test[5,i]<-1-FN/(FN+TP) # sensitivity
  }
  
  
  # false fails
  falseFails<-sapply(models,FUN=function(l) l$confusions[1,2])
  #falseFails[1]<-0
  plot(y=falseFails/378,x=1:(N+2),type='b',col='green',cex.main=2,cex.lab=2,cex.axis=2,
       main='False Fail and Pass Rates',xlab='Weighting combination',ylab="Proportion of test set",ylim=c(0,0.5))
  falsePasses<-sapply(models,FUN=function(l) l$confusions[2,1])
  lines(y=falsePasses/378,x=1:(N+2),col='blue',type='b')
  abline(v=4,lty=14)
  legend("topright",legend=c("False fails","False passes"),col=c('Green','Blue'),
         lty=c('solid','solid'),bty="n",cex=1.5)
  
  
  plot(y=test[1,],x=1:(N+2),col='blue',type='l',ylim=c(0,1),cex.main=2,cex.lab=2,cex.axis=2,
        main="Predictive Performance",ylab="Proportion",xlab="Weighting combination")
  lines(y=test[2,],x=1:(N+2),col='green')
  lines(y=test[3,],x=1:(N+2),col='red')
  lines(y=test[4,],x=1:(N+2),col='brown')
  lines(y=test[5,],x=1:(N+2),col='purple')
  #lines(y=test[6,],x=1:(N+2),col='pink')
  legend("bottomright",legend=c('Accuracy','Specificity','NPV','AUC','Sensitivity'),
         col=c('blue','green','red','brown','purple'),lty=rep('solid',4),bty="n",cex=1.5)
  abline(h=0.9,lty=14)
  abline(v=4,lty=14)
  
  # ROC and youdin
  #plot.roc(models[[model]]$AUC,main="ROC curve")
  #y<-models[[model]]$AUC$sensitivities+models[[model]]$AUC$specificities-1
  #x<-models[[model]]$AUC$thresholds
  #plot(y~x,type='l',main="Youden's statistic",ylab="sensitivity+specificity-1",xlab="Threshold")


  #return(chosen_pi)
  return(list(AUC=mean(test[4,]),accuracy=mean(test[1,]),specificity=mean(test[2,]),
              sensitivity=mean(test[5,]),falsefails=mean(falseFails),falsePasses=mean(falsePasses)))
}

# modified for model 7
test_weightings<-function(tuned,dat,newdat,weightsPass,weightsFail) {
  results<-vector("list",length(weightsPass))
  for(i in 1:length(weightsPass)) {
    model<-svm(Final~GenderM+RaceChinese+RaceColoured+RaceIndian+RaceNon.Applicable.Unknown+
                 RaceWhite+Home.Language.2Other+Wealth.Indicator.23+Wealth.Indicator.24+
                 Wealth.Indicator.25+Wealth.Indicator.26+NBT.AL.Score+
                 NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
                 Accounting1+AP.Maths1+Physics1+Life.Sciences1+A1+T2+A2+T3+B1+A3+T4+A4+T5+Test1+
                 B2+A5,data = dat,gamma=tuned$best.parameters[1],scale = FALSE,
               cost=tuned$best.parameters[2],class.weights=c('0'=weightsFail[i],'1'=weightsPass[i]),
               probability=TRUE,cross=10)
    results[[i]]<-test_model(model,newdat)
  }
  results
}

#### REMAINING DATA PREPARATION ####

data_train_years<-rbind(data2015,data2016)

xdata<-data.frame(model.matrix(Final~.,data)[,-1])
data<-data.frame(cbind(xdata,data$Final))
names(data)[ncol(data)]<-"Final"
data<-data[,-c(4,5)]

# training and test set
n<-floor(0.8*nrow(data))# justify split
set.seed(46)
training_index<-sample(seq_len(nrow(data)),size=n)
data_train<-data.frame(data[training_index,])
data_test<-data.frame(data[-training_index,])

# rebalancing 
M<-nrow(data[data$Final==0,])
set.seed(1)
data.F<-data[data$Final==0,]
data.P<-data[data$Final==1,]
passIndex<-sample(seq_len(nrow(data.P)),size=M)
data.P<-data.P[passIndex,]
data.downsample<-rbind(data.F,data.P)

n<-floor(0.8*nrow(data.downsample))# justify split
training_index<-sample(seq_len(nrow(data.downsample)),size=n)
data_train_balanced<-data.frame(data.downsample[training_index,])
data_test_balanced<-data.frame(data.downsample[-training_index,])

# other rebalancing
M<-nrow(data_train[data_train$Final==0,])
set.seed(1)
data.F<-data_train[data_train$Final==0,]
data.P<-data_train[data_train$Final==1,]
passIndex<-sample(seq_len(nrow(data.P)),size=M)
data.P<-data.P[passIndex,]
data_train_balanced2<-rbind(data.F,data.P)

#### PARAMETER TUNING ####

set.seed(1)

timeYears<-system.time(
  tunedYears <- tune.svm(Final~., data = data_train_years, cost=c(1,10,100,500,1000),
                        gamma=exp(seq(-10,0,1)),scale=FALSE, tunecontrol=tune.control(cross=10),
                        probability=TRUE)
)

timeFull<-system.time(
  tunedFull <- tune.svm(Final~., data = data_train, cost=c(1,10,100,500,1000),
                        gamma=exp(seq(-10,0,1)),scale=FALSE, tunecontrol=tune.control(cross=10),
                        probability=TRUE)
)
timeBalanced<-system.time(
  tunedBalanced <- tune.svm(Final~., data = data_train_balanced, cost=c(1,10,100,500,1000),
                            gamma=exp(seq(-10,0,1)),scale=FALSE, tunecontrol=tune.control(cross=10),
                            probability=TRUE)
)

timeBalanced2<-system.time(
  tunedBalanced2 <- tune.svm(Final~., data = data_train_balanced2, cost=c(1,10,100,500,1000),
                            gamma=exp(seq(-10,0,1)),scale=FALSE, tunecontrol=tune.control(cross=10),
                            probability=TRUE)
)

#### CHRONOLOGICAL RESULTS, NO CLASS WEIGHTING ####

svmYears<-chronological_svm(tunedYears,dat=data_train_years,newdat1 = data2017)
chronological_svm2<-function(tunedmodel,dat,year1="",newdat1,wF=1,wP=1) {
  
  # demographic features only
  m1<-svm(Final~GenderM+RaceChinese+RaceColoured+RaceIndian+RaceNon.Applicable.Unknown+
            RaceWhite+Home.Language.2Other+Wealth.Indicator.23+Wealth.Indicator.24+
            Wealth.Indicator.25+Wealth.Indicator.26,scale=FALSE,data=dat,
          gamma=tunedmodel$performances[7,1],cost=1,probability=TRUE,
          class.weights=c('0'=wF,'1'=wP),cross=10)
  r1<-test_model(m1,newdat1)
  
  # demographic and pre-course cognitive
  m2<-svm(Final~GenderM+RaceChinese+RaceColoured+RaceIndian+RaceNon.Applicable.Unknown+
            RaceWhite+Home.Language.2Other+Wealth.Indicator.23+Wealth.Indicator.24+
            Wealth.Indicator.25+Wealth.Indicator.26+NBT.AL.Score+
            NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
            Accounting1+AP.Maths1+Physics1+Life.Sciences1,scale=FALSE,data=dat,
          gamma=tunedmodel$performances[7,1],cost=1,probability=TRUE,
          class.weights=c('0'=wF,'1'=wP),cross=10)
  r2<-test_model(m2,newdat1)
  
  N<-12
  results <- vector("list", N+2)
  results[[1]]<-r1
  results[[2]]<-r2
  
  # during semester data: iteration i is week i+2
  for(i in 1:N) { 
    new_features<-features[1:i]
    reg_formula<-formula(paste("Final~GenderM+RaceChinese+RaceColoured+RaceIndian+RaceNon.Applicable.Unknown+
                               RaceWhite+Home.Language.2Other+Wealth.Indicator.23+Wealth.Indicator.24+
                               Wealth.Indicator.25+Wealth.Indicator.26+NBT.AL.Score+
                               NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
                               Accounting1+AP.Maths1+Physics1+Life.Sciences1",paste(new_features,collapse="+"),sep="+"))
    #print(reg_formula)
    m<-svm(reg_formula,scale=FALSE,data=dat,
           gamma=tunedmodel$performances[7,1],cost=1,probability=TRUE,
           class.weights=c('0'=wF,'1'=wP),cross=10)
    results[[i+2]]<-test_model(m,newdat1)
    
  }
  
  return(results)
}
svmYears2<-chronological_svm2(tunedYears,dat=data_train_years,newdat1 = data2017)
svmFull<-chronological_svm(tunedFull,dat=data_train,newdat1 = data_test)
svmBalanced<-chronological_svm(tunedBalanced,dat=data_train_balanced,newdat1 = data_test_balanced)
svmBalanced2<-chronological_svm(tunedBalanced2,dat=data_train_balanced2,newdat1 = data_test)

performance_svm(svmYears,data2017,model=7)
performance_svm(svmYears2,data2017,model=7)
performance_svm(svmFull,data_test,model = 7)
performance_svm(svmBalanced,data_test_balanced)
performance_svm(svmBalanced2,data_test,model=7)

#### FINDING OPTIMAL CLASS WEIGHTING ####

pass_weights<-seq(1,10,1)
fail_weights<-seq(10,1,-1)

wYears<-test_weightings(tunedYears,data_train_years,data2017,pass_weights,fail_weights)
wFull<-test_weightings(tunedFull,data_train,data_test,pass_weights,fail_weights)
wBalanced<-test_weightings(tunedBalanced,data_train_balanced,data_test_balanced,pass_weights,fail_weights)
wBalanced2<-test_weightings(tunedBalanced2,data_train_balanced2,data_test,pass_weights,fail_weights)

performance_svm(wYears,data2017,1,8) # model is 10, N=8 => 10 'weeks'
performance_svm(wFull,data_test,4,8) # optimal weighting is pass=4,fail=7
performance_svm(wBalanced,data_test_balanced,1,8)
performance_svm(wBalanced2,data_test,1,8)

#### MODEL 7, CLASS WEIGHTING CONFIGURATION 4 ####

gamma<-0.01831564
cost<-1

model<-svm(Final~GenderM+RaceChinese+RaceColoured+RaceIndian+RaceNon.Applicable.Unknown+
             RaceWhite+Home.Language.2Other+Wealth.Indicator.23+Wealth.Indicator.24+
             Wealth.Indicator.25+Wealth.Indicator.26+NBT.AL.Score+
             NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
             Accounting1+AP.Maths1+Physics1+Life.Sciences1+A1+T2+A2+T3+B1+A3+T4+A4+T5+Test1+
             B2+A5,data = data_train,gamma=gamma,scale = FALSE,
           cost=cost,class.weights=c('0'=fail_weights[4],'1'=pass_weights[4]),
           probability=TRUE,cross=10)

pi<-predict(model,newdata = data_train,probability = TRUE)
pi<-attr(pi,"probabilities")[,1]

auc<-roc(data_train$Final,pi)
y<-auc$sensitivities+auc$specificities-1
x<-auc$thresholds
bestpi<-x[which.max(y)]

pi<-predict(model,newdata = data_test,probability = TRUE)
pi<-attr(pi,"probabilities")[,1]

# threshold=0.5
confusion1<-wFull[[4]]$confusions
auc1<-wFull[[4]]$AUC$auc[1]
accuracy1<-wFull[[4]]$accuracies
specificity1<-wFull[[4]]$sensitivities
NPV1<-wFull[[4]]$precisions
falseFails1<-wFull[[4]]$confusions[1,2]

# threshold=0.77
pred2<-ifelse(pi>=bestpi,1,0)
confusion2<-table(pred=pred2,true=data_test$Final)
auc2<-roc(data_test$Final,pred2,plot=FALSE)$auc[1]
accuracy2<-sum(diag(confusion2))/sum(confusion2)
specificity2<-confusion2[1,1]/as.numeric(colSums(confusion2)[1])
NPV2<-confusion2[2,2]/as.numeric(rowSums(confusion2)[2])
falseFails2<-confusion2[1,2]

# threshold=halfway between
newpi<-(bestpi+0.5)/2
pred3<-ifelse(pi>=newpi,1,0)
confusion3<-table(pred=pred3,true=data_test$Final)
auc3<-roc(data_test$Final,pred3,plot=FALSE)$auc[1]
accuracy3<-sum(diag(confusion3))/sum(confusion3)
specificity3<-confusion3[1,1]/as.numeric(colSums(confusion3)[1])
NPV3<-confusion3[2,2]/as.numeric(rowSums(confusion3)[2])
falseFails3<-confusion3[1,2]

table<-rbind(c(0.5,auc1,accuracy1,specificity1,NPV1,falseFails1),
             c(round(newpi,2),auc2,accuracy3,specificity3,NPV3,falseFails3),
             c(round(bestpi,2),auc3,accuracy2,specificity2,NPV2,falseFails2))
colnames(table)<-c('Threshold','AUC',"Accuracy",'Specificity','NPV','False fails')

#### SHOWING TUNING ####

display_tuning<-function(tuned,ylim1=0.1,ylim2=0.2,xlim=0.15,main="") {
  par(mar=c(5,6,4,2)+0.1,mgp=c(4,1,0))
  costs<-c(1,10,100,500,1000)
  colours<-1:length(costs)
  # zoomed in
  for(i in 1:length(costs)) {
    sub<-subset(tuned$performances,tuned$performances$cost==costs[i])
    if(i!=1) lines(sub$error~sub$gamma,col=colours[i])
    else plot(sub$error~sub$gamma,type='l',col=colours[i],xlim=c(0,xlim),
              cex.lab=2,cex.axis=1.5,cex.main=2,
              ylim=c(ylim1,ylim2),main=main,xlab="Gamma",
              ylab="Misclassification error")
  }
  abline(v=tuned$best.parameters[1],lty=14,col='grey')
  abline(v=1.831564e-02,lty=14,col='grey')    
  abline(h=tuned$best.performance,lty=14,col='grey')
  legend("bottomright",legend=costs,col=colours,
         lty=rep('solid',length(costs),title="Cost"),cex=1.5,bty="n")
}

par(mfrow=c(2,2))
display_tuning(tunedBalanced,ylim1=0.2,ylim2=0.3,main="Model 24")
display_tuning(tunedBalanced2,ylim1=0.2,ylim2=0.3,main="Model 25")
display_tuning(tunedFull,main="Model 26")
display_tuning(tunedYears,main="Model 27/28")


