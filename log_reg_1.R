library(readxl)
library(MASS)
library(boot)
library(lmvar)
library(MLmetrics)

##### SETUP ################################################################


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

#### CORRELATIONS WITH RESPONSE ####

data_numeric<-data[,c(1:3,9,10,17:ncol(data))]
corrs<-cor(x=data_numeric,y=data_numeric$Final,use="complete.obs")
corr_matrix<-cor(x=data_numeric,use="complete.obs")

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

#### SPLITTING TEST AND TRAINING ON FULL DATASET ####

n<-floor(0.8*nrow(data))# justify split
set.seed(46)
training_index<-sample(seq_len(nrow(data)),size=n)
data_train<-data.frame(data[training_index,])
data_test<-data.frame(data[-training_index,])

#### GENERIC FUNCTIONS FOR LOGISTIC REGRESSION ####

log_reg<-function(features,dat,newdat) { # features is a string
  predictors<-paste(features)
  reg_formula<-formula(paste("Final~",predictors))
  fit<-glm(reg_formula,family="binomial",data=dat)
  pi<-predict.glm(fit,newdata=newdat,type="response")
  prediction<-rep(0,length(newdat$Final))
  for(i in 1:length(pi)) {
    if(pi[i]>0.5)
      prediction[i]<-1
  }
  confusion<-table(pred=prediction,true=newdat$Final)
  misclass<-1-sum(diag(confusion))/sum(confusion)
  return(list(fit=fit,pi=pi,prediction=prediction,confusion=confusion,misclass=misclass,pi=pi))
}

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

log_reg_main<-function(fit,dat,newdat1,pi=0.5,prior=FALSE) { 
  
  # CV error
  cv<-cv.glm(dat,fit,K=10,cost=function(y,pi=0) mean(abs(y-pi)>0.5))$delta[1]
  
  # first prediction
  if(!prior) pi1<-predict.glm(fit,newdata=newdat1,type="response")
  else pi1<-prior_correction(fit,dat = newdat1)
  prediction1<-rep(0,length(newdat1$Final))
  for(i in 1:length(pi1)) {
    if(pi1[i]>pi)
      prediction1[i]<-1
  }
  confusion1<-table(pred=prediction1,true=newdat1$Final)

  if(nrow(confusion1)>1) { 
    accuracy1<-sum(diag(confusion1))/sum(confusion1)
    sensitivity1<-confusion1[1,1]/as.numeric(colSums(confusion1)[1])
    precision1<-confusion1[2,2]/as.numeric(rowSums(confusion1)[2])
  }
  else {
    accuracy1<-confusion1[2]/sum(confusion1)
    sensitivity1<-0
    precision1<-accuracy1
  }
  auc<-roc(newdat1$Final,pi1)
  #optimal_pi<-auc$thresholds[which.max(auc$sensitivities+auc$specificities-1)]
  return(list(cv=cv,accuracies=accuracy1,confusions=confusion1,sensitivities=sensitivity1,
              precisions=precision1,AUC=auc))
}

#### EXPLORATORY LR ####

# interactions

EDA.fit.1<-log_reg("Race+Home.Language.2+Race*Home.Language.2",dat=data,newdat=data_test)
EDA.fit.2<-log_reg("Race+Wealth.Indicator.2+Race*Wealth.Indicator.2",dat=data,newdat=data_test)
EDA.fit.3<-log_reg("Maths..High.school.+Wealth.Indicator.2+Maths..High.school.*Wealth.Indicator.2",
                   dat=data,newdat=data_test)
EDA.fit.4<-log_reg("Maths..High.school.+Race+Maths..High.school.*Race",
                   dat=data,newdat=data_test) # some significant interaction, still probably not meaningful
EDA.fit.5<-log_reg("NBT.Math.Score+Wealth.Indicator.2+NBT.Math.Score*Wealth.Indicator.2",
                   dat=data,newdat=data_test)
EDA.fit.6<-log_reg("NBT.Math.Score+Race+NBT.Math.Score*Race",
                   dat=data,newdat=data_test)

#### CHRONOLOGICAL MODEL-BUILDING ####



# 2015, 2016, 2017
# build 14 models each time
# cross validate and test on the other two years 

# Store the features to be used in chronological model building
features<-c("A1","T2+A2","T3+B1+A3","T4+A4",
            "T5+Test1+B2+A5","T6+A6","T7+B3+A7","T8+A8",
            "T9+B4+A9","T10+A10","T11+B5+A11","T12+B6+A12")

# Function controlling the chronological model building procedure for logistic regression
chronological_log_reg<-function(dat,newdat,pi=0.5) {
  
  # Fit a model with demographic features only
  f1<-glm(Final~Gender+Race+Home.Language.2+Wealth.Indicator.2,
          family="binomial",data = dat)
  # Select the best combination of features
  s1<-stepAIC(f1,direction = "both",trace=0)
  # Capture in-sample and out-of-sample results
  r1<-log_reg_main(s1,dat = dat,newdat1 = newdat1,pi=pi)
  # Capture variable appearance proportions
  appearances<-var_imp(s1)

  # Fit a model with both demographic and pre-course cognitive features
  f2<-glm(Final~Gender+Race+Home.Language.2+Wealth.Indicator.2+NBT.AL.Score+
            NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
            Accounting+AP.Maths+Physics+Life.Sciences,family="binomial",
            data = dat)
  # Select the best combination of features
  s2<-stepAIC(f2,direction = "both",trace=0)
  # Capture in-sample and out-of-sample results
  r2<-log_reg_main(s2,dat = dat,newdat1 = newdat1,pi=pi)
  # Capture variable appearance proportions
  appearances<-rowSums(cbind(appearances,var_imp(s2)))

  # Set the number of weeks and initialise structures for results storage  
  N<-12
  AICs<-rep(0,N+2)
  AICs[1]<-s1$aic
  AICs[2]<-s2$aic
  results <- vector("list", N+2)
  results[[1]]<-r1
  results[[2]]<-r2
  
  # Iterate over the weeks in the semester
  for(i in 1:N) { 
    # Capture the features up to this point in the semester and find the best model
    new_features<-features[1:i]
    reg_formula<-formula(paste("Final~Gender+Race+Home.Language.2+Wealth.Indicator.2+
                                NBT.AL.Score+NBT.QL.Score+NBT.Math.Score+Math.Prereq.+
                                Maths..High.school.+Accounting+AP.Maths+Physics+
                                Life.Sciences",paste(new_features,collapse="+"),sep="+"))
    f<-glm(reg_formula,family="binomial",data = dat)
    s<-stepAIC(f,direction = "both",trace=0)
    # Store results
    AICs[i+2]<-s$aic
    results[[i+2]]<-log_reg_main(s,dat = dat,newdat1 = newdat1,pi=pi)    
    appearances<-rowSums(cbind(appearances,var_imp(s)))
  }
  return(list(results=results,AICs=AICs,lastmodel=s,appearances=appearances))
}


#models2015<-chronological_log_reg(dat=data2015,newdat1 = data2016,newdat2 = data2017,pi=0.5)
#results2015<-models2015$results
#aic2015<-models2015$AICs
#results2015<-chronological_log_reg(dat=data2015,newdat1 = data2016,newdat2 = data2017[data2017$Wealth.Indicator.2!=2,])
#models2016<-chronological_log_reg(dat=data2016,newdat1 = data2015,newdat2 = data2017,pi=0.5)
#results2016<-models2016$results
#aic2016<-models2016$AICs
#results2016<-chronological_log_reg(dat=data2016[data2016$Wealth.Indicator.2!=2,],newdat1 = data2015[data2015$Wealth.Indicator.2!=2,],newdat2 = data2017[data2017$Wealth.Indicator.2!=2,])
#models2017<-chronological_log_reg(dat=data2017,newdat1 = data2015,newdat2 = data2016,pi=0.5)
#results2017<-models2017$results
#aic2017<-models2017$AICs

N<-12
cv2015<-rep(0,N+2)
cv2016<-rep(0,N+2)
cv2017<-rep(0,N+2)
test2015<-matrix(nrow=6,ncol=N+2)
test2016<-matrix(nrow=6,ncol=N+2)
test2017<-matrix(nrow=6,ncol=N+2)
for(i in 1:(N+2)) {
  cv2015[i]<-results2015[[i]]$cv
  cv2016[i]<-results2016[[i]]$cv
  cv2017[i]<-results2017[[i]]$cv
  
  test2015[1,i]<-results2015[[i]][2][[1]][1] # accuracy
  test2015[2,i]<-results2015[[i]][2][[1]][2]
  test2015[3,i]<-results2015[[i]][4][[1]][1] # sensitivity
  test2015[4,i]<-results2015[[i]][4][[1]][2]
  test2015[5,i]<-results2015[[i]][5][[1]][1] # precision
  test2015[6,i]<-results2015[[i]][5][[1]][2]
  
  test2016[1,i]<-results2016[[i]][2][[1]][1]
  test2016[2,i]<-results2016[[i]][2][[1]][2]
  test2016[3,i]<-results2016[[i]][4][[1]][1]
  test2016[4,i]<-results2016[[i]][4][[1]][2]
  test2016[5,i]<-results2016[[i]][5][[1]][1]
  test2016[6,i]<-results2016[[i]][5][[1]][2]

  test2017[1,i]<-results2017[[i]][2][[1]][1]
  test2017[2,i]<-results2017[[i]][2][[1]][2]
  test2017[3,i]<-results2017[[i]][4][[1]][1]
  test2017[4,i]<-results2017[[i]][4][[1]][2]
  test2017[5,i]<-results2017[[i]][5][[1]][1]
  test2017[6,i]<-results2017[[i]][5][[1]][2]
}

# AIC 
plot(y=aic2015,x=1:(N+2),type='l',ylim=c(200,900),col='blue',main="AIC",xlab="Week",ylab="AIC")
lines(y=aic2016,x=1:(N+2),type='l',col='red')
lines(y=aic2017,x=1:(N+2),type='l',col='green')
legend("topright",legend=c("2015","2016","2017"),
       lty=c("solid","solid","solid"),
       col=c("blue","red","green"))
axis(4,at=c(aic2015[14],aic2016[14],aic2017[14]),labels = round(c(aic2015[14],aic2016[14],aic2017[14])))

# CV ERROR 
plot(y=cv2015,x=1:(N+2),type='l',ylim=c(0,0.4),col='blue',main="CV Error",xlab="Week",ylab="CV error")
lines(y=cv2016,x=1:(N+2),type='l',col='red')
lines(y=cv2017,x=1:(N+2),type='l',col='green')
legend("topright",legend=c("2015","2016","2017"),
       lty=c("solid","solid","solid"),
       col=c("blue","red","green"))

# ACCURACY
plot(y=test2015[1,],x=1:(N+2),type='l',ylim=c(0,1),col='blue',main="Accuracy",xlab="Week",ylab="Accuracy")    
lines(y=test2015[2,],x=1:(N+2),type='l',col='blue',lty=14)              
lines(y=test2016[1,],x=1:(N+2),type='l',col='red')              
lines(y=test2016[2,],x=1:(N+2),type='l',col='red',lty=14)          
lines(y=test2017[1,],x=1:(N+2),type='l',col='green')             
lines(y=test2017[2,],x=1:(N+2),type='l',col='green',lty=14)           
legend("bottomright",legend=c("2015 on 2016","2015 on 2017","2016 on 2015","2016 on 2017","2017 on 2015","2017 on 2016"),
       lty=c("solid","dashed","solid","dashed","solid","dashed"),
       col=c("blue","blue","red","red","green","green"))
abline(h=0.9,lty=14)

# SENSITIVITY
plot(y=test2015[3,],x=1:(N+2),type='l',ylim=c(0,1.5),col='blue',main="Sensitivity",xlab="Week",ylab="Sensitivity")  
lines(y=test2015[4,],x=1:(N+2),type='l',col='blue',lty=14) 
lines(y=test2016[3,],x=1:(N+2),type='l',col='red')
lines(y=test2016[4,],x=1:(N+2),type='l',col='red',lty=14)
lines(y=test2017[3,],x=1:(N+2),type='l',col='green')
lines(y=test2017[4,],x=1:(N+2),type='l',col='green',lty=14)
legend("topright",legend=c("2015 on 2016","2015 on 2017","2016 on 2015","2016 on 2017","2017 on 2015","2017 on 2016"),
       lty=c("solid","dashed","solid","dashed","solid","dashed"),
       col=c("blue","blue","red","red","green","green"))

# PRECISION
plot(y=test2015[5,],x=1:(N+2),type='l',ylim=c(0,1.5),col='blue',main="Precision",xlab="Week",ylab="Precision")  
lines(y=test2015[6,],x=1:(N+2),type='l',col='blue',lty=14) 
lines(y=test2016[5,],x=1:(N+2),type='l',col='red')
lines(y=test2016[6,],x=1:(N+2),type='l',col='red',lty=14)
lines(y=test2017[5,],x=1:(N+2),type='l',col='green')
lines(y=test2017[6,],x=1:(N+2),type='l',col='green',lty=14)
legend("topright",legend=c("2015 on 2016","2015 on 2017","2016 on 2015","2016 on 2017","2017 on 2015","2017 on 2016"),
       lty=c("solid","dashed","solid","dashed","solid","dashed"),
       col=c("blue","blue","red","red","green","green"))

#### MANUAL MODEL BUILDING ####

# fitting univariable models
all_features<-names(data)[-16]
p_values<-rep(0,length(all_features)-2)
summaries<-list()
for(i in 1:(length(all_features)-2)) {
  uni<-glm(formula(paste('data$Final~',paste("data",all_features[i],sep="$"))),family="binomial",data=data)
  p_values[i]<-summary(uni)$coefficients[2,4]
  summaries[[i]]<-data.frame(summary(uni)$coef[summary(uni)$coef[,4] <= .05, 4])
}

# significant features

full_model<-glm(Final~NBT.QL.Score+NBT.Math.Score+Race+Course+Math.Prereq.+
                  Maths..High.school.+AP.Maths+Physics+Life.Sciences+Home.Language.2+Wealth.Indicator.2+Test1+B1+B2+
                  B3+B4+B5+B6+A2+A3+A10+A11+A12+T5+T6+T7+T8+T9+T10,family = 'binomial',data=data) # 466.54

m2<-glm(Final~NBT.QL.Score+NBT.Math.Score+Math.Prereq.+
                  Maths..High.school.+Wealth.Indicator.2+Test1+B1+B2+
                  B3+B4+B5+B6+A2+A3+A10+A11+A12+T5+T6+T7+T8+T9+T10,family = 'binomial',data=data) # 464.56

m3<-glm(Final~NBT.QL.Score+NBT.Math.Score+Math.Prereq.+
          Maths..High.school.+Wealth.Indicator.2+Test1+
          A2+A3+A10+A11+A12+T5+T6+T7+T8+T9+T10,family = 'binomial',data=data) # 461.24

m4<-glm(Final~Math.Prereq.+Maths..High.school.+Wealth.Indicator.2+Test1+
          A2+A3+A10+A11+A12+T5+T6+T7+T8+T9+T10,family = 'binomial',data=data) # 457.96

m5<-glm(Final~Math.Prereq.+Maths..High.school.+Wealth.Indicator.2+Test1+
          A3+T6+T7+T10,family = 'binomial',data=data) # 451.31

m6<-glm(Final~Math.Prereq.+Maths..High.school.+Wealth.Indicator.2+Test1+
          A3+T7+T10,family = 'binomial',data=data) # 452.04 but everything is now very significant

m7<-glm(Final~Math.Prereq.+Maths..High.school.+Wealth.Indicator.2+Test1+
          A3+T6+T7+T10,family = 'binomial',data=data) # 449.55 though not every level is significant

m8<-glm(Final~Race+Math.Prereq.+Maths..High.school.+Wealth.Indicator.2+Test1+
          A3+T6+T7+T10+NBT.Math.Score,family = 'binomial',data=data) # 450.8

#### VARYING THRESHOLD FOR PREDICTIONS ####

# taking m7 further, looking at thresholds

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
#  plot(accuracy~pis,type='l',ylim=c(0,1),col='blue',main=main)
#  lines(sensitivity~pis,col='green')
#  lines(precision~pis,col='red')
#  lines(specificity~pis,col='orange')
#  legend('center',legend = c('accuracy','sensitivity','precision','specificity'),
#         col=c('blue','green','red','orange'),lty=rep('solid',4))
  
  # ROC curve
  one_minus_specificity<-1-specificity
  #plot(sensitivity~one_minus_specificity,type='l',main="ROC curve")
  
  # automated roc
  auc<-roc(newdat$Final,probs,plot=TRUE)
  
  # Youdin's test
  plot(youdin~pis,type='l',main="Youdin's statistic")
  abline(h=0,col='red')
  
  return(auc)
  #return(pis[which.max(youdin)])
}

m9<-lm(Final~Math.Prereq.,data=data_train_balanced)

test_pi(m9,data_test_balanced)
test_pi(m7,data2016)
test_pi(models2017$lastmodel,data2016)
test_pi(m9,data2015)

par(mfrow=c(2,3))
test_pi(models2015$lastmodel,data2016,"2015 on 2016")
test_pi(models2015$lastmodel,data2017,"2015 on 2017")

par(mfrow=c(2,3))
test_pi(models2016$lastmodel,data2015,"2016 on 2015")
test_pi(models2016$lastmodel,data2017,"2016 on 2017")

par(mfrow=c(2,3))
test_pi(models2017$lastmodel,data2015,"2017 on 2015")
test_pi(models2017$lastmodel,data2016,"2017 on 2016")

#### TRYING WHOLE DATASET ####

#resultsFull2<-chronological_log_reg(dat=data_train,newdat1 = data_test,newdat2 = data_test,pi=0.02)

N<-12
cvFull<-rep(0,N+2)
testFull<-matrix(nrow=3,ncol=N+2)
for(i in 1:(N+2)) {
  cvFull[i]<-resultsFull$results[[i]]$cv
  testFull[1,i]<-resultsFull$results[[i]][2][[1]][1] # accuracy
  testFull[2,i]<-resultsFull$results[[i]][4][[1]][1] # sensitivity
  testFull[3,i]<-resultsFull$results[[i]][5][[1]][1] # precision
}
plot(y=cvFull,x=1:(N+2),col='orange',type='l',ylim=c(0,1),
     main="Logistic regression results on full test set",ylab="",xlab="Week")
lines(y=testFull[1,],x=1:(N+2),col='blue')
lines(y=testFull[2,],x=1:(N+2),col='green')
lines(y=testFull[3,],x=1:(N+2),col='red')
legend(x=10,y=0.4,legend=c('CV error','Accuracy','Sensitivity','Precision'),
       col=c('orange','blue','green','red'),lty=rep('solid',4))
abline(h=0.9,lty=14)

par(mfrow=c(1,3))
test_pi(resultsFull$lastmodel,data_test,main="Full data")

#### TRYING BALANCED SUBSET OF DATA ####

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

#resultsBalanced2<-cf(dat=data_train_balanced,newdat1 = data_test_balanced,newdat2 = data_test_balanced,pi=0.56)

N<-12
cvBalanced<-rep(0,N+2)
testBalanced<-matrix(nrow=3,ncol=N+2)
for(i in 1:(N+2)) {
  cvBalanced[i]<-resultsBalanced$results[[i]]$cv
  testBalanced[1,i]<-resultsBalanced$results[[i]][2][[1]][1] # accuracy
  testBalanced[2,i]<-resultsBalanced$results[[i]][4][[1]][1] # sensitivity
  testBalanced[3,i]<-resultsBalanced$results[[i]][5][[1]][1] # precision
}
misclassBalanced<-1-testBalanced[1,]
plot(y=cvFull,x=1:(N+2),col='orange',type='l',ylim=c(0,1),
     main="Logistic regression results on full test set",ylab="",xlab="Week")
lines(y=testBalanced[1,],x=1:(N+2),col='blue')
lines(y=testBalanced[2,],x=1:(N+2),col='green')
lines(y=testBalanced[3,],x=1:(N+2),col='red')
lines(y=misclassBalanced,x=1:(N+2),col='brown')
legend(x=10,y=0.4,legend=c('CV error','Accuracy','Sensitivity','Precision'),
       col=c('orange','blue','green','red'),lty=rep('solid',4))
abline(h=0.9,lty=14)

par(mfrow=c(1,3))
pi<-test_pi(resultsBalanced$lastmodel,data_test_balanced,main="Balanced data")

# trying a different threshold

#resultsBalanced2<-chronological_log_reg(dat=data_train_balanced,newdat1 = data_test_balanced,newdat2 = data_test_balanced,pi=pi)
N<-12
cvBalanced2<-rep(0,N+2)
testBalanced2<-matrix(nrow=3,ncol=N+2)
for(i in 1:(N+2)) {
  cvBalanced2[i]<-resultsBalanced2$results[[i]]$cv
  testBalanced2[1,i]<-resultsBalanced2$results[[i]][2][[1]][1] # accuracy
  testBalanced2[2,i]<-resultsBalanced2$results[[i]][4][[1]][1] # sensitivity
  testBalanced2[3,i]<-resultsBalanced2$results[[i]][5][[1]][1] # precision
}
misclassBalanced2<-1-testBalanced2[1,]
plot(y=cvFull,x=1:(N+2),col='orange',type='l',ylim=c(0,1),
     main="Logistic regression results on full test set",ylab="",xlab="Week")
lines(y=testBalanced2[1,],x=1:(N+2),col='blue')
lines(y=testBalanced2[2,],x=1:(N+2),col='green')
lines(y=testBalanced2[3,],x=1:(N+2),col='red')
lines(y=misclassBalanced2,x=1:(N+2),col='brown')
legend(x=10,y=0.4,legend=c('CV error','Accuracy','Sensitivity','Precision'),
       col=c('orange','blue','green','red'),lty=rep('solid',4))
abline(h=0.9,lty=14)
plot(y=resultsBalanced2$AICs,x=1:(N+2),type='l',main="AIC",ylab="",xlab="Week")

par(mfrow=c(1,3))
pi2<-test_pi(resultsBalanced$lastmodel,data_test_balanced,main="Balanced data")

#### FUNCTIONS FOR MLR ####


reg_triple<-function(fit,dat,newdat1,newdat2) { 
  
  # CV error
  cv<-cv.lm(fit,K=10,seed=1)$MSE[1]
  
  # first prediction
  prediction1<-predict.lm(fit,newdata=newdat1,type="response")
  mse1<-MSE(prediction1,newdat1$Final)
  trueFail<-sum(ifelse((newdat1$Final<50)&(prediction1<50),1,0))
  truePass<-sum(ifelse((newdat1$Final>=50)&(prediction1>=50),1,0))
  falseFail<-sum(ifelse((newdat1$Final>=50)&(prediction1<50),1,0))
  falsePass<-sum(ifelse((newdat1$Final<50)&(prediction1>=50),1,0))
  confusion1<-matrix(c(trueFail,falseFail,falsePass,truePass),nrow=2,byrow = TRUE)
  accuracy1<-(trueFail+truePass)/(trueFail+truePass+falsePass+falseFail)
  sensitivity1<-trueFail/(trueFail+falsePass)
  precision1<-trueFail/(trueFail+falseFail)
  
  # second prediction
  prediction2<-predict.lm(fit,newdata=newdat2,type="response")
  mse2<-MSE(prediction2,newdat2$Final)
  trueFail<-sum(ifelse((newdat2$Final<50)&(prediction2<50),1,0))
  truePass<-sum(ifelse((newdat2$Final>=50)&(prediction2>=50),1,0))
  falseFail<-sum(ifelse((newdat2$Final>=50)&(prediction2<50),1,0))
  falsePass<-sum(ifelse((newdat2$Final<50)&(prediction2>=50),1,0))
  confusion2<-matrix(c(trueFail,falseFail,falsePass,truePass),nrow=2,byrow = TRUE)
  accuracy2<-(trueFail+truePass)/(trueFail+truePass+falsePass+falseFail)
  sensitivity2<-trueFail/(trueFail+falsePass)
  precision2<-trueFail/(trueFail+falseFail)
  
  return(list(cv=cv,MSEs=c(mse1,mse2),accuracies=c(accuracy1,accuracy2),confusions=c(confusion1,confusion2),
              sensitivities=c(sensitivity1,sensitivity2),precisions=c(precision1,precision2)))
}

chronological_reg<-function(dat,year1="",newdat1,year2="",newdat2) {
  
  # demographic features only
  f1<-lm(Final~Gender+Race+Home.Language.2+Wealth.Indicator.2,data = dat,y=TRUE,x=TRUE)
  s1<-stepAIC(f1,direction = "both",trace=0)
  r1<-reg_triple(s1,dat = dat,newdat1 = newdat1,newdat2 = newdat2)
  appearances<-var_imp(s1)
  
  # demographic and pre-course cognitive
  f2<-lm(Final~Gender+Race+Home.Language.2+Wealth.Indicator.2+NBT.AL.Score+
            NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
            Accounting+AP.Maths+Physics+Life.Sciences,data = dat,y=TRUE,x=TRUE)
  s2<-stepAIC(f2,direction = "both",trace=0)
  r2<-reg_triple(s2,dat = dat,newdat1 = newdat1,newdat2 = newdat2)
  appearances<-rowSums(cbind(appearances,var_imp(s2)))
  
  N<-12
  AICs<-rep(0,N+2)
  AICs[1]<-summary(s1)$adj.r.squared
  AICs[2]<-summary(s2)$adj.r.squared
  results <- vector("list", N+2)
  results[[1]]<-r1
  results[[2]]<-r2
  
  # during semester data: iteration i is week i+2
  for(i in 1:N) { 
    new_features<-features[1:i]
    reg_formula<-formula(paste("Final~Gender+Race+Home.Language.2+Wealth.Indicator.2+
                               NBT.AL.Score+NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
                               Accounting+AP.Maths+Physics+Life.Sciences",paste(new_features,collapse="+"),sep="+"))
    #print(reg_formula)
    f<-lm(reg_formula,data = dat,y=TRUE,x=TRUE)
    s<-stepAIC(f,direction = "both",trace=0)
    AICs[i+2]<-summary(s)$adj.r.squared
    results[[i+2]]<-reg_triple(s,dat = dat,newdat1 = newdat1,newdat2 = newdat2)  
    if(i==5) lastmodel<-s
    appearances<-rowSums(cbind(appearances,var_imp(s)))
  }
  
  return(list(results=results,AICs=AICs,lastmodel=lastmodel,appearances=appearances))
}

#### TESTING MLR ON FULL DATASET AND REBALANCED SUBSET ####

#resultsRegFull<-chronological_reg(data_train,newdat1 = data_test,newdat2 = data_test)

N<-12
cvReg<-rep(0,N+2)
aicReg<-rep(0,N+2)
for(i in 1:(N+2)) {
  cvReg[i]<-resultsRegFull$results[[i]]$cv$mean
  testReg[1,i]<-resultsRegFull$results[[i]]$accuracies[1] # accuracy
  testReg[2,i]<-resultsRegFull$results[[i]]$sensitivities[1] # sensitivity
  testReg[3,i]<-resultsRegFull$results[[i]]$precisions[1] # precision
}
misclassReg<-1-testReg[1,]

# rebalancing
M<-nrow(data[data$Final<50,])
set.seed(1)
data.F<-data[data$Final<50,]
data.P<-data[data$Final>=50,]
passIndex<-sample(seq_len(nrow(data.P)),size=M)
data.P<-data.P[passIndex,]
data.downsample<-rbind(data.F,data.P)

n<-floor(0.8*nrow(data.downsample))# justify split
training_index<-sample(seq_len(nrow(data.downsample)),size=n)
data_train_balanced<-data.frame(data.downsample[training_index,])
data_test_balanced<-data.frame(data.downsample[-training_index,])

#resultsRegBalanced<-chronological_reg(data_train_balanced,newdat1 = data_test_balanced,newdat2 = data_test_balanced)

# plot full
par(mfrow=c(2,3))

plot(y=testReg[1,],x=1:(N+2),col='blue',ylim=c(0,1),type='l',
      main="MLR results on full test set",ylab="",xlab="Week")
lines(y=testReg[2,],x=1:(N+2),col='green')
lines(y=testReg[3,],x=1:(N+2),col='red')
legend(x=10,y=0.4,legend=c('CV error','Accuracy','Sensitivity','Precision'),
       col=c('orange','blue','green','red'),lty=rep('solid',4))
abline(h=0.9,lty=14)

plot(y=cvReg,x=1:(N+2),col='orange',type='l',
     main="CV error (MSE)",ylab="",xlab="Week")

plot(y=resultsRegFull$AICs,x=1:(N+2),type='l',main='AIC')

# plot rebalanced

N<-12
cvRegBalanced<-rep(0,N+2)
testRegBalanced<-matrix(nrow=3,ncol=N+2)
for(i in 1:(N+2)) {
  cvRegBalanced[i]<-resultsRegBalanced$results[[i]]$cv$mean
  testRegBalanced[1,i]<-resultsRegBalanced$results[[i]]$accuracies[1] # accuracy
  testRegBalanced[2,i]<-resultsRegBalanced$results[[i]]$sensitivities[1] # sensitivity
  testRegBalanced[3,i]<-resultsRegBalanced$results[[i]]$precisions[1] # precision
}
misclassRegBalanced<-1-testRegBalanced[1,]

plot(y=testRegBalanced[1,],x=1:(N+2),col='blue',type='l',xlab="Week",ylim=c(0,1),
     main="MLR resuts on rebalanced test set")
lines(y=testRegBalanced[2,],x=1:(N+2),col='green')
lines(y=testRegBalanced[3,],x=1:(N+2),col='red')
legend(x=10,y=0.4,legend=c('CV error','Accuracy','Sensitivity','Precision'),
       col=c('orange','blue','green','red'),lty=rep('solid',4))
abline(h=0.9,lty=14)

plot(y=cvRegBalanced,x=1:(N+2),col='orange',type='l',
     main="CV error (MSE)",ylab="",xlab="Week")

plot(y=resultsRegBalanced$AICs,x=1:(N+2),type='l',main='AIC')

# capturing the number of falseFails
par(mfrow=c(1,2))

## MLR full and rebalanced
falseFailsReg<-sapply(resultsRegFull$results,FUN = function(l) l$confusions[3])
falseFailsRegBalanced<-sapply(resultsRegBalanced$results,FUN = function(l) l$confusions[3])
plot(y=falseFailsReg,x=1:14,type='b',col='blue',main='False fails from MLR')
lines(y=falseFailsRegBalanced,x=1:14,type='b',col='green') # good
legend('bottomright',legend=c('Original','Rebalanced'),col=c('blue','green'),lty=rep('solid',2))

## logistic full and rebalanced
falseFails<-sapply(resultsFull$results,FUN=function(l) l$confusions[[1]][1,2])
falseFails[1]<-0
falseFailsBalanced<-sapply(resultsBalanced$results,FUN=function(l) l$confusions[[1]][1,2])
plot(y=falseFailsBalanced[-1],x=2:14,type='b',col='green',ylim=c(0,22),main='False fails from logistic regression')
lines(y=falseFails[-1],x=2:14,type='b',col='blue') # good
legend('bottomright',legend=c('Original','Rebalanced'),col=c('blue','green'),lty=rep('solid',2))

## logistic per year
falseFails2015<-cbind(sapply(models2015$results,FUN=function(l) l$confusions[[1]][1,2]),
                      sapply(models2015$results,FUN=function(l) l$confusions[[2]][1,2]))
falseFails2015[1,]<-0    
falseFails2016<-cbind(sapply(models2016$results,FUN=function(l) l$confusions[[1]][1,2]),
                      sapply(models2016$results,FUN=function(l) l$confusions[[2]][1,2]))
falseFails2016[1,]<-0   
falseFails2017<-cbind(sapply(models2017$results,FUN=function(l) l$confusions[[1]][1,2]),
                      sapply(models2017$results,FUN=function(l) l$confusions[[2]][1,2]))
falseFails2017[1,]<-0   

plot(y=falseFails2017[,2],x=1:14,type='b',col='green',lty=14,ylim=c(0,130))
lines(y=falseFails2017[,1],x=1:14,type='b',col='green')
lines(y=falseFails2016[,2],x=1:14,type='b',col='red',lty=14)
lines(y=falseFails2016[,1],x=1:14,type='b',col='red')
lines(y=falseFails2015[,2],x=1:14,type='b',col='blue',lty=14)
lines(y=falseFails2015[,1],x=1:14,type='b',col='blue')

#### VARIABLE APPEARANCE ####

expanded_features<-names(xdata)[-c(4,5)]
expanded_features[8]<-"RaceNon Applicable/Unknown"
expanded_demographic<-expanded_features[c(4:9,18:22)]
expanded_precourse<-expanded_features[c(1:3,10:17)]

var_imp<-function(model) { # takes in a result from stepAIC
  (expanded_features %in% names(model$coefficients))
}

var_imp_plot<-function(models) { 
  appearances<-models$appearances
  names(appearances)<-expanded_features
  appearances<-sort(appearances)
  par(las=2)
  par(mar=c(5,15,4,2)+0.1,mgp=c(4,1,0))
  # appearance as a proportion of whole semester
  #barplot(appearances/14,names.arg=names(appearances),main="Variable appearance",horiz=TRUE,cex.names = 0.5,xlab="Proportion of appearance")
  
  # appearance as a proportion of times it could actually appear
  pools<-rep(0,length(expanded_features))
  bool<-ifelse(models$appearances>0,1,0)
  for(i in 1:length(bool)) {
    if(bool[i]==1) {
      # check for demographic
      if(expanded_features[i] %in% expanded_demographic) pools[i]<-14
      # check for precourse cognitive
      else if(expanded_features[i] %in% expanded_precourse) pools[i]<-13
      # check for during semester data
      else pools[i]<-12-grep(expanded_features[i],features)[1]+1
    }
  }
  
  props<-ifelse(pools>0,models$appearances/pools,0)
  names(props)<-expanded_features
  props<-props[props>0]
  props<-sort(props)
  barplot(props[props>0],names.arg=names((props))[props>0],main="Variable Appearance",cex.main=2,
          horiz=TRUE,cex.lab = 2,xlab="Proportion of appearance",cex.names=0.9,col="deepskyblue3",border=NA)
}

var_imp_plot(models2015$appearances)
var_imp_plot(models2016$appearances)
var_imp_plot(models2017$appearances)
var_imp_plot(resultsFull$appearances)
var_imp_plot(resultsBalanced$appearances)
var_imp_plot(resultsRegFull$appearances)
var_imp_plot(resultsRegBalanced$appearances)

 




#### WORKING WITH 2015+2016 VS 2017 ####

data_train_years <- rbind(data2015,data2016)
#models_years2 <- chronological_log_reg(data_train_years,newdat1 = data2017,newdat2 = data2017,pi=0.8)

N<-12
cvYears<-rep(0,N+2)
testYears<-matrix(nrow=3,ncol=N+2)
for(i in 1:(N+2)) {
  cvYears[i]<-models_years$results[[i]]$cv
  testYears[1,i]<-models_years$results[[i]][2][[1]][1] # accuracy
  testYears[2,i]<-models_years$results[[i]][4][[1]][1] # sensitivity
  testYears[3,i]<-models_years$results[[i]][5][[1]][1] # precision
}
plot(y=cvYears,x=1:(N+2),col='orange',type='l',ylim=c(0,1),
     main="Logistic regression results on full test set",ylab="",xlab="Week")
lines(y=testYears[1,],x=1:(N+2),col='blue')
lines(y=testYears[2,],x=1:(N+2),col='green')
lines(y=testYears[3,],x=1:(N+2),col='red')
legend(x=10,y=0.4,legend=c('CV error','Accuracy','Sensitivity','Precision'),
       col=c('orange','blue','green','red'),lty=rep('solid',4))
abline(h=0.9,lty=14)

par(mfrow=c(1,3))
test_pi(models_years$lastmodel,data2017,main="Full data")

#### LOGISTIC: WORKING WITH REBALANCED TRAIN BUT IMBALANCED TEST ####

# rebalancing
M<-nrow(data_train[data_train$Final==0,])
set.seed(1)
data.F<-data_train[data_train$Final==0,]
data.P<-data_train[data_train$Final==1,]
passIndex<-sample(seq_len(nrow(data.P)),size=M)
data.P<-data.P[passIndex,]
data_train_balanced2<-rbind(data.F,data.P)

#resultsBalImbal<-chronological_log_reg(data_train_balanced2,newdat1 = data_test,newdat2=data_test)

#### LINEAR: WORKING WITH REBALANCED TRAIN BUT IMBALANCED TEST ####

# rebalancing
M<-nrow(data_train[data_train$Final<50,])
set.seed(1)
data.F<-data_train[data_train$Final<50,]
data.P<-data_train[data_train$Final>=50,]
passIndex<-sample(seq_len(nrow(data.P)),size=M)
data.P<-data.P[passIndex,]
data_train_balanced2<-rbind(data.F,data.P)

#resultsBalImbal<-chronological_log_reg(data_train_balanced2,newdat1 = data_test,newdat2=data_test)

#### RUNNING ALL THE LINEAR STUFFS ####

#models_yearsReg<-chronological_reg(data_train_years,newdat1 = data2017,newdat2 = data2017)
#resultsRegFull<-chronological_reg(data_train,newdat1 = data_test,newdat2 = data_test)
#resultsRegBalanced<-chronological_reg(data_train_balanced,newdat1 = data_test_balanced,newdat2 = data_test_balanced)
#resultsRegBalImbal<-chronological_reg(data_train_balanced2,newdat1 = data_test,newdat2 = data_test)

#### PRIOR CORRECTION ####

prior_correction<-function(model,tau,dat) {
  xdata<-data.frame(model.matrix(Final~.,dat)[,-1])
  indicator<-names(xdata) %in% names(model$coefficients)
  ybar<-sum(dat$Final==0)/nrow(dat)
  C<-log(((1-tau)/tau)*(ybar/(1-ybar)))
  pred<-rep(model$coefficients[1][[1]]-C,nrow(xdata))
  # for each observation
  for(i in 1:nrow(xdata)) {
    #k<-1
    # for each feature
    for(j in 1:length(indicator)) { 
      if(indicator[j]==TRUE) { 
        pred[i]<-pred[i]+model$coefficients[names(xdata)[j]]*xdata[i,j]
      }
    }
    #print(pred[i])
    pred[i]<-(1+exp(pred[i]))^(-1)
  }
  return(1-pred)
} 

#resultsFullPrior<-chronological_log_reg(data_train,newdat1 = data_test,newdat2 = data_test,prior=TRUE)
