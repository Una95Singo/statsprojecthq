library(dplyr)

# exploratory stuff
prob = function(x){
  return(exp(model$coefficients[[1]] + x*model$coefficients[[2]])/(1 +exp(model$coefficients[[1]] + x*model$coefficients[[2]])))
}

one_predictor<-function(data,predictor) {
  model<-glm(formula(paste("Final",t,sep ="~")),data = data,family = "binomial")
  plot(y = as.numeric(data$Final)-1, x=data[predictor][,1], main=predictor,xlim=c(0,100))
  curve(prob, from = 0 , to=100, add=T)
  abline(h=0.5,  col="red", lwd=1, lty=2) # 50% threshold
  abline(v = 32, col="red", lwd=1, lty=2) # 50% threshold
}

# parameter is an object returned by chronological_log_reg
performance_logistic<-function(models,newdat,model=14) {
  par(mar=c(5,6,4,2)+0.1,mgp=c(4,1,0))
  par(las=1)
  N<-12
  par(mfrow=c(1,3))
  
  # AIC
  plot(y=models$AICs,x=1:(N+2),type='l',main="AIC",ylab="AIC",xlab="Time point",cex.lab=2,cex.axis=2,cex.main=2)
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
  plot(y=cv,x=1:(N+2),col='orange',type='l',ylim=c(0,1),cex.lab=2,cex.axis=2,cex.main=2,
       main="Predictive Performance",ylab="Proportion",xlab="Time point")
  lines(y=test[1,],x=1:(N+2),col='blue')
  lines(y=test[2,],x=1:(N+2),col='green')
  lines(y=test[3,],x=1:(N+2),col='red')
  lines(y=test[4,],x=1:(N+2),col='brown')
  lines(y=test[5,],x=1:(N+2),col='purple') 
  legend(x=8,y=0.425,legend=c('CV error','Accuracy','Specificity','NPV','AUC','Sensitivity'),
         col=c('orange','blue','green','red','brown','purple'),lty=rep('solid',5),cex=1.5,bty="n")
  abline(v=7,lty=14)
  abline(h=0.9,lty=14)
  box(lwd=1.5)
  # deprecated:
  # ROC and youdin
  #plot.roc(models$results[[model]]$AUC,main="ROC curve")
  #y<-models$results[[model]]$AUC$sensitivities+models$results[[model]]$AUC$specificities-1
  #x<-models$results[[model]]$AUC$thresholds
  #plot(y~x,type='l',main="Youden's statistic",xlab="Threshold",ylab="sensitivity+specificity-1")
  
  #par(mfrow=c(1,2))
  # false fails
  falseFails<-sapply(models$results,FUN=function(l) l$confusions[1,2])
  #falseFails[1]<-0
  if(nrow(models$results[[1]]$confusions)==1) falseFails[1]<-0
  plot(y=falseFails/378,x=1:14,type='b',col='green',main='Model 12',
       xlab='Time point',ylab="Proportion of test set",ylim=c(0,0.4),cex.lab=2,cex.axis=2,cex.main=2)
  
  FP<-sapply(models$results,FUN=function(l) ifelse(nrow(l$confusions)>1,l$confusions[2,1],l$confusions[1]))
  lines(y=FP/378,x=1:14,col='blue',type='b')
  legend("topright",legend=c("False fails","False passes"),
         col=c('Green','Blue'),lty=c('solid','solid'),cex=2,bty="n")
  box(lwd=1.5)
  # variable importance
  #var_imp_plot(models)
  
  return(list(AUC=mean(test[4,]),accuracy=mean(test[1,]),specificity=mean(test[2,]),sensitivity=mean(test[5,]),falsefails=mean(falseFails),falsePasses=mean(FP)))
}

# parameter is an object returned by chronological_reg
performance_linear<-function(models,newdat) {
  par(las=1)
  par(mar=c(5,6,4,2)+0.1,mgp=c(4,1,0))
  N<-12
  par(mfrow=c(1,3))
  
  # AIC
  plot(y=models$AICs,x=1:(N+2),type='l',main="Adjusted R-Squared",cex.lab=2,cex.axis=2,cex.main=2,
       ylab="Proportion",xlab="Time point")

  # accuracy, sensitivity, precision
  N<-12
  cv<-rep(0,N+2)
  test<-matrix(nrow=5,ncol=N+2)
  for(i in 1:(N+2)) {
    cv[i]<-models$results[[i]]$cv$mean
    test[1,i]<-models$results[[i]]$accuracies[1] # accuracy
    test[2,i]<-models$results[[i]]$sensitivities[1] # sensitivity
    test[3,i]<-models$results[[i]]$precisions[1] # precision
    test[4,i]<-models$results[[i]]$cv$mean # CV MSE
    FN<-models$results[[i]]$confusions[3]
    FP<-models$results[[i]]$confusions[2]
    TN<-models$results[[i]]$confusions[1]
    TP<-models$results[[i]]$confusions[4]
    
    #test[5,i]<-FP/(FP+TN) # FPR
    test[5,i]<-1-FN/(FN+TP) # sensitivity
  }
  
  # CV MSE
  plot(y=test[4,],x=1:(N+2),type='l',main="CV error",ylab="Average MSE",xlab="Time point",cex.lab=2,cex.axis=2,cex.main=2,)
  
  plot(y=test[1,],x=1:(N+2),col='blue',type='l',ylim=c(0,1),cex.lab=2,cex.axis=2,cex.main=2,
       main="Predictive Performance",ylab="Proportion",xlab="Time point")
  lines(y=test[2,],x=1:(N+2),col='green')
  lines(y=test[3,],x=1:(N+2),col='red')
  lines(y=test[5,],x=1:(N+2),col='purple')
  legend("bottomright",legend=c('Accuracy','Specificity','NPV','Sensitivity'),
         col=c('blue','green','red','purple'),lty=rep('solid',3),bty="n",cex=1.5)
  abline(h=0.9,lty=14)
  abline(v=7,lty=14)

  #par(mfrow=c(1,2))
  # false fails
  falseFails<-sapply(models$results,FUN=function(l) l$confusions[3])
  #falseFails[1]<-0
  plot(y=falseFails/378,x=1:14,type='b',col='green',cex.lab=2,cex.axis=2,cex.main=2,
       main='Model 22',xlab='Time point',ylab="Percentage",ylim=c(0,0.4))
  
  FP<-sapply(models$results,FUN=function(l) l$confusions[2])
  lines(y=FP/378,x=1:14,col='blue',type='b')
  legend("topright",legend=c("False fails","False passes"),bty="n",cex=2,
         col=c('Green','Blue'),lty=c('solid','solid'))
  
  # variable importance
  #var_imp_plot(models)

  return(list(adjR2=mean(models$AICs),accuracy=mean(test[1,]),specificity=mean(test[2,]),
              sensitivity=mean(test[5,]),falsefails=mean(falseFails),falsePasses=mean(FP)))
  
}
