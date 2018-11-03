library(readxl)
library(pROC)
library(car)
library(ResourceSelection)

##########***** LOGISTIC REGRESSION *****##########
# run the following to prepare data for logistic regression output

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

#### SPLITTING TEST AND TRAINING ON FULL DATASET ####

n<-floor(0.8*nrow(data))# justify split
set.seed(46)
training_index<-sample(seq_len(nrow(data)),size=n)
data_train<-data.frame(data[training_index,])
data_test<-data.frame(data[-training_index,])

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

# rebalancing
M<-nrow(data_train[data_train$Final==0,])
set.seed(1)
data.F<-data_train[data_train$Final==0,]
data.P<-data_train[data_train$Final==1,]
passIndex<-sample(seq_len(nrow(data.P)),size=M)
data.P<-data.P[passIndex,]
data_train_balanced2<-rbind(data.F,data.P)



##########***** CHOSEN LOGISTIC REGRESSION MODEL *****##########

data_train.releved<-within(data_train, Wealth.Indicator.2 <- relevel(Wealth.Indicator.2, ref = 2))
fit<-glm(Final~Wealth.Indicator.2+Math.Prereq.+Maths..High.school.+A2+T3+B1+A3+A4+T5+Test1+
           B2,data=data_train,family="binomial")

#fit<-glm(Final~Gender+Race+Home.Language.2+Wealth.Indicator.2+
#           NBT.AL.Score+NBT.QL.Score+NBT.Math.Score+Math.Prereq.+Maths..High.school.+
#           Accounting+AP.Maths+Physics+Life.Sciences+A1+T2+A2+T3+B1+A3+T4+A4+T5+Test1+
#           B2+A5,data=data_train.releved,family="binomial")
#fit<-stepAIC(fit,direction = "both",trace=0)

pi<-predict.glm(fit,newdata = data_train,type="response")
auc<-roc(data_train$Final,pi)

par(mfrow=c(1,2))
plot.roc(auc)
y<-auc$sensitivities+auc$specificities-1
x<-auc$thresholds
plot(y~x,type='l')
bestpi<-x[which.max(y)]

pi<-predict.glm(fit,newdata = data_test,type="response")

pred1<-ifelse(pi>=0.5,1,0)
pred2<-ifelse(pi>=bestpi,1,0)

# threshold=0.5
confusion1<-resultsFull$results[[7]]$confusions
auc1<-resultsFull$results[[7]]$AUC$auc[1]
accuracy1<-resultsFull$results[[7]]$accuracies
specificity1<-resultsFull$results[[7]]$sensitivities
NPV1<-resultsFull$results[[7]]$precisions
falseFails1<-resultsFull$results[[7]]$confusions[1,2]

# threshold=optimal
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

OR<-as.matrix(exp(summary(fit)$coefficients[,1]),ncol=1)



#### MODEL CHECKING ####

par(mfrow=c(1,3))
gOfEta<-fit$fitted.values
eta<-log(gOfEta/(1-gOfEta))
temp<-as.numeric(data_train$Final)-1
#plot(gOfEta[-678]~eta[-678],main="Linearity of link function",ylab="Link",xlab=expression(eta))
plot(y=temp,x=data_train$Math.Prereq.,cex.lab=2,cex.main=2,cex.axis=2,
     main="Prereqiusite Maths",ylab="Final",xlab="Prerequisite Maths")
plot(y=temp,x=data_train$Maths..High.school.,cex.lab=2,cex.main=2,cex.axis=2,
     main="High School Maths",ylab="Final",xlab="High School Maths")
plot(y=temp,x=data_train$Test1,main="Test 1",cex.lab=2,cex.main=2,cex.axis=2,
     ylab="Final",xlab="Test 1")

cooksd <- cooks.distance(fit)
cutoff<-4*mean(cooksd, na.rm=T)
cutoff<-4/(nrow(data_train)-length(fit$coefficients)-1)

plot(cooksd, pch=".", cex=2, main="Influential Observations by Cook's Distance",cex.lab=2,cex.main=2,cex.axis=2,
     type='h',ylab="Cook's Distance",xlab="Observation number")  # plot cook's distance
abline(h = cutoff, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>0.02,names(cooksd),""),
     col="red",cex=2)  # add labels

summary(data_train[cooksd>cutoff,])
data_train["374",]

data_train.mod<-data_train[cooksd<=cutoff,]

# hosmer lemeshow test
h1<-hoslem.test(fit$y,fitted(fit),g=10)

plot(fit$y,fitted(fit),xlab="True response",cex.lab=2,cex.main=2,cex.axis=2,
     ylab="Fitted values",main="Predicted probabilities vs response")


#### ALL LOG REG MODELS ####

models_years <- chronological_log_reg(data_train_years,newdat1 = data2017)
resultsFull<-chronological_log_reg(dat=data_train,newdat1 = data_test)
resultsBalanced<-chronological_log_reg(dat=data_train_balanced,newdat1 = data_test_balanced)
resultsBalImbal<-chronological_log_reg(data_train_balanced2,newdat1 = data_test)

performance_logistic(models_years,data2017)
performance_logistic(resultsFull,data_test,7)
performance_logistic(resultsBalanced,data_test_balanced)
performance_logistic(resultsBalImbal,data_test)


##########***** LINEAR REGRESSION *****##########
# run the following to prepare data for linear regression output
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

#### SPLITTING TEST AND TRAINING ON FULL DATASET ####

n<-floor(0.8*nrow(data))# justify split
set.seed(46)
training_index<-sample(seq_len(nrow(data)),size=n)
data_train<-data.frame(data[training_index,])
data_test<-data.frame(data[-training_index,])

#### TRYING BALANCED SUBSET OF DATA ####

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

# rebalancing
M<-nrow(data_train[data_train$Final<50,])
set.seed(1)
data.F<-data_train[data_train$Final<50,]
data.P<-data_train[data_train$Final>=50,]
passIndex<-sample(seq_len(nrow(data.P)),size=M)
data.P<-data.P[passIndex,]
data_train_balanced2<-rbind(data.F,data.P)


#### ALL MLR MODELS ####

models_yearsReg<-chronological_reg(data_train_years,newdat1 = data2017,newdat2 = data2017)
resultsRegFull<-chronological_reg(data_train,newdat1 = data_test,newdat2 = data_test)
resultsRegBalanced<-chronological_reg(data_train_balanced,newdat1 = data_test_balanced,newdat2 = data_test_balanced)
resultsRegBalImbal<-chronological_reg(data_train_balanced2,newdat1 = data_test,newdat2 = data_test)

performance_linear(models_yearsReg,data2017)
performance_linear(resultsRegFull,data_test)
performance_linear(resultsRegBalanced,data_test_balanced)
performance_linear(resultsRegBalImbal,data_test)

##########***** CHOSEN MLR MODEL *****##########

data_train.releved<-within(data_train, Wealth.Indicator.2 <- relevel(Wealth.Indicator.2, ref = 4))
data_train.releved<-within(data_train.releved, Gender <- relevel(Gender, ref = 1))

fit<-lm(formula = Final ~ Gender + Wealth.Indicator.2 + NBT.AL.Score + 
     Math.Prereq. + Maths..High.school. + A2 + T3 + B1 + A3 + 
     T4 + A4 + Test1 + B2 + A5, data = data_train)
summary(fit)

nrow(data_train[data_train$Wealth.Indicator.2==5,])/nrow(data_train)
nrow(data_train[data_train$Wealth.Indicator.2==6,])/nrow(data_train)

#### MODEL CHECKING ####


res<-fit$residuals
par(mfrow=c(1,3))
plot(y=res,x=fit$fitted.values,main="Residuals vs fitted values")
abline(h=0,col='red')
acf(res,main="Autocorrelation of residuals")
hist(res,probability = TRUE,main="Distribution of residuals")
lines(seq(-40, 40, by=1), dnorm(seq(-40, 40, by=1),
                              mean(res), sd(res)), col="red")
par(mfrow=c(1,1))
qqPlot(res,main="Normal QQ-Plot of residuals")

par(mfrow=c(2,2))
plot(fit,which=1,cex.lab=2,cex.axis=2)
acf(res,main="Autocorrelation of residuals",cex.lab=2,cex.axis=2)
hist(res,probability = TRUE,main="Distribution of residuals",cex.lab=2,cex.axis=2)
lines(seq(-40, 40, by=1), dnorm(seq(-40, 40, by=1),
                                mean(res), sd(res)), col="red")
plot(fit,which=2,cex.lab=2,cex.axis=2)

student_res<-outlierTest(fit,order=TRUE)

par(mfrow=c(1,3))
plot(y=data_train$Final,x=data_train$Math.Prereq.,main="Prerequisite Maths",
     xlab="Prerequisite Maths",ylab="Final",cex.lab=2,cex.main=2,cex.axis=2)
abline(lm(data_train$Final~data_train$Math.Prereq.), col="red") # regression line (y~x) 

plot(y=data_train$Final,x=data_train$Maths..High.school.,main="High School Maths",
     xlab="High School Maths",ylab="Final",cex.lab=2,cex.main=2,cex.axis=2)
abline(lm(data_train$Final~data_train$Maths..High.school.), col="red") # regression line (y~x) 

plot(y=data_train$Final,x=data_train$Test1,main="Test 1",
     xlab="Test 1",ylab="Final",cex.lab=2,cex.main=2,cex.axis=2)
abline(lm(data_train$Final~data_train$Test1), col="red") # regression line (y~x) 



shapiro.test(res)
ks.test(res, "pnorm")
# or rather use
ks.test((res-mean(res))/sd(res),"pnorm")

cooksd <- cooks.distance(fit)
cutoff<-4*mean(cooksd, na.rm=T)
cutoff<-4/(nrow(data_train)-length(fit$coefficients)-1)

plot(cooksd, pch=".",  main="Influential Observations by Cook's Distance",
     type='h',cex.lab=2,cex.main=2,cex.axis=1.5,ylab="Cook's Distance",xlab="Observation number")  # plot cook's distance
abline(h = cutoff, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>0.015,names(cooksd),""),
     col="red",cex=2)  # add labels

summary(data_train[cooksd>cutoff,])
data_train["1937",]
data_train["51",]

data_train.mod<-data_train[cooksd<=cutoff,]

