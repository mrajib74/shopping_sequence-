getwd()
setwd("C:/Software/xlri/Datamining/Resource/")

library(MASS)
library(corrplot)
library(car)
library(caTools)
library(ROCR)
bankdata<-read.csv("bankdata1.csv",header=TRUE)
##Find out if there are any NAs,
naColumns <- function(df) 
  {
      colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
  }
naColumns(bankdata)

##Find Mode

job_mode<-names(table(bankdata$job))[table(bankdata$job)==max(table(bankdata$job))]

bankdata$job[bankdata$job == "unknown"] <- job_mode

# Data Transformation
bankdata$sing_status<-ifelse(bankdata$marital=="single",1,0)
bankdata$mar_status<-ifelse(bankdata$marital=="married",1,0)
bankdata$marital<-NULL;

bankdata$education_primary<-ifelse(bankdata$education=='primary',1,0)
bankdata$education_secondary<-ifelse(bankdata$education=='secondary',1,0)
bankdata$education_tertiary<-ifelse(bankdata$education=='tertiary',1,0)
bankdata$education<-NULL

bankdata$default_yes<-ifelse(bankdata$default=='yes',1,0)
bankdata$default<-NULL
bankdata$loan_approval<-ifelse(bankdata$loan=='yes',1,0)
bankdata$loan<-NULL
bankdata$housing_loan<-ifelse(bankdata$housing=='yes',1,0)
bankdata$housing<-NULL
bankdata$contact_cellular<-ifelse(bankdata$contact=='cellular',1,0)
bankdata$contact_telephone<-ifelse(bankdata$contact=='telephone',1,0)
bankdata$contact<-NULL
bankdata$job_status<-ifelse(bankdata$job!='unemployed',1,0)
bankdata$job<-NULL
bankdata$month<-NULL
bankdata$p_outcome_fail<-ifelse(bankdata$poutcome=='failure',1,0)
bankdata$p_outcome_success<-ifelse(bankdata$poutcome=='success',1,0)
bankdata$p_outcome_other<-ifelse(bankdata$poutcome=='other',1,0)
bankdata$poutcome<-NULL
bankdata$churn_y_deposit<-ifelse(bankdata$y=='yes',1,0)
bankdata$y<-NULL

res <-glm(churn_y_deposit~age+balance+day+duration+campaign+pdays+
            p_outcome_fail+ p_outcome_success+p_outcome_other+mar_status+sing_status+
            education_primary+education_tertiary+education_secondary+
            default_yes+loan_approval+housing_loan+contact_cellular+contact_telephone+job_status
          ,data=bankdata,family = "binomial")

summary(res)
#Check for multicollinearity
vif(res)

#sPLIT the data
spl<-sample.split(bankdata,SplitRatio = 0.7)
bankdata_training<-subset(bankdata,spl==TRUE)
bankdata_testing<-subset(bankdata,spl==FALSE)

# Model Definition

churn_logistic<-glm(churn_y_deposit~age+balance+day+duration+campaign+pdays+
           p_outcome_fail+ p_outcome_success+p_outcome_other+mar_status+sing_status+
           education_primary+education_tertiary+education_secondary+
           default_yes+loan_approval+housing_loan+contact_cellular+contact_telephone+job_status,data = bankdata_training,family = "binomial")
summary(churn_logistic)

#find the accuracy of the logistic model 
prediction_data<-predict(churn_logistic,bankdata_testing,type="response")
predicted_outcome<-ifelse(prediction_data>0.5,1,0)
churn<-data.frame(bankdata_testing,prediction_data,predicted_outcome)
write.csv(churn,"TEST_Assignnment_TA17002.csv")
tb<-table(predicted_outcome,bankdata_testing$churn_y_deposit)
acc<-sum(diag(tb))/sum(tb)
acc
miss_class<-1-acc
#ROC CURVE & performance of the model 
roc_pred<-predict(churn_logistic,bankdata_testing,type="response")
roc_obj<-prediction(roc_pred,bankdata_testing$churn_y_deposit)
roc_performance<-performance(roc_obj,"tpr","fpr")
plot(roc_performance)
abline(a=0,b=1)
accuracy<-performance(roc_obj,"auc")
accuracy

# Get TPR & FPR
tpr<-round(as.numeric(unlist(roc_performance@y.values)),4)
fpr<-round(as.numeric(unlist(roc_performance@x.values)),4)
alpha<-round(as.numeric(unlist(roc_performance@alpha.values)),4)
i<-which(round(alpha,2)==0.5)
paste("Threshold=",(alpha[i]),"TPR=",tpr[i],"FPR=",fpr[i])
