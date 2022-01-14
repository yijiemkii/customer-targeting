#"Final Project Team 29"
#"Team 29"
#"2021/10/9"

#install.packages("factoextra")
#install.packages("ggfortify")
#install.packages("psych")
#install.packages('fastDummies')
#install.packages("EnsCat")
#install.packages("klaR")
library(fastDummies)
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(GGally)
library(faraway)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggfortify)
library(caret)
library(knitr)
library(rpart.plot)
library(MASS)
library(randomForest)
library(glmnet)
library(psych)
library(EnsCat)
library(ModelMetrics)
library(ROCR)
require(klaR)
library(tree)
library(partykit)
library(data.table)
source("DataAnalyticsFunctions.R")



### Import Data
rawdata <- read.csv("WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv")



### Data Understanding

## Summary statistics
describe(rawdata)
str(rawdata, give.attr = FALSE)
num_entry <- dim(rawdata)[1]
num_col <- dim(rawdata)[2]
num_missing <- sum(complete.cases(rawdata)) - nrow(rawdata)
print(paste("Our dataset has",num_entry,"entries and", num_col, "of columns with", num_missing, "missing values"))
# Column types and numbers
numeric_col <- select_if(rawdata, is.numeric) %>% colnames()
num_numeric_col <- select_if(rawdata, is.numeric) %>% ncol()
char_col <- select_if(rawdata, is.character) %>% colnames()
num_char_col <- select_if(rawdata, is.character) %>% ncol()
# Drop Effective.To.Date
drops <- c("Customer", "Effective.To.Date")
data <- rawdata[,!(names(rawdata) %in% drops)]
# Dummy the target variable, Response
a <- sub("Yes", 1, data$Response)
data$Response <- as.numeric(sub("No", 0, a))
str(data, give.attr = FALSE)
ggplot(data, aes(x = factor(Sales.Channel), y = Response)) + 
  stat_summary(fun = "mean", geom = "bar")
ggplot(data, aes(x = factor(EmploymentStatus), y = Response)) + 
  stat_summary(fun = "mean", geom = "bar")

## Checking for multicollinearity problem
# Factor the categorical variables
factor_char_col <- c("State", "Coverage", "Education","EmploymentStatus", "Gender", "Location.Code", "Marital.Status", "Policy.Type", "Policy", "Renew.Offer.Type", "Sales.Channel", "Vehicle.Class", "Vehicle.Size")    
data[, factor_char_col] <- lapply(data[,factor_char_col], factor)
# In order to test for collinearity, temporarily change factored variables to numeric
data[, factor_char_col] <- lapply(data[,factor_char_col], as.numeric)
# Correlation plots
data[, -1] %>% select_if(is.numeric) %>% cor() %>% corrplot()
data[, c(11, 16, 17, 20)] %>% select_if(is.numeric) %>% cor() %>% corrplot(, method = "number")
data[, c(11, 16, 17, 20)] %>% select_if(is.numeric) %>% ggpairs()
# Visualization of correlations
plot(data$Monthly.Premium.Auto, data$Total.Claim.Amount)
plot(data$Policy, data$Policy.Type)
# Multicollinearity score, problematic if >5
multicol <- data[, -1] %>% select_if(is.numeric) %>% faraway::vif()
multicol
multicol_var <- multicol[multicol > 5]
multicol_var

## Checking for outliers
bp_CLV <- boxplot(data$Customer.Lifetime.Value, main = "CLV in dollars",
                  xlab = "Dollar",
                  ylab = "CLV",
                  col = "navy",
                  border = "dark grey",
                  horizontal = TRUE,
                  notch = TRUE)

length(bp_CLV$out) # 816

hist(data$Customer.Lifetime.Value, density=20, breaks=20, prob=TRUE, 
     xlab="Customer Lifetime Value", 
     main="normal curve over histogram")

m <- mean(data$Customer.Lifetime.Value)
std <- sqrt(var(data$Customer.Lifetime.Value))
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

bp_Inc <- boxplot(data$Income,main = "Income in dollars",
                  xlab = "Dollar",
                  ylab = "Income",
                  col = "navy",
                  border = "dark grey",
                  horizontal = TRUE,
                  notch = FALSE)
length(bp_Inc$out) #0

bp_MPA <- boxplot(data$Monthly.Premium.Auto,main = "Monthly Premium Auto in dollars",
                  xlab = "Dollar",
                  ylab = "Monthly Premium Auto",
                  col = "navy",
                  border = "dark grey",
                  horizontal = TRUE,
                  notch = TRUE)
length(bp_MPA$out) # 430

bp_MSLC <- boxplot(data$Months.Since.Last.Claim,main = "Months Since Last Claim",
                   xlab = "Number of months",
                   ylab = "Months Since Last Claim",
                   col = "navy",
                   border = "dark grey",
                   horizontal = TRUE,
                   notch = TRUE)
length(bp_MSLC$out) # 0

bp_MSPI <- boxplot(data$Months.Since.Policy.Inception,main = "Months Since Policy Inception",
                   xlab = "Number of months",
                   ylab = "Months Since Policy Inception",
                   col = "navy",
                   border = "dark grey",
                   horizontal = TRUE,
                   notch = TRUE)
length(bp_MSPI$out) # 0

hist_NOC <- hist(data$Number.of.Open.Complaints,main = "Number of Open Complaints",
                 xlab = "Counts",
                 ylab = "Number of Open Complaints",
                 col = "navy",
                 border = "dark grey")

bp_NOP <- boxplot(data$Number.of.Policies,main = "Number.of.Policies",
                  xlab = "Counts",
                  ylab = "Number of Policies",
                  col = "navy",
                  border = "dark grey",
                  horizontal = TRUE,
                  notch = TRUE)
length(bp_NOP$out) # 416

bp_TCA <- boxplot(data$Total.Claim.Amount,main = "Total Claim Amount in Dollars",
                  xlab = "Dollars",
                  ylab = "Total Claim Amount",
                  col = "navy",
                  border = "dark grey",
                  horizontal = TRUE,
                  notch = TRUE)
length(bp_TCA$out) # 453

hist(data$Total.Claim.Amount, density=20, breaks=20, prob=TRUE, 
     xlab="Total Claim Amount in dollars",
     main="normal curve over histogram")

m_2 <- mean(data$Total.Claim.Amount)
std_2 <- sqrt(var(data$Total.Claim.Amount))
curve(dnorm(x, mean=m_2, sd=std_2), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")



### Data Preparation
rawdata_1 <- read.csv("WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv")

## Customer and Effective.To.Date are useless and Policy.Type is subjected to collinearity problem
drops_1 <- c("Customer", "Effective.To.Date", "Policy.Type")
data_1 <- rawdata_1[,!(names(rawdata_1) %in% drops_1)]

## Dummy the Target Variable Response
a_1 <- sub("Yes", 1, data_1$Response)
data_1$Response <- as.factor(as.numeric(sub("No", 0, a_1)))

## Whole Dataset for Unsupervised Learning
data <- dummy_cols(data_1, select_columns = c("State", "Coverage", "Education","EmploymentStatus", "Gender", "Location.Code", "Marital.Status", "Policy", "Renew.Offer.Type", "Sales.Channel", "Vehicle.Class", "Vehicle.Size"),
                   remove_selected_columns = TRUE, remove_first_dummy = TRUE)
colnames(data) <- make.names(colnames(data))

## Log transformation of CLV
CLV <- data$Customer.Lifetime.Value
data$log_Customer.Lifetime.Value <- log(data$Customer.Lifetime.Value)

bp_log_CLV <- boxplot(data$log_Customer.Lifetime.Value, main = "log of Customer Lifetime Value in dollars",
                      xlab = "Dollar",
                      ylab = "log_CLV",
                      col = "navy",
                      border = "dark grey",
                      horizontal = TRUE,
                      notch = TRUE)
length(bp_log_CLV$out) # 185

hist(data$log_Customer.Lifetime.Value, density=20, breaks=20, prob=TRUE, 
     xlab="log of Customer Lifetime Value in dollars",
     main="normal curve over histogram")

m_3 <- mean(data$log_Customer.Lifetime.Value)
std_3 <- sqrt(var(data$log_Customer.Lifetime.Value))
curve(dnorm(x, mean=m_3, sd=std_3), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

## SQRT transformation of TCA
data$sqrt_Total.Claim.Amount <- sqrt(data$Total.Claim.Amount)

bp_sqrt_TCA <- boxplot(data$sqrt_Total.Claim.Amount, main = "Square Root of Total Claim Amount in dollars",
                       xlab = "Dollar",
                       ylab = "log_TCA",
                       col = "navy",
                       border = "dark grey",
                       horizontal = TRUE,
                       notch = TRUE)
length(bp_sqrt_TCA$out) # 563

hist(data$sqrt_Total.Claim.Amount, density=20, breaks=20, prob=TRUE, 
     xlab="Square Root of Total Claim Amount in dollars",
     main="normal curve over histogram")

m_4 <- mean(data$sqrt_Total.Claim.Amount)
std_4 <- sqrt(var(data$sqrt_Total.Claim.Amount))
curve(dnorm(x, mean=m_4, sd=std_4), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

## Delete the orignal two columns, 1: CLV, 9: TCA
data <- data[, -c(1,9)]



### Unsupervised: K-modes

data_2 <- data_1 %>% select_if(negate(is.numeric)) #select only categorical columns
simple_modes <- data_2[, -c(2:3, 9:11)] 
set.seed(123)

## Running the K-Modes Cluster
simple_kmodes <- kmodes(simple_modes, 6, iter.max = 10, weighted = FALSE, fast = TRUE)
simple_kmodes

## Calculate the avga of each cluster
avg_clv <- tapply(data_1$Customer.Lifetime.Value,simple_kmodes$cluster,mean)
avg_income <- tapply(data_1$Income,simple_kmodes$cluster,mean)
tapply(data_1$Monthly.Premium.Auto,simple_kmodes$cluster,mean)
tapply(data_1$Months.Since.Policy.Inception,simple_kmodes$cluster,mean)
tapply(data_1$Months.Since.Last.Claim,simple_kmodes$cluster,mean)
tapply(data_1$Number.of.Open.Complaints,simple_kmodes$cluster,mean)
tapply(data_1$Number.of.Policies,simple_kmodes$cluster,mean)
avg_tol_claim_amount <- tapply(data_1$Total.Claim.Amount,simple_kmodes$cluster,mean)

## Defining Clusters
cbind(simple_kmodes$modes, avg_clv, avg_income, avg_tol_claim_amount)
# write.csv(cbind(simple_kmodes$modes, avg_clv, avg_income, avg_tol_claim_amount), 'center_kmodes.csv')

## Add result cluster to dataset for supervised learning
data$cluster <- simple_kmodes$cluster




### Supervised

## Split Dataset for Supervised Learning
set.seed(1)
holdout.indices <- sample(nrow(data), nrow(data)*0.5)

holdout <- data[holdout.indices,]
tr_val <- data[-holdout.indices,]

#The dataset is imbalanced, but not too much. To fix this problem, we will not use accuracy as the metrics because it can be misleading when dataset is skewed.
#We will not downsampling the data because our final goal is not pure classification, but ranking.
#We choose two metrics, the first one is AUC, the second one is expect profit. Both are easy to interpret even the data is imbalanced.

## Checking how balanced they are:
mean(tr_val$Response==1)
mean(holdout$Response==1)
# imbalanced dataset 


#In surpervised model, 

#This dataset is about cross-selling. We are sending a new policy offer to our customer. The goal is to predict who is more likely to respond the offer. We also want to calculated expect value of targeting for each customer as score to generate a targeting list.

#We have following assumptions for this business problem:
  
#*   The new policy is only available via the offer.If the offer is not made to a consumer, the consumer will not buy the policy;
#*   The cost of offer is equal for all customers;
#*   If the customer respond to the offer, his/her CLV will increase by 5%.

#how to calculate benefit for targeting a customer:
#$$
#  \text { Expected benefit of targeting }=p(R \mid \mathbf{x}) \cdot v_{R}(\mathbf{x})+[1-p(R \mid \mathbf{x})] \cdot v_{N R}(\mathbf{x})
#$$
#  $p(R \mid \mathbf{x})$ is the probability of response given consumer x, vR is the value we get from a response, and vNR is the value we get from no response. Under our assumption, 
#vr = CLV - COST
#vnr = - COST
#p can be predicted by mining historical data

#Then we get the following formula

#$$
#  \text { Expected benefit of targeting }=p(R \mid \mathbf{x}) \cdot [5\% CLV(\mathbf{x})-Cost]-[1-p(R \mid \mathbf{x})] \cdot Cost
#$$
  
#  At first we use AUC as the metric, but since our goal is maximizing profit, we then built this metric 'profit' to estimate the profit generated by targeting

#Once we accept the assumption, we can build the cost benefit matrix

#Resonse             Not response
#Target      0.05*CLV(X)-70     -70               
#not target   0                  0

#Since every customer has different CLV, the cost benefit matrix is different for each individual.
#The profit for each customer can be calculated by the confusion matrix and cost benefit matrix, the total profit is the sum of all individual profit.

#score by p_response * clv
R1maxprofitmeasure <- function(p,y, lift=0.05, cost=70, CLV = exp(holdout$log_Customer.Lifetime.Value),...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  CLV <- CLV
  Egain <- lift*p*CLV - cost
  nn <- sum(Egain>0)
  #p<- p[Egain>0]
  #y <- y[Egain>0]
  #CLV <- CLV[Egain>0]
  pp <- p[order(p*CLV, decreasing =TRUE, na.last = NA)]
  yy <- y[order(p*CLV, decreasing =TRUE, na.last = NA)]
  CLVV <- CLV[order(p*CLV, decreasing =TRUE, na.last = NA)]
  VV1<-(yy==levels(yy)[1])
  VV2<-(yy==levels(yy)[2])
  profit <- rep.int(0,n)
  for ( kk in 1:n ){
    profit[kk] <- lift*sum(CLVV[which(VV2[1:kk])]) -cost* kk
  }
  optimizedprofit<- lift*sum(CLVV[which(VV2[1:nn])]) -cost* nn
  #plot(seq(from=1,to=n,by=1)/n, profit, type="l", xlab="Proportion of data", ylab="Profit", main="Profit Curve", ...)
  #lines(seq(from=1,to=n,by=1)/n, profit, ...)
  return (list('profit' = profit, "optimizedprofit" = optimizedprofit, 'prop' = nn/n))
}

#score by p_response
R2maxprofitmeasure <- function(p,y, top = 0, lift=0.05, cost=70 ,CLV = exp(holdout$log_Customer.Lifetime.Value),...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  pp <- p[order(p, decreasing =TRUE, na.last = NA)]
  yy <- y[order(p, decreasing =TRUE, na.last = NA)]
  CLV <- as.vector(CLV)[order(p, decreasing =TRUE, na.last = NA)]
  profit <- rep.int(0,n)
  VV1<-(yy==levels(yy)[1])
  VV2<-(yy==levels(yy)[2])
  profit <- rep.int(0,n)
  for ( kk in 1:n ){
    profit[kk] <- lift*sum(CLV[which(VV2[1:kk])]) -cost*kk
  }
  maxprofit <- max(profit)
  prop <- which.max(profit)/n
  optimizedprofit <- profit[as.integer(top*n)]
  #plot(seq(from=1,to=n,by=1)/n, profit, type="l", xlab="Proportion of data", ylab="Profit", main="Profit Curve", ...)
  return (list("profit" = profit, "maxprofit" = maxprofit, "prop" = prop, "optimizedprofit" = optimizedprofit))
}

## Classification
# Since Income and Total.Claim.Amount are significant from the base model, we built interaction base on them when training lasso. We only keep post lasso because post lasso have better performance than lasso.
# The model including logistic regression, classification tree, random forest, three types of post lasso, and null model.

Mx<- model.matrix(Response ~ .+Income*.+sqrt_Total.Claim.Amount*., data=tr_val)[,-1]
My<- tr_val$Response
Mx_test <- model.matrix(Response ~ .+Income*.+sqrt_Total.Claim.Amount*., data=holdout)[,-1]
My_test <- holdout$Response

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.Response <- sum(My == 1)
w <- (num.Response/num.n)*(1-(num.Response/num.n))
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)

lasso <- glmnet(Mx,My, family="binomial")
lassoCV <- cv.glmnet(Mx,My, family="binomial")
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)

# Selected features for post lasso model
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
#features.min
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
#features.1se
length(features.1se)
features.theory <- support(lassoTheory$beta)
#features.theory
length(features.theory)

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
data.theory <- data.frame(Mx[,features.theory],My)

data.min_test <- data.frame(Mx_test[,features.min],My_test)
data.1se_test <- data.frame(Mx_test[,features.1se],My_test)
data.theory_test <- data.frame(Mx_test[,features.theory],My_test)

# Create empty metric tables OOS AUC
n <- nrow(tr_val)
nfold <- 10
set.seed(4)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

AUC <- data.frame(reg=rep(NA,nfold), tree =rep(NA,nfold), RF=rep(NA,nfold),null=rep(NA,nfold)) 
PL.AUC <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 
L.AUC <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold)) 

profitR1 <- data.frame(reg=rep(NA,nfold), tree =rep(NA,nfold), RF=rep(NA,nfold),null=rep(NA,nfold)) 
PL.profitR1 <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 

profitR2 <- data.frame(reg=rep(NA,nfold), tree =rep(NA,nfold), RF=rep(NA,nfold),null=rep(NA,nfold)) 
PL.profitR2 <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 

# Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold k
  
  # fit multiple linear regression, random forest, and null model
  model.reg <-glm(Response ~., 
                  data = tr_val, subset=train, family = "binomial")
  model.tree <- tree(Response ~.,  
                     data = tr_val[train,])
  model.RF <-randomForest(tr_val[train,-1], tr_val$Response[train], mtry=sqrt(49), ntree=500)
  model.nulll <-glm(Response ~ 1, data = tr_val, subset=train, family = "binomial")
  
  # get predictions
  pred.reg <- predict(model.reg, newdata=tr_val[-train,], type = 'response')
  pred.tree <- predict(model.tree, newdata=tr_val[-train,])[,2]
  pred.RF <- predict(model.RF, newdata=tr_val[-train,-1], type = 'prob')[,2]
  pred.null <- predict(model.nulll, newdata=tr_val[-train,], type = 'response')
  
  # calculate metric: AUC
  AUC$reg[k] <- auc(My[-train], pred.reg, quiet = T)
  AUC$tree[k] <- auc(My[-train], pred.tree, quiet = T)
  AUC$RF[k] <- auc(My[-train], pred.RF, quiet = T)
  AUC$null[k] <- auc(My[-train], pred.null, quiet = T)
  
  # profit metric
  profitR1$reg[k] <- R1maxprofitmeasure(pred.reg, My[-train], 
                                        CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$optimizedprofit
  profitR1$tree[k] <- R1maxprofitmeasure(pred.tree, My[-train], 
                                         CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$optimizedprofit
  profitR1$RF[k] <- R1maxprofitmeasure(pred.RF, My[-train], 
                                       CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$optimizedprofit
  profitR1$null[k] <- R1maxprofitmeasure(pred.null, My[-train], 
                                         CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$optimizedprofit
  
  profitR2$reg[k] <- R2maxprofitmeasure(pred.reg, My[-train], 
                                        CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]),
                                        top = R2maxprofitmeasure(
                                          predict(model.reg, newdata=tr_val[train,], type = 'response'),
                                          My[train],
                                          CLV = exp(Mx[train,which(colnames(Mx)=='log_Customer.Lifetime.Value')])
                                        )$prop)$optimizedprofit
  profitR2$tree[k] <- R2maxprofitmeasure(pred.tree, My[-train], 
                                         CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]),
                                         top = R2maxprofitmeasure(
                                           predict(model.tree, newdata=tr_val[train,])[,2],
                                           My[train],
                                           CLV = exp(Mx[train,which(colnames(Mx)=='log_Customer.Lifetime.Value')])
                                         )$prop)$optimizedprofit
  profitR2$RF[k] <- R2maxprofitmeasure(pred.RF, My[-train], 
                                       CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]),
                                       top = R2maxprofitmeasure(
                                         predict(model.RF, newdata=tr_val[train,], type = 'prob')[,2],
                                         My[train],
                                         CLV = exp(Mx[train,which(colnames(Mx)=='log_Customer.Lifetime.Value')])
                                       )$prop)$optimizedprofit
  profitR2$null[k] <- R2maxprofitmeasure(pred.null, My[-train], 
                                         CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]),
                                         top = R2maxprofitmeasure(
                                           predict(model.nulll, newdata=tr_val[train,], type = 'response'),
                                           My[train],
                                           CLV = exp(Mx[train,which(colnames(Mx)=='log_Customer.Lifetime.Value')])
                                         )$prop)$optimizedprofit
  
  # Using Post Lasso to improve multiple linear regression
  rmin <- glm(My~., data=data.min, subset=train, family="binomial")
  if ( length(features.1se) == 0){  r1se <- glm(Response~1, data=data.min, subset=train, family="binomial") 
  } else {r1se <- glm(My~., data=data.1se, subset=train, family="binomial")
  }
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Response~1, data=data.min, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=data.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  predtheory <- predict(rtheory, newdata=data.theory[-train,], type="response")
  PL.AUC$PL.min[k] <- auc(My[-train], predmin, quiet = T)
  PL.AUC$PL.1se[k] <- auc(My[-train], pred1se, quiet = T)
  PL.AUC$PL.theory[k] <- auc(My[-train], predtheory, quiet = T)
  
  PL.profitR1$PL.min[k] <- R1maxprofitmeasure(predmin, My[-train], 
                                              CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$optimizedprofit
  PL.profitR1$PL.1se[k] <- R1maxprofitmeasure(pred1se, My[-train], 
                                              CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$optimizedprofit
  PL.profitR1$PL.theory[k] <- R1maxprofitmeasure(predtheory, My[-train], 
                                                 CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$optimizedprofit
  
  PL.profitR2$PL.min[k] <- R2maxprofitmeasure(predmin, My[-train], 
                                              CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]),
                                              top = R2maxprofitmeasure(
                                                predict(rmin, newdata=data.min[train,], type = 'response'),
                                                My[train],
                                                CLV = exp(Mx[train,which(colnames(Mx)=='log_Customer.Lifetime.Value')])
                                              )$prop)$optimizedprofit
  PL.profitR2$PL.1se[k] <- R2maxprofitmeasure(pred1se, My[-train], 
                                              CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]),
                                              top = R2maxprofitmeasure(
                                                predict(r1se, newdata=data.1se[train,], type="response"),
                                                My[train],
                                                CLV = exp(Mx[train,which(colnames(Mx)=='log_Customer.Lifetime.Value')])
                                              )$prop)$optimizedprofit
  PL.profitR2$PL.theory[k] <- R2maxprofitmeasure(predtheory, My[-train], 
                                                 CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]),
                                                 top = R2maxprofitmeasure(
                                                   predict(rtheory, newdata=data.theory[train,], type="response"),
                                                   My[train],
                                                   CLV = exp(Mx[train,which(colnames(Mx)=='log_Customer.Lifetime.Value')])
                                                 )$prop)$optimizedprofit
  print(paste("Iteration",k,"of",nfold,"completed"))
}

# OOS AUC
AUCperformance <- cbind(PL.AUC,AUC)
colMeans(AUCperformance)
#par( mar=  c(8, 4, 4, 2) + 0.6 )
barplot(sort(colMeans(AUCperformance), decreasing = T), las=2,xpd=FALSE, ylim=c(0,1) , xlab="", ylab = bquote( "Average Out of Sample " ~ AUC))

# OOS Profit R1
profit1performance <- cbind(PL.profitR1,profitR1)
colMeans(profit1performance)
#par( mar=  c(8, 4, 4, 2) + 0.6 )
barplot(sort(colMeans(profit1performance), decreasing = T), las=2,xpd=FALSE, xlab="", ylab = bquote( "Average Out of Sample " ~ Rank1profit))

# OOS Profit R2
profit2performance <- cbind(PL.profitR2,profitR2)
colMeans(profit2performance)
barplot(sort(colMeans(profit2performance), decreasing = T), las=2,xpd=FALSE, xlab="", ylab = bquote( "Average Out of Sample " ~ Rank2profit))

# Comprehensive metric table
metric <- as.data.frame(cbind(colMeans(AUCperformance), colMeans(profit1performance), colMeans(profit2performance)))
colnames(metric) = c('AUC', 'Rank1Profit', 'Rank2profit')
metric
# fwrite(metric, 'metric.csv', row.names = TRUE)


#We will try to interpret the result of RF, it can be challenging because machine learning is blackbox
#cluster is important!

importance(model.RF)[order(importance(model.RF), decreasing = T),,drop = FALSE]

#Feature importance: For categorical, log_Customer.Lifetime.Value, sqrt_Total.Claim.Amount and Months.Since.Policy.Inception etc. have significant impact. For categorical, EmploymentStatus, Marital and cluster determined by K-modes are important. We will not look into Offer.Type here as mentioned before since we don't know how different offer is distributed.

model.PLMin <-glm(My ~.,data = data.min, family = "binomial")
pred <- predict(model.PLMin, newdata=data.min_test, type = 'response')
auc(holdout$Response, pred)
pred <- prediction(pred, holdout$Response)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


## Ranking
#A ranking classifier, we can produce a list of instances and their predicted scores, ranked by decreasing score, and then measure the expected profit that would result from choosing each successive cut-point in the list. Provost, Foster; Fawcett, Tom. Data Science for Business (p. 270). O'Reilly Media. 
#There two part of ranking, the first one is how to design score; the second one is the proportion of targeting, how many customers we want to target.
#We design score in two different way, the first one is the prob predicted by our model. prob higher, rank higher; the second score is calculated by 

#$$
#  p(R \mid \mathbf{x}) \cdot CLV(\mathbf{x})
#$$
  
#The logic behind: this is part of the expecting profit for each individual; for some customer with high CLV, even his probability of responding is low, we still want to make sure he will be targeted, for some low value customer, we don't care that much.
#Function to generate rank list and profit plot:

profit1.RF <- R1maxprofitmeasure(pred.RF, My[-train], 
                                        CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$profit
profit2.RF <- R2maxprofitmeasure(pred.RF, My[-train], 
                                        CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$profit
profit1.1se <- R1maxprofitmeasure(pred1se, My[-train], 
                                        CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$profit
profit2.1se <- R2maxprofitmeasure(pred1se, My[-train], 
                                        CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$profit

plot(seq(from=1,to=length(pred.RF),by=1)/length(pred.RF), profit2.RF, type="l", lty = 2, xlab="Proportion of data", ylab="Profit", main="Profit Curve", col = "#2166AC")
lines(seq(from=1,to=length(pred.RF),by=1)/length(pred.RF), profit1.RF,  type="l", col ="#2166AC")
lines(seq(from=1,to=length(pred.RF),by=1)/length(pred.RF), profit1.1se,  type="l", col = "#B2182B")
lines(seq(from=1,to=length(pred.RF),by=1)/length(pred.RF), profit2.1se,  type="l", lty = 2, col = "#B2182B")
abline(v = R2maxprofitmeasure(pred.RF, My[-train], 
                                        CLV = exp(Mx[-train,which(colnames(Mx)=='log_Customer.Lifetime.Value')]))$prop)#0.15
abline(v = 0.035, lty = 2)
axis(1, at=0.1476,labels=.15)
axis(1, at = 0.035, labels = .035)
legend("topright",legend=c("Randomforest p*CLV score", "Randomforest p score", "PLasso p*CLV score", "PLasso p score"),
       col=c("#2166AC", "#2166AC", "#B2182B", "#B2182B"), lty=1:2, cex=0.8)

#Here the solid blue and red line represent using the first ranking method, the dashed ones are the performance of second ranking method. 

#We can clearly see the dashed blue line is the best, the peak point is higher than that of the solid one, which means if the company could have developed a great classifier, results from the model itself would suffice.

#However, for the red one, where the classifier's performance alone is not that good, the company might as well taking into consideration the CLV to generate better prediction since the peak point for the red one is on the line of the first ranking method.

#Another insight is that during the upward slope part of the curve, the performance starts with the solid blue line being better than the dashed one, this tells us that if the company has a budget limit for a specific targeting campaign where they might not be able to reach the proportion where the peak point located, they should choos the first ranking method so as to get a better prediction.



