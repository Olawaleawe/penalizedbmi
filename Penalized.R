library(tidyverse)
library(caret)	
library(glmnet)
library(mlbench)
# Load the data
aneM = read.csv('SANHANES_3.csv')
aneM
dim(aneM)
names(aneM)
attach(aneM)
bmi = bmi*100
bmi
plot(bmi, type = 'l')
age


# Split the data into training and test set
set.seed(123)
training.samples <- aneM$bmi %>%
  createDataPartition(p = 0.75, list = FALSE)
train  <- aneM[training.samples, ]
test <- aneM[-training.samples, ]
lambda <- 10^seq(-3, 3, length = 100)

# Build the ridge regression model
set.seed(123)
ridge <- train(bmi ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda))
summary(ridge)
# Model coefficients
k=coef(ridge$finalModel, ridge$bestTune$lambda)
plot(k, type='l')
# Make predictions
predictions <- ridge %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$bmi),
  Rsquare = R2(predictions, test.data$bmi)
)
plot(ridge)
# Build the LASSO regression model
set.seed(123)
lasso <- train(
  bmi ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Model coefficients
K=coef(lasso$finalModel, lasso$bestTune$lambda)
plot(K, type='l')
# Make predictions
predictions <- lasso %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$bmi),
  Rsquare = R2(predictions, test.data$bmi)
)
plot(lasso)


# Build the ELASTIC NET model
set.seed(123)
elastic <- train(bmi ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Model coefficients
kk=coef(elastic$finalModel, elastic$bestTune$lambda)
plot(kk, type='l')
# Make predictions
predictions <- elastic %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$bmi),
  Rsquare = R2(predictions, test.data$bmi)
)

plot(elastic)
# Compare Models
models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
rep1 = resamples(models) %>% summary( metric = "RMSE")

##################################################################################################
#########################################################################################################
###########################################################################################
####################################################
# Prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)

####################################################################
# Robust Linear model
set.seed(7)
modelrlm <- train(bmi ~., data = train.data, method="rlm", trControl=control, tuneLength = 10)
modelrlm
modelrlm$finalModel
plot(varImp(modelrlm, scale=T))
#########################################################################

###############################################################################
#######Bayesian Ridge Regression
set.seed(7)
modelbridge<- train(bmi~., data=train.data, method="bridge", trControl=control, tuneLength = 10)
modelbridge
plot(varImp(modelbridge, scale=T))
#############################################################################
###Elastic Net
#############################################################################
set.seed(7)
modelenet <- train(bmi~., data=train.data, method="enet", trControl=control, tuneLength = 10)
modelenet
plot(varImp(modelenet, scale=T))
############################################################################
###########Ridge Regression
set.seed(7)
modelridge <- train(bmi~., data=train.data, method="ridge", trControl=control, tuneLength = 10)
modelridge
plot(varImp(modelridge, scale=T))
##########################################################################
###Random Forest
############################################################################
set.seed(7)
modelplr <- train(bmi~., data=train.data, method="penalized", trControl=control, tuneLength = 10)
modelplr$results
modelplr
plot(varImp(modelplr, scale=T))
############################################################################
####################Bayesian LASSO###########################################
set.seed(7)
modelblasso <- train(bmi~., data=train.data, method="blasso", trControl=control, tuneLength = 10)
modelblasso
plot(varImp(modelblasso, scale=T))
############################################################################
###LASSO
set.seed(7)
modellasso <- train(bmi~., data=train.data, method="lasso", trControl=control, tuneLength = 10)
plot(modellasso)
plot(varImp(modellasso, scale=T))

############################################################################
# collect resamples and list the models here. Use only 10 models
results <- resamples(list(Lasso=modellasso, Bridge=modelbridge, Enet=modelenet, RLM=modelrlm,Blasso=modelblasso,PLR=modelplr,Ridge=modelridge))

# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

#In this chapter we described the most commonly used penalized regression methods
#including ridge regression, lasso regression and elastic net regression. 
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
# Load the data
aneW = read.csv('womanemi.csv')
aneW
dim(aneW)
names(aneW)
attach(aneW)
anemia_bin
bmi = bmi/100
bmi
plot(bmi, type = 'l')
age

##############################################################
# Split the data into training and test set

set.seed(123)
ind <- sample(2, nrow(aneW), replace = TRUE, prob = c(0.7, 0.3))
train <- aneW[ind==1,]
test <- aneW[ind==2,]
lambda <- 10^seq(-3, 3, length = 100)
# Prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)

####################################################################
# Robust Linear model
set.seed(7)
modelrlm1 <- train(bmi ~., data = train, method="rlm", trControl=control, tuneLength = 10)
modelrlm1
modelrlm1$finalModel
plot(varImp(modelrlm1, scale=T))
#########################################################################

###############################################################################
#######Bayesian Ridge Regression
set.seed(7)
modelbridge1 <- train(bmi~., data=train, method="bridge", trControl=control, tuneLength = 10)
modelbridge1
plot(varImp(modelbridge1, scale=T))
#############################################################################
###Elastic Net
#############################################################################
set.seed(7)
modelenet1 <- train(bmi~., data=train, method="enet", trControl=control, tuneLength = 10)
modelenet1
plot(varImp(modelenet1, scale=T))
############################################################################
###########Ridge Regression
set.seed(7)
modelridge1 <- train(bmi~., data=train, method="ridge", trControl=control, tuneLength = 10)
modelridge1
plot(varImp(modelridge1, scale=T))
##########################################################################
###Random Forest
############################################################################
set.seed(7)
modelplr1 <- train(bmi~., data=train, method="penalized", trControl=control, tuneLength = 10)
modelplr1$results
modelplr1
plot(varImp(modelplr1, scale=T))
############################################################################
####################Bayesian LASSO###########################################
set.seed(7)
modelblasso1 <- train(bmi~., data=train, method="blasso", trControl=control, tuneLength = 10)
modelblasso1
plot(varImp(modelblasso1, scale=T), n.var=5)
############################################################################
###LASSO
set.seed(7)
modellasso1 <- train(bmi~., data=train, method="lasso", trControl=control, tuneLength = 10)
plot(modellasso1)
plot(varImp(modellasso1, scale=T))

############################################################################
# collect resamples and list the models here. Use only 10 models
results <- resamples(list(Lasso=modellasso1, Bridge=modelbridge1, Enet=modelenet1, RLM=modelrlm1,Blasso=modelblasso1,PLR=modelplr1,Ridge=modelridge1))

# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
#These methods are very useful in a situation, where you have a large multivariate data sets.

######################################################################################
#####################################################################################
######################################################################################
odata2 = read.csv("SANHANES_3.csv", header = TRUE)
odata2
dim(odata2)
names(odata2)
attach(odata2)
summary(odata2)
c=odata2$bmi
plot(c, type='l', col='blue', ylab='Body Mass Index', xlab='Respondents')
abline(h=mean(c))
mean(c)


#nzv1=nearZeroVar(odata2)
#odata2=odata3[,-nzv1]
#odata2


#datac= cor(odata2)
#datac
#hdata=findCorrelation(datac,cutoff=0.75)
#hdata
#odata2= odata2[, -hdata]
#odata2
#################################################################################
#################################################################################
set.seed(123)
ind <- sample(2, nrow(odata2), replace = TRUE, prob = c(0.8, 0.2))
train1 <- odata2[ind==1,]
test1 <- odata2[ind==2,]
#lambda <- 10^seq(-3, 3, length = 100)
# Prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)

####################################################################
# Robust Linear model
#set.seed(7)
modelrlm1 <- train(bmi ~., data = train1, method="rlm", trControl=control, preprocess=c('center', 'scale'),tuneLength = 10)
modelrlm1
modelrlm1$finalModel
plot(varImp(modelrlm1, scale=F))
###############################################################################

###############################################################################
#######Bayesian Ridge Regression
set.seed(7)
modelbridge1 <- train(bmi~., data=train1, method="bridge", preProcess= c('center','scale'), trControl=control, tuneLength = 10)
modelbridge1
plot(varImp(modelbridge1, scale=F))
#############################################################################
###Elastic Net
#############################################################################
set.seed(7)
modelenet1 <- train(bmi~., data=train1, method="enet", preProcess= c('center','scale'), trControl=control, tuneLength = 10)
modelenet1
plot(varImp(modelenet1, scale=F))
############################################################################
###########Ridge Regression
set.seed(7)
modelridge1 <- train(bmi~., data=train1, method="ridge", preProcess= c('center','scale'), trControl=control, tuneLength = 10)
modelridge1
plot(varImp(modelridge1, scale=T))
##########################################################################
###Random Forest
############################################################################
set.seed(7)
modellm1 <- train(bmi~., data=train, method="lm", trControl=control, tuneLength = 10)
modellm1$results
modellm1
plot(varImp(modellm1, scale=T))
############################################################################
####################Bayesian LASSO###########################################
set.seed(7)
modelblasso1 <- train(bmi~., data=train1, method="blasso", preProcess= c('center','scale'), trControl=control, tuneLength = 10)
modelblasso1
plot(varImp(modelblasso1, scale=T))
############################################################################
###LASSO
set.seed(7)
modellasso1 <- train(bmi~., data=train1, method="lasso",preProcess= c('center','scale'), trControl=control, tuneLength = 10)
plot(modellasso1)
plot(varImp(modellasso1, scale=T))

############################################################################
# collect resamples and list the models here. Use only 10 models
results1 <- resamples(list(LASSO=modellasso1, Bridge=modelbridge1, ENET=modelenet1,RLM= modelrlm1, Blasso=modelblasso1,Ridge=modelridge1))

# summarize the distributions
summary(results1)
# boxplots of results
#bwplot(results1)
# dot plots of results

#These methods are very useful in a situation, where you have a large multivariate data sets.
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results1, scales=scales, metric='RMSE')
dotplot(results1, scales=scales )

###########################################################################
###########################################################################
###########################################################################
#BMI SURVEY
######################################################################################
odata3 = read.csv("BMI-Data.csv", header = TRUE)
odata3
dim(odata3)
names(odata3)
attach(odata3)
summary(odata3)


BMI
k=odata3$BMI
plot(k, type='l', col='blue', ylab='Body Mass Index', xlab='Respondents')
abline(h=mean(k))
mean(k)

odata3= odata3[, -c(5,6,12,16,18)]
#nzv=nearZeroVar(odata3)
#odata3=odata3[,-nzv]
#odata3
#################################################################################
#################################################################################
set.seed(123)
ind <- sample(2, nrow(odata3), replace = TRUE, prob = c(0.8, 0.2))
train2 <- odata3[ind==1,]
test2 <- odata3[ind==2,]
#lambda <- 10^seq(-3, 3, length = 100)
# Prepare training scheme

control <- trainControl(method="repeatedcv", number=10, repeats=5)

####################################################################
# Robust Linear model
set.seed(7)
modelrlm2 <- train(BMI ~., data = train2, method="rlm", trControl=control, tuneLength = 10)
modelrlm2
modelrlm1$finalMode2
plot(varImp(modelrlm2, scale=T))
#########################################################################

###############################################################################
#######Bayesian Ridge Regression
set.seed(7)
modelbridge2 <- train(BMI ~., data=train2, method="bridge", trControl=control, preProcess= c('center','scale'),tuneLength = 10)
modelbridge2
#plot(modelbridge1)
plot(varImp(modelbridge2, scale=T))
#############################################################################
###Elastic Net
#############################################################################
set.seed(7)
modelenet2 <- train(BMI ~., data=train2, method="enet", trControl=control, preProcess= c('center','scale'), tuneLength = 10)
modelenet2
plot(modelenet2)
plot(varImp(modelenet2, scale=T))
############################################################################
###########Ridge Regression
set.seed(7)
modelridge2 <- train(BMI ~., data=train2, method="ridge", trControl=control, preProcess= c('center','scale'), tuneLength = 10)
modelridge2
plot(varImp(modelridge2, scale=T))
##########################################################################
###LM
############################################################################
set.seed(7)
modellm2 <- train(bmi~., data=train, method="lm", trControl=control, tuneLength = 10)
modellm2$results
modellm2
plot(varImp(modellm2, scale=T))
############################################################################
####################Bayesian LASSO###########################################
set.seed(7)
modelblasso2 <- train(BMI~., data=train2, method="blasso", trControl=control, preProcess= c('center','scale'), tuneLength = 10)
modelblasso2
plot(varImp(modelblasso2, scale=T))
############################################################################
###LASSO
set.seed(7)
modellasso2 <- train(BMI~., data=train2, method="lasso", trControl=control, preProcess= c('center','scale'), tuneLength = 10)
plot(modellasso2)
plot(varImp(modellasso2, scale=T))
#varImpPlot(modellasso2, n.var = 5)
#varImpPlot(tuned_model$finalModel, n.var = 5)
############################################################################
# collect resamples and list the models here. Use only 10 models
results2 <- resamples(list(LASSO=modellasso2, Bridge=modelbridge2, ENET=modelenet2, RLM= modelrlm2, Blasso=modelblasso2,Ridge=modelridge2))
scales <- list(x=list(relation="free"), y=list(relation="free"))
# summarize the distributions
summary(results2)
# boxplots of results
bwplot(results2, scales=scales)
# dot plots of results
dotplot(results2, scales=scales, metric='MAE')
densityplot(results2, scales=scales)
#These methods are very useful in a situation, where you have a large multivariate data sets.
#######################################


par(mfrow=c(1,2))
dotplot(results1, metric='RMSE', main='RMSE 1')
dotplot(results2, metric='RMSE')


bwplot(results1, metric='RMSE', main='RMSE 1', col=rainbow(6))
bwplot(results2, metric='RMSE', main='RMSE 2', col=rainbow(6))



c1=c(3.719, 3.708, 3.719, 3.736, 3.706, 3.770)
c2=c(4.632, 4.867, 4.626, 4.804, 4.820, 4.839)
plot(c1, type='l', ylim=c(3,5))
lines(c2)
