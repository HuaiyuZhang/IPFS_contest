#################################################################
# IPFS competition code                                       
#----------------------------------------------------------------
# Goal: xgboost tuning                                     
# Author: Huaiyu Zhang                                        
# Created: 4/7/2019    
#----------------------------------------------------------------
# Description: 
# 1. Import the candidate feature list done by rough selecting
# 2. Use MLR pacakge to run a granular tuning and then fine tune
# 3. Fit a final model
#################################################################
rm(list= ls())
require(xgboost)
setwd('C:\\Dropbox\\ipfs\\data')
# The sparse matrix includes the categorical variables
dat_all <- readRDS('dat_all.RDS')
dat_train <- dat_all[dat_all[ ,30] == 0,]
dat_train <- dat_train[, -30]

var_list <- colnames(dat_train)
label <- dat_train[,1]
dat_train <- dat_train[, -1] 
candidate_var_list <- readRDS('../output/candidate_var_list')
features <- dat_train[, colnames(dat_train) %in% candidate_var_list]

#------------------------ Train-test split ------------------------
indices <- 1:nrow(features)
set.seed(2017)
train_index <- sample(indices, round(0.7*length(indices),0), replace = F)
test_index <- indices[-train_index]
X_train <- features[train_index,]; y_train <- label[train_index]
X_test <- features[test_index,]; y_test <- label[test_index]
# mlr only accepts dataframe
df <- data.frame(as.matrix(X_train))
df$label <- as.factor(y_train)
#---------------------- Tuning --------------------------------
# library('parallel')
# library('parallelMap')
# parallelStartSocket(cpus = detectCores())
library(mlr)
# https://mlr.mlr-org.com/articles/tutorial/tune.html
traintask <- makeClassifTask (data = df,target = "label")
lrn <- makeLearner("classif.xgboost",predict.type = "prob")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="auc")
params <- makeParamSet(
                        makeIntegerParam("nrounds", lower = 1, upper =100),
                       makeIntegerParam("max_depth", lower = 3, upper = 15),
                       makeNumericParam("eta", lower = .1, upper = .5),
                        makeNumericParam("lambda", lower = -1, upper = 2, trafo = function(x) 10^x)
                       )
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
ctrl <- makeTuneControlRandom(maxit = 100)
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, 
                     measures = auc, par.set = params, control = ctrl)
saveRDS(mytune, 'tune_0413.RDS')
dim(df)
# Tune result:
#   Op. pars: nrounds=65; max_depth=15; eta=0.282; lambda=50.6
# auc.test.mean=0.9918165

# Tuning result
mytune$x
mytune$y
# generateHyperParsEffectData(mytune, trafo = TRUE,partial.dep = T)

# Final hyperparameter setting


#----------------- Fit the final model-------------------------
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
xgb_model_final <- xgb.train(data = dtrain,
                         max.depth = 15, 
                         eta = 0.282,
                         nrounds = 65,
                         lambda=50.6,
                         objective = "binary:logistic",
                         eval.metric = 'auc',
                         verbose = 1
)


get_top_features <- function(xgbModel, k = 10){
  importance_matrix <- xgb.importance(model = xgbModel)
  return(importance_matrix$Feature[1:k])
}
get_top_features(xgb_model_final, 20)

get_recall <- function(truth, pred){
  sum(truth == 1 & pred == 1)/sum(truth == 1)
}
get_precision <- function(truth, pred){
  sum(truth == 1 & pred == 1)/sum(pred == 1)
}

get_evaluation <- function(xgbModel, dtest, threshold = 0.5){
  pred <- predict(xgbModel, dtest)
  y_pred <- as.numeric(pred > threshold)
  y_test <- getinfo(dtest,'label')
  err <- mean(y_pred != y_test)
  recall <- get_recall(truth = y_test,pred = y_pred)
  precision <- get_precision(truth = y_test,pred = y_pred)
  library(pROC)
  auc <- round(as.numeric(roc( y_test, pred, algorithm = 2)$auc), 4)
  print(paste('AUC =', auc) )
  print(paste("test-error =", round(err,4)))
  print(paste('test-recall =', round(recall,4) ))
  print(paste('test-precision =', round(precision,4) ))
}
dtest <- xgb.DMatrix(data = X_test, label = y_test)
get_evaluation(xgb_model_final,dtest)
# 
# [1] "AUC = 0.9926"
# [1] "test-error = 0.0239"
# [1] "test-recall = 0.9071"
# [1] "test-precision = 0.8449"

threshold_list <- seq(0.2, 0.4, 0.01)
for (i in 1:length(threshold_list)){
  print( threshold_list[i])
  get_evaluation(xgb_model_final, dtest, threshold_list[i])
}







