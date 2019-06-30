#################################################################
# IPFS competition code                                       
#----------------------------------------------------------------
# Goal: xgboost modeling 1                                     
# Author: Huaiyu Zhang                                        
# Created: 4/1/2019    
#----------------------------------------------------------------
# Description: 
# Write a set of functions to evaluate the model.
# 
#################################################################
rm(list= ls())
require(xgboost)
setwd('C:\\Dropbox\\ipfs')
# The sparse matrix includes the categorical variables
dat_all <- readRDS('dat_all.RDS')
dat_train <- dat_all[dat_all[ ,30] == 0,]
dat_test <- dat_all[dat_all[ ,30] == 1,]
dat_train <- dat_train[, -30]
dat_test <- dat_test[, -30]

var_list <- colnames(dat_train)
continuous_var_list <- var_list[2:36]
categorical_var_list <- var_list[37: length(var_list)]

label <- dat_train[,1]
dat_train <- dat_train[, -1] 
dtrain <- xgb.DMatrix(data = dat_train, label = label)
watchlist <- list(train = dtrain)

#-------------------------- Modeling ------------------------------
xgb_model_1 <- xgb.train(data = dtrain,
                       max.depth = 6, 
                       eta = 1,
                       subsample = 0.7,
                       nrounds = 10,
                       objective = "binary:logistic",
                       eval.metric = 'auc',
                       verbose = 1,
                       watchlist = watchlist
                )

xgb_model_2 <- xgb.train(data = dtrain,
                         max.depth = 4, 
                         eta = 0.3,
                         gamma = 1,
                         # subsample = 0.7,
                         nrounds = 50,
                         objective = "binary:logistic",
                         eval.metric = 'auc',
                         verbose = 1,
                         watchlist = watchlist
                )

xgb_model_3 <- xgb.train(data = dtrain,
                         max.depth = 13, 
                         eta = 0.7,
                         gamma = 1,
                         subsample = 0.9,
                         nrounds = 50,
                         objective = "binary:logistic",
                         eval.metric = 'auc',
                         verbose = 1,
                         watchlist = watchlist
)


#----------- Result exploration -------------------

# Top features
get_top_features <- function(xgbModel, k = 10){
  importance_matrix <- xgb.importance(model = xgbModel)
  return(importance_matrix$Feature[1:k])
}

feature_set_1 <- get_top_features(xgb_model_1,100)
feature_set_2 <- get_top_features(xgb_model_2,100)
feature_set_3 <- get_top_features(xgb_model_3,100)
feature_set <- union(union(feature_set_1, feature_set_2), feature_set_3)
# Use the top features to construct a candidate feature set.
useful_categorical_feature_list <- intersect(feature_set, categorical_var_list)
candidate_var_list <- union(useful_categorical_feature_list, continuous_var_list)
saveRDS(candidate_var_list, '../output/candidate_var_list')

# print out the variables
for ( i in 1:length(candidate_var_list)){
  cat('"', candidate_var_list[i], '", ', sep = '' )
}

#-------------------------------------------------------------------------------
# Evaluation
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

label_test <- dat_test[,1]
dat_test<- dat_test[, -1] 
dtest <- xgb.DMatrix(data = dat_test, label = label_test)
get_evaluation(xgb_model_1, dtest)
get_evaluation(xgb_model_2, dtest)
get_evaluation(xgb_model_3, dtest)












