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
dat <- readRDS('dat_all.RDS')
var_list <- colnames(dat)
continuous_var_list <- var_list[2:39]
categorical_var_list <- var_list[40: length(var_list)]

label <- dat[,1]
dat <- dat[, -1] # if use all the predictors
# dat <- dat[,2:39] # if use only the numerical predictors

#------------------------ Train-test split ------------------------
indices <- 1:nrow(dat)
set.seed(2017)
# Split ratio: 42% train, 28% valid, 30% test
train_index <- sample(indices, round(0.7*length(indices),0), 
                      replace = F)
valid_index <- sample(train_index, round(0.4*length(train_index),0), 
                      replace = F)
test_index <- indices[-train_index]
train_index <- indices[-c(valid_index, test_index)]

X_train <- dat[train_index,]; y_train <- label[train_index]
X_test <- dat[test_index,]; y_test <- label[test_index]
X_valid <- dat[valid_index,]; y_valid <- label[valid_index]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)
dvalid <- xgb.DMatrix(data = X_valid, label = y_valid)

watchlist <- list(train = dtrain, eval = dvalid)

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
saveRDS(candidate_var_list, 'candidate_var_list')
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

get_evaluation(xgb_model_1, dtest)
get_evaluation(xgb_model_2, dtest)
get_evaluation(xgb_model_3, dtest)












