setwd('C:\\Dropbox\\ipfs\\data')
dat_train <- read.csv('my_train_data.csv', header = T)
y_train <- dat_train[, 1]
dat_train <- dat_train[,-1]
dim(dat_train)
#---------- Impute missing values using mice-------------
# imputed_Data <- mice(dat_train, 
#                      m=5, maxit = 5, 
#                       seed = 500)
# saveRDS(imputed_Data, 'Imputed_Data')

#----------- glmnet with the imputed data ---------------
# imputed_Data <- readRDS('../data/Imputed_Data')
imputed_Data <- readRDS('C:\\Dropbox\\Wang-hy\\!XGBOOST\\Imputed_Data')
library(mice)
complete_set <- complete(imputed_Data,5)
com_index <- complete.cases(complete_set)
# discard_index <- c(29832, 29833, 29834)
y_train <- y_train[com_index]
X_train <- as.matrix( complete_set[com_index, ])
sum(is.na(X_train))
sum(is.na(y_train))



require(glmnet)
# model_all <- cv.glmnet(X_train, y_train, family='binomial',
                       # type.measure="auc") 
# coeffs=coef(model_all, s = "lambda.1se")
# selected=row.names(coeffs)[which(coeffs != 0)]

model_glmnet <- glmnet(X_train, y_train, family='binomial',
                       lambda = 0.001, alpha = 0.5)
coeffs=coef(model_glmnet, s = "lambda.1se")
y_pred_prob <- predict(model_glmnet, newx = X_train, 
                       type='response') 
library(pROC)
roc(y_train, as.numeric(y_pred_prob))
# The AUC under training is 0.9531



#----------------- glmnet with interactions-----------------
# Consider the top 15 features in xgboost and use the to construct 
# glmnet
# Model information
xgb_model <- readRDS('./output/my_model.RDS')
importance_matrix <- xgb.importance(model = xgb_model)
var_list <- importance_matrix[1:15, 1][[1]]
imputed_Data <- readRDS('../data/Imputed_Data')
library(mice)
complete_set <- complete(imputed_Data,5)
discard_index <- c(29832, 29833, 29834)
complete_set <- complete_set[-discard_index,]
sum(names(complete_set) %in% var_list)
dat <- complete_set[, names(complete_set) %in% var_list]
dim(dat)

# Train a model with only these 15 features

# Interactions





