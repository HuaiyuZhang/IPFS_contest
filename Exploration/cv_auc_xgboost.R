
rm(list = ls())
# These hyper-parameters are found by tuning.
dat_train <- read.csv('my_train_data.csv', header = T)

library(quantileDA) # balanced.folds function
set.seed(2019)
folds <- balanced.folds(dat_train$Cancelled, nfold = 10)
X <-  as.matrix(dat_train[,-1])
response <- dat_train[, 1]
pred_outcome1 <- numeric(length(response))
for (i in 1:length(folds)){
  cat(i,'~')
  all_index  <- 1:length(response)
  test_index <- folds[[i]]
  train_index <- all_index[-test_index]
  X_train <- X[train_index,]
  y_train <- response[train_index]
  X_test <- X[test_index,]
  require(xgboost)
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  xgb_model<- xgb.train(data = dtrain,
                        max.depth = 15, 
                        eta = 0.282,
                        nrounds = 65,
                        lambda=50.6,
                        objective = "binary:logistic",
                        eval.metric = 'auc',
                        verbose = 1
                        # watchlist = list(train = dtrain)
  )
  pred_outcome1[test_index] <- predict(xgb_model, X_test) 
}

library(pROC)
roc1 <- roc(response, pred_outcome1, algorithm = 2)
plot(roc1)




thresh_list <- seq(0.2, 0.6, by = 0.02)

for (i in 1:length(thresh_list)){
  pred_label <- as.factor( ifelse(pred_outcome1 > thresh_list[i], 1, 0) )
  library(caret)
  results <- confusionMatrix(pred_label,as.factor(response), positive = '1')
  acc <- round(results$overall['Accuracy'], 3)
  rec <- round(results$byClass['Recall'], 3)
  prec <- round(results$byClass['Precision'], 3)
  print(paste0(thresh_list[i],
         'Accuracy: ', acc, 'Recall: ', rec, 'Precision: ', prec))
}

