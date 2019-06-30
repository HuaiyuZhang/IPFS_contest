read.csv()
target_test <- df_test$y_test
pred <- predict(my_model, X_test)
library(pROC)
roc0 <- roc(response = target_test, predictor = pred)
plot(roc0)

roc1 <- roc(response = target_test, predictor = pred, percent=TRUE,
                    # arguments for auc
                    partial.auc=c(100, 90), partial.auc.correct=TRUE,
                    partial.auc.focus="sens",
                    # arguments for ci
                    ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
                    # arguments for plot
                    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE)
roc2 <- roc(response = target_test, predictor = pred,
            plot=TRUE, add=TRUE, percent=roc1$percent)


############

# Evaluation
get_recall <- function(truth, pred){
  sum(truth == 1 & pred == 1)/sum(truth == 1)
}
get_precision <- function(truth, pred){
  sum(truth == 1 & pred == 1)/sum(pred == 1)
}
thresh_list <- seq(0.2, 0.6, by = 0.02)
recall_list <- NULL
precision_list <- NULL
for (i in 1:length( thresh_list ) ){
  y_pred <- as.numeric(pred > thresh_list[i])
  recall <- get_recall(truth = target_test,pred = y_pred)
  recall_list <- c(recall_list, recall)
  precision <- get_precision(truth = target_test,pred = y_pred)
  precision_list <- c(precision_list, precision)
}
res_df <- data.frame(threshold = thresh_list, 
           recall = recall_list,
           precision = precision_list,row.names = NULL)
print(round(res_df, 3))
