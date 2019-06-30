
setwd('C:\\Dropbox\\datacats\\Deliverables_datacats')

my_model <- readRDS(file = "my_model_datacats.RDS")

require(xgboost)
importance_matrix <- xgb.importance(model = my_model)
print(importance_matrix[1:10,])
