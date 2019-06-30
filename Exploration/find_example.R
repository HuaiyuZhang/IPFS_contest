
setwd('C:\\Dropbox\\datacats\\Deliverables_datacats')
df_test <- read.csv('my_test_data_datacats.csv', header = T)
target_test <- as.factor(df_test$Cancelled)

my_model <- readRDS(file = "my_model_datacats.RDS")

require(xgboost)
X_test <- as.matrix(df_test[,2:125])

y_pred_prob <- predict(my_model, X_test) 

big_prob <- y_pred_prob>0.9
which(big_prob)[1:10]
y_pred_prob[25]
y_pred_prob[1]
#Oringinal data
original_data <- read.csv('C:\\Dropbox\\datacats\\recycleBin\\Test_Data.csv')
# Default example
# 25  30  47  60  69  73 106 116 134 151
original_data[25,]
original_data[1,]

# Non Default example
original_data[1,]
