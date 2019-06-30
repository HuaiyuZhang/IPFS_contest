setwd('C:\\Dropbox\\ipfs')
xgb_model <- readRDS('./output/model_0413_v2.RDS')
dat <- read.csv('./data/my_train_data.csv', header = T)
require(xgboost)
# require(iml)
X_train <- as.matrix(dat[,-1])
pred0 <- predict(xgb_model, X_train)

pred_contr <- predict(xgb_model, X_train, predcontrib = T, 
                      approxcontrib = T)
# summary(rowSums(pred_contr) - qlogis(pred0))
# rowSums(pred_contr) - qlogis(pred0)
#####################
all_mean <- apply(pred_contr, 2, mean)
round(all_mean, 3)
hist(pred_contr[, 'Late_Charge'])
hist(pred_contr[, 'payments_left_percent'])
hist(pred_contr[, 'Agent_StateTX'])
##############33
contr1 <- pred_contr[1,]
contr1 <- contr1[-length(contr1)]    # drop BIAS
contr1 <- contr1[abs(contr1) > 1e-2]        # drop non-contributing features
contr1 <- contr1[order(abs(contr1))] # order by contribution magnitude
old_mar <- par("mar")
par(mar = old_mar + c(0,7,0,0))
barplot(contr1, horiz = TRUE, las = 2, 
        xlab = "contribution to prediction in log-odds")
par(mar = old_mar)

# shiny
# url <- ('https://raw.githubusercontent.com/HuaiyuZhang/data_structure/master/model_try.RDS')
# download.file(url, 'try.RDS', method = 'curl')
# my <- readRDS('try.RDS')