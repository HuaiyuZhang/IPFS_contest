
xgb_model <- readRDS('./output/model_0413_v2.RDS')
dat <- read.csv('./data/my_train_data.csv', header = T)
# require(iml)
X_train <- as.matrix(dat[1:10,-1])
pred0 <- predict(xgb_model, X_train)
pred_contr <- predict(xgb_model, X_train, predcontrib = T)
# summary(rowSums(pred) - qlogis(pred0))
# rowSums(pred) - qlogis(pred0)

pred0 <- pred0[3]
contr1 <- pred_contr[3,]
contr1 <- contr1[-length(contr1)] # drop BIAS
contr1 <- contr1[order(abs(contr1), decreasing = T)]
contr1 <- contr1[1:15]        # drop non-contributing features
# contr1 <- contr1[order(abs(contr1))] # order by contribution magnitude
# old_mar <- par("mar")
# par(mar = old_mar + c(0,7,0,0))
# barplot(contr1, horiz = TRUE, las = 2, 
#         xlab = "Feature contribution to prediction in log-odds",
#         col = rgb(164, 6, 62,  max = 255), border = NA,
#         xlim = c(-4.2, 0.5))
# par(mar = old_mar)

library(ggplot2)
df <- data.frame(var_name = names(contr1), 
                 values = contr1)
ggplot(df, aes(x=reorder(var_name, -values), y=values)) + 
  geom_bar(stat = "identity", fill = '#A4063E', color = '#A4063E') +
  coord_flip() +
  theme_minimal()+
  labs(title = paste0("The cancellation probability of this transaction is ", 
                      round(pred0, 4)))

       