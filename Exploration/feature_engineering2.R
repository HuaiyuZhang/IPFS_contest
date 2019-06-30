#################################################################
# IPFS competition code                                       
#----------------------------------------------------------------
# Goal: Feature engineering 2                                    
# Author: Huaiyu Zhang                                        
# Created: 4/3/2019    
#----------------------------------------------------------------
# Description: 
# 1. Creaete one-hot encoding
# 2. Store the data in two forms

#################################################################
rm(list = ls())
setwd('C:\\Dropbox\\ipfs')
dat <- readRDS('clean_data_1')
label <- dat$Cancelled
dat <- dat[, !(names(dat) == 'Cancelled')]
require(xgboost)

#------------ Categorical variables to sparse matrix -------------
character_var_list <- names(dat)[sapply(dat, class) == 'character']
# [1] "Agent_ID"                "Borrower_State"         
# [3] "Borrower_Zip"            "Agent_State"            
# [5] "Borrower_Classification" "Borrower_SubTerm"       
# [7] "Risk_Score"              "BankruptcyFlag"

dat_categorical <- dat[,c(names(dat) %in% character_var_list)]
# Make sparse matrix
require(Matrix)
previous_na_action <- options('na.action')
options(na.action='na.pass')
categorical_matrix <- sparse.model.matrix(~ Agent_ID +
                                            Borrower_State +
                                            Borrower_Zip +
                                            Agent_State + 
                                            Borrower_Classification +
                                            Borrower_SubTerm +
                                            Risk_Score +
                                            BankruptcyFlag ,
                                          dat_categorical)
options(na.action=previous_na_action$na.action)

#---------- Combine the dense matrix-----------------------------
dense_df <- dat[,c( !(names(dat) %in% character_var_list))]
dense_mat <- as.matrix(dense_df)
# Output this dense matrix
saveRDS(cbind(label, dense_mat), 'dat_numerical.RDS')

non_char_var_list <- colnames(dense_mat) <- names(dense_df)
predictors <- cbind(dense_mat, categorical_matrix)
saveRDS(cbind(label,predictors), 'dat_all.RDS')



