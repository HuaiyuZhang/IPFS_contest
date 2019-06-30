#################################################################
# IPFS competition code                                       
# Author: DataCat                                     
#################################################################
rm(list = ls())
####################################################
# Preprocess the training set and test set together#
####################################################
dat <- read.csv('Data.csv', 
                stringsAsFactors = F, 
                na.strings = c('NULL','NS',''))
dat$test_flag = rep(0, nrow(dat))

test_set <- read.csv('Test_Data.csv', 
                     stringsAsFactors = F, 
                     na.strings = c('NULL','NS',''))
test_set$test_flag = rep(1, nrow(test_set))
dat <- rbind(dat, test_set)
var_list <- colnames(dat)
# Create a list for the features that need to be dropped
drop_var_list <- c("Borrower_Category", # all missing
                   "Loan_Num") # id variable
# 
#------ Convert numerical variables to numerical values --------
#
numerical_var_list <- c('Default_Charge', 
                        'Late_Charge', 
                        'NSF_Charge',
                        'Premium', 
                        'Down', 
                        'AmtFin', # = Premium - down
                        'FinChg', 
                        'APR', 
                        'Term', 
                        'Payments_Rcvd', 
                        'Exposure', 
                        'Borrower_CreditScore')
for (j in 1:length(numerical_var_list)){
  dat[,numerical_var_list[j]] = as.numeric(dat[,numerical_var_list[j]])
}
#
#---------------- Process date variables ------------------------
#
date_var_list <- var_list[grep('Date',var_list, ignore.case = T)]
for (j in 1: length(date_var_list)){
  dat[,date_var_list[j]] <- as.Date(dat[,date_var_list[j]], "%m/%d/%Y")
}

# 1. Sanity check: The mature day should be later than the accepted day
flag <- which(dat$Mature_Date > dat$Accepted_Date)
dat <- dat[flag, ]

# 2. The dat$term has covered the information of Mature-Accepted
#    Convert the Accepted and Mature as numeric values
dat$Accepted_Date_number <- as.numeric(dat$Accepted_Date)
dat$Mature_Date_number <- as.numeric(dat$Mature_Date)
drop_var_list <- union(drop_var_list, 
                       c('Mature_Date', 'Accepted_Date'))

# 3. Use the NoticeOfIntentToCancelDate to Create two features
# dat$days_notice_intent_cancel_since_start <- 
#   as.numeric(dat$NoticeOfIntentToCancelDate -dat$Accepted_Date)
dat$days_ratio_notice_intent_cancel_to_end <- 
  as.numeric(dat$Mature_Date - dat$NoticeOfIntentToCancelDate) / as.numeric(dat$Mature_Date - dat$Accepted_Date)
dat$days_ratio_notice_intent_cancel_to_end[is.na(dat$days_ratio_notice_intent_cancel_to_end)] <- 1
drop_var_list <- union(drop_var_list, 
                       c('NoticeOfIntentToCancelDate'))

# 4. Convert the Borrower dates to numeric
dat$Borrower_Registered_Web_Date_days_to_accept <- 
  as.numeric(dat$Borrower_DateRegisteredForWeb) - as.numeric(dat$Accepted_Date)

dat$Borrower_EForm_Date_days_to_accept <- 
  as.numeric(dat$Borrower_DateRegisteredForEForms) - as.numeric(dat$Accepted_Date)

dat$Borrower_Reg_Cancel_Warning_Date_number_days_intent_notice <- 
  as.numeric(dat$NoticeOfIntentToCancelDate) - as.numeric(dat$Borrower_DATERegisteredForCancellationWarning)
drop_var_list <- union(drop_var_list, 
                       c('Borrower_DateRegisteredForWeb',
                         'Borrower_DateRegisteredForEForms',
                         'Borrower_DATERegisteredForCancellationWarning'))
#
#-------------Create a new variable: old_customer--------------------
#
dat$old_customer_flag <- rep(0, nrow(dat))
for (i in 1:nrow(dat)){# runing this step takes a while
 if (i%%1000 == 0) {
   cat(i,'~')
 }
 current_borrower_id <- dat$Borrower_ID[i]
 if (dat$Accepted_Date_number[i] >
     min(dat$Accepted_Date_number[dat$Borrower_ID == current_borrower_id])){
   dat$old_customer_flag[i] = 1
 }
}
drop_var_list <- union(drop_var_list, 'Borrower_ID')

#-------------Create a new variable: payments_left_percent--------------------
dat$payments_left_percent <- (dat$Term - dat$Payments_Rcvd) / dat$Term
#
#-------------Convert the Borrower zipcode------------------------
#
dat$Borrower_Zip <- substr(dat$Borrower_Zip, 1, 5)
#
#------------ AgentID to charactors ------------------------------
#
dat$Agent_ID <- as.character(dat$Agent_ID)
#
#------------ Remove drop list predictors ------------------------
#
drop_var_list <- union(drop_var_list, c('Premium', 
                                        'Payments_Rcvd',
                                        'Accepted_Date_number'))
dat <- dat[, !(names(dat) %in% drop_var_list)]
#
#--------- Categorical variables to sparse matrix ----------------
#
character_var_list <- names(dat)[sapply(dat, class) == 'character']
dat_categorical <- dat[,c(names(dat) %in% character_var_list)]
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
#
#---------- Combine the dense matrix-----------------------------
#
dense_mat <- as.matrix(dat[,c( !(names(dat) %in% character_var_list))])
dat_finish <- cbind(dense_mat, categorical_matrix)
# saveRDS(dat_finish, 'dat_all.RDS')
# rm(list = ls())
# dat_finish <- readRDS('dat_all.RDS')
#
# This candicate_var_list is built based on three different xgboost 
# models. In each one, pick out the first 100 important variables
# then take the union.
# To make the algorithm reproducible, the variable list is shown.
# There are 124 variables.
candidate_var_list <- c("Borrower_StateTX", "Borrower_ClassificationTransportation", "Borrower_StateLA", "Borrower_StateFL", "Borrower_Zip71202", "Borrower_SubTermReal Estate", "Risk_ScoreC", "Agent_StateLA", "Borrower_SubTermTrucking", "Agent_StateMS", "Agent_StateTX", "Agent_ID71", "Borrower_SubTermPersonal DBA", "Borrower_ClassificationReal Estate", "Borrower_Zip39564", "Borrower_SubTermGeneral Transportation", "Borrower_SubTermGeneral Services", "Borrower_ClassificationServices", "Agent_ID115", "Agent_ID37", "Borrower_ClassificationRetail Trade", "Borrower_Zip77510", "Borrower_SubTermFinance", "Borrower_SubTermMedical", "Borrower_ClassificationGeneral", "Borrower_Zip75495", "Borrower_Zip75380", "Agent_ID1179", "Agent_ID1006", "Borrower_Zip75043", "Borrower_Zip77493", "Borrower_SubTermLegal", "Borrower_SubTermGeneral Retail", "Borrower_StateMS", "Borrower_SubTermProfessional Services", "Agent_ID90", "Borrower_ClassificationConstruction", "Borrower_SubTermGeneral Other", "Borrower_Zip39560", "Borrower_SubTermRecreational Services", "Borrower_StateIL", "Borrower_Zip39301", "Borrower_StateGA", "Agent_ID274", "Borrower_Zip70130", "Agent_ID28", "Agent_ID26", "Borrower_StateCA", "Agent_ID178", "Borrower_SubTermAviation", "Agent_ID298", "Borrower_Zip70124", "Agent_ID17", "Agent_ID113", "Risk_ScoreB", "Borrower_StateNY", "Borrower_SubTermGeneral Construction", "Borrower_SubTermOrganization", "Agent_ID27", "Agent_ID5", "Borrower_SubTermTechnical Assortment", "Agent_ID29", "Borrower_Zip70122", "Borrower_SubTermMarine", "Borrower_StateOR", "Borrower_Zip78045", "Borrower_Zip70056", "Borrower_Zip77008", "Borrower_SubTermChurch", "Borrower_StateAL", "Agent_StateFL", "Borrower_Zip78572", "Borrower_ClassificationWholesale Trade", "Borrower_Zip70065", "Agent_ID83", "Borrower_Zip39532", "Agent_ID38", "Borrower_Zip77055", "Borrower_StateNC", "Agent_StateOK", "Borrower_StateOH", "Borrower_Zip70525", "Borrower_SubTermLocations", "Agent_ID126", "Agent_ID70", "Agent_ID23", "Agent_StateAR", "Borrower_SubTermTaxi/Limo Livery", "Agent_ID65", "Default_Charge", "Late_Charge", "NSF_Charge", "Down", "AmtFin", "FinChg", "APR", "Term", "Exposure", "Borrower_CreditScore", "Borrower_EnrolledInEForms", "Agent_RegisteredOnWeb", "Agent_EnrolledInEForms", "Agent_IntentEForm", "Agent_CancelEForm", "Agent_ReinstateEForm", "Agent_PendingPolicyRequestEForm", "Agent_ACHPaymentLetterEForm", "Agent_RegisteredForCancelListReport", "Agent_RegisteredForEimpendingReport", "Agent_RecurringACHForm", "Agent_EnrolledInCreditProgram", "Agent_DownPaymentViaCC", "Agent_DownPaymentViaACH", "Borrower_RegisteredOnWeb", "Borrower_RegisteredForEForms", "Borrower_RegisteredForCancellationWarning", "RecurringACH_TF", "Mature_Date_number", "days_ratio_notice_intent_cancel_to_end", "Borrower_Registered_Web_Date_days_to_accept", "Borrower_EForm_Date_days_to_accept", "Borrower_Reg_Cancel_Warning_Date_number_days_intent_notice", "old_customer_flag", "payments_left_percent")
var_list <- union(c('Cancelled','test_flag'), candidate_var_list)
dat_finish <- dat_finish[, var_list]
dat_train <- dat_finish[dat_finish[,'test_flag'] == 0,]
dat_test <- dat_finish[dat_finish[,'test_flag'] == 1,]
X_train <- dat_train[, 3:126]
X_test <- dat_test[,3:126]
y_train <- dat_train[, 1]
y_test <- dat_test[, 1]

write.csv(as.matrix(cbind(Cancelled = y_test, X_test)), 'my_test_data_datacats.csv',
          row.names = F)
write.csv(as.matrix(cbind(Cancelled = y_train, X_train)), 'my_train_data_datacats.csv',
          row.names = F)
#


##################
# Model building #
##################
rm(list = ls())
# Tune result:
#   Op. pars: nrounds=65; max_depth=15; eta=0.282; lambda=50.6
# auc.test.mean=0.9918165
# These hyper-parameters are found by tuning.
# The code for parameter tuning is not presented here. If you would
# like to see it, please let us know.
dat_train <- read.csv('my_train_data_datacats.csv', header = T)
X_train <- as.matrix(dat_train[,-1])
y_train <- dat_train[, 1]
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
)

saveRDS(xgb_model,'my_model_datacats.RDS')

#########################
### GRADING COMMITTEE ###
#########################

# 1) Load your version of the test data
df_test <- read.csv('my_test_data_datacats.csv', header = T)
target_test <- as.factor(df_test$Cancelled)

# 2) Load model from .rds file
my_model <- readRDS(file = "my_model_datacats.RDS")

# 3) Fit model to your test data for prediction

# Note: 
# 1. The example code doesn't work for xgboost model because it
#   only predicts the probability. A little change of the original code.
# 2. The 'predict' function for xgboost object requires xgboost pacakge
# 3. We examined a series of thereshold values and choose 0.32 for a 0.99
# recall score and acceptable precison.
require(xgboost)
X_test <- as.matrix(df_test[,2:125])
pred <- as.factor(ifelse(predict(my_model, X_test) > 0.32, 1, 0))

# 4) Calculate accuracy metrics
install.packages('caret')
library(caret)
results <- confusionMatrix(pred, target_test, positive = '1')
acc <- round(results$overall['Accuracy'], 3)
rec <- round(results$byClass['Recall'], 3)
prec <- round(results$byClass['Precision'], 3)
paste0('Accuracy: ', acc)
paste0('Recall: ', rec)
paste0('Precision: ', prec)

