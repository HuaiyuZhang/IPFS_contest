#################################################################
# IPFS competition code                                       
#----------------------------------------------------------------
# Goal: Data Cleaning                                         
# Author: Huaiyu Zhang                                        
# Created: 3/27/2019    
#----------------------------------------------------------------
# Description: 
# 1. Seperate the predictors into numerical, categorical, binary,
#    date, and drop_list. 
# 2. Missing values are coded as NA. They are not imputed because
#    some algorithms have their own way of handling missing.
# 3. Numerical and date variables were in strings and now are 
#    converted to proper types.
# 4. The finished dataset is output to an RDS file.
#################################################################

rm(list = ls())
setwd('C:\\Dropbox\\ipfs\\data')
dat <- read.csv('Data.csv', stringsAsFactors = F, 
                na.strings = c('NULL','NS',''))

dim(dat)
# [1] 140574     48
# head(dat)
table(dat$Cancelled)
# 1: cancelled, 0: not cancelled
# 0      1 
# 127960  12614 


## All candidate predictors
var_list <- colnames(dat)
## This list of variables will be dropped.
drop_var_list <- NULL

## Missing percentage
missing_percentage <- round(apply(dat, 2,  function(x) sum(is.na(x))/nrow(dat)), 3)
missing_percentage[missing_percentage>0.2]
drop_var_list <- union(drop_var_list, 'Borrower_Category')
##~ Note that 'BankruptcyFlag' also has large missing, but this variable is informative

## ID variable
length(unique(dat$Loan_Num)) # 140574
drop_var_list <- union(drop_var_list, 'Loan_Num')


## Categorical variables
categorical_var_list <- c('Borrower_ID',
                          'Agent_ID',
                          'Borrower_State',
                          'Borrower_Zip', # substr(zip, 1, 5)
                          'Agent_State',
                          'Borrower_Classification', 
                          'Borrower_SubTerm',
                          'Risk_Score',
                          'BankruptcyFlag')
for (j in 1: length(categorical_var_list)){
  cat(categorical_var_list[j], ' ',length(unique(dat[,categorical_var_list[j]])), '\n')
}


## Binary variables list
# These are all of integer values
binary_var_list <- c(
  'Borrower_EnrolledInEForms',
  'Agent_RegisteredOnWeb',
  'Agent_EnrolledInEForms',
  'Agent_IntentEForm',
  'Agent_CancelEForm',
  'Agent_ReinstateEForm',
  'Agent_PendingPolicyRequestEForm',
  'Agent_ACHPaymentLetterEForm',
  'Agent_RegisteredForCancelListReport',
  'Agent_RegisteredForEimpendingReport',
  'Agent_RecurringACHForm',
  'Agent_EnrolledInCreditProgram',
  'Agent_DownPaymentViaCC',
  'Agent_DownPaymentViaACH',
  'Borrower_RegisteredOnWeb',
  'Borrower_RegisteredForEForms',
  'Borrower_RegisteredForCancellationWarning',
  'RecurringACH_TF'
)


## Numerical variables list
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

## Date variables list
date_var_list <- var_list[grep('Date',var_list, ignore.case = T)]
for (j in 1: length(date_var_list)){
  dat[,date_var_list[j]] <- as.Date(dat[,date_var_list[j]], "%m/%d/%Y")
}

## Check sum number of predictor candidates
num_predictors <- length(drop_var_list) + length(numerical_var_list) + 
  length(categorical_var_list) + length(date_var_list) +
  length(binary_var_list)

## Drop columns
dat <- dat[, !(names(dat) %in% drop_var_list)]

## Data types after processing
# sapply(dat, class)

## Output the data to a RDS file
saveRDS(dat, 'clean_data')

## For future use, load the data
# rm(list = ls())
# dat <- readRDS('clean_data')
