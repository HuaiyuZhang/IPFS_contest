#################################################################
# IPFS competition code                                       
#----------------------------------------------------------------
# Goal: Feature Engineering 1                                       
# Author: Huaiyu Zhang                                        
# Created: 3/29/2019    
#----------------------------------------------------------------
# Description: 
# 1. Convert date variables to numeric
# 2. Create new variables based on date information
#################################################################
rm(list = ls())
setwd('C:\\Dropbox\\ipfs')
dat <- readRDS('clean_data')
drop_var_list <- NULL

#----------------- Dealing with date variables-------------------
#
var_list <- colnames(dat)
date_var_list <- var_list[grep('Date',var_list, ignore.case = T)]
# [1] "Accepted_Date"                                
# [2] "Mature_Date"                                  
# [3] "Borrower_DateRegisteredForWeb"                
# [4] "Borrower_DateRegisteredForEForms"             
# [5] "Borrower_DATERegisteredForCancellationWarning"
# [6] "NoticeOfIntentToCancelDate"

# 1. The mature day should be later than the accepted day
flag <- which(dat$Mature_Date > dat$Accepted_Date)
dat <- dat[flag, ]

# 2. The dat$term has covered the information of Mature-Accepted
#    Convert the Accepted and Mature as numeric values
dat$Accepted_Date_number <- as.numeric(dat$Accepted_Date)
dat$Mature_Date_number <- as.numeric(dat$Mature_Date)
drop_var_list <- union(drop_var_list, 
                       c('Mature_Date', 'Accepted_Date'))


# 3. Use the NoticeOfIntentToCancelDate to Create two features
dat$days_notice_intent_cancel_since_start <- 
  as.numeric(dat$NoticeOfIntentToCancelDate -dat$Accepted_Date)
dat$days_notice_intent_cancel_to_end <- 
  as.numeric(dat$Mature_Date - dat$NoticeOfIntentToCancelDate)
drop_var_list <- union(drop_var_list, 
                       c('NoticeOfIntentToCancelDate'))

# 4. Convert the Borrower dates to numeric
dat$Borrower_Registered_Web_Date_number <- 
  as.numeric(dat$Borrower_DateRegisteredForWeb)
dat$Borrower_EForm_Date_number <- 
  as.numeric(dat$Borrower_DateRegisteredForEForms)
dat$Borrower_Reg_Cancel_Warning_Date_number <- 
  as.numeric(dat$Borrower_DATERegisteredForCancellationWarning)
drop_var_list <- union(drop_var_list, 
                       c('Borrower_DateRegisteredForWeb',
                         'Borrower_DateRegisteredForEForms',
                         'Borrower_DATERegisteredForCancellationWarning'))

#-------------Create a new variable: old customer--------------------
dat$old_customer_flag <- rep(0, nrow(dat))
for (i in 1:nrow(dat)){
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

#-------------Convert the Borrower zipcode-----------------------
dat$Borrower_Zip <- substr(dat$Borrower_Zip, 1, 5)


#------------ AgentID to charactors ----------------------------
dat$Agent_ID <- as.character(dat$Agent_ID)

#----------------------------------------------------------------
dat <- dat[, !(names(dat) %in% drop_var_list)]
saveRDS(dat, 'clean_data_1')


