# var_list <- readRDS('C:\\Dropbox\\ipfs\\output\\candidate_var_list')
var_new <- c("Borrower_StateTX", "Borrower_ClassificationTransportation", "Borrower_StateLA", "Borrower_StateFL", "Borrower_Zip71202", "Borrower_SubTermReal Estate", "Risk_ScoreC", "Agent_StateLA", "Borrower_SubTermTrucking", "Agent_StateMS", "Agent_StateTX", "Agent_ID71", "Borrower_SubTermPersonal DBA", "Borrower_ClassificationReal Estate", "Borrower_Zip39564", "Borrower_SubTermGeneral Transportation", "Borrower_SubTermGeneral Services", "Borrower_ClassificationServices", "Agent_ID115", "Agent_ID37", "Borrower_ClassificationRetail Trade", "Borrower_Zip77510", "Borrower_SubTermFinance", "Borrower_SubTermMedical", "Borrower_ClassificationGeneral", "Borrower_Zip75495", "Borrower_Zip75380", "Agent_ID1179", "Agent_ID1006", "Borrower_Zip75043", "Borrower_Zip77493", "Borrower_SubTermLegal", "Borrower_SubTermGeneral Retail", "Borrower_StateMS", "Borrower_SubTermProfessional Services", "Agent_ID90", "Borrower_ClassificationConstruction", "Borrower_SubTermGeneral Other", "Borrower_Zip39560", "Borrower_SubTermRecreational Services", "Borrower_StateIL", "Borrower_Zip39301", "Borrower_StateGA", "Agent_ID274", "Borrower_Zip70130", "Agent_ID28", "Agent_ID26", "Borrower_StateCA", "Agent_ID178", "Borrower_SubTermAviation", "Agent_ID298", "Borrower_Zip70124", "Agent_ID17", "Agent_ID113", "Risk_ScoreB", "Borrower_StateNY", "Borrower_SubTermGeneral Construction", "Borrower_SubTermOrganization", "Agent_ID27", "Agent_ID5", "Borrower_SubTermTechnical Assortment", "Agent_ID29", "Borrower_Zip70122", "Borrower_SubTermMarine", "Borrower_StateOR", "Borrower_Zip78045", "Borrower_Zip70056", "Borrower_Zip77008", "Borrower_SubTermChurch", "Borrower_StateAL", "Agent_StateFL", "Borrower_Zip78572", "Borrower_ClassificationWholesale Trade", "Borrower_Zip70065", "Agent_ID83", "Borrower_Zip39532", "Agent_ID38", "Borrower_Zip77055", "Borrower_StateNC", "Agent_StateOK", "Borrower_StateOH", "Borrower_Zip70525", "Borrower_SubTermLocations", "Agent_ID126", "Agent_ID70", "Agent_ID23", "Agent_StateAR", "Borrower_SubTermTaxi/Limo Livery", "Agent_ID65", "Default_Charge", "Late_Charge", "NSF_Charge", "Down", "AmtFin", "FinChg", "APR", "Term", "Exposure", "Borrower_CreditScore", "Borrower_EnrolledInEForms", "Agent_RegisteredOnWeb", "Agent_EnrolledInEForms", "Agent_IntentEForm", "Agent_CancelEForm", "Agent_ReinstateEForm", "Agent_PendingPolicyRequestEForm", "Agent_ACHPaymentLetterEForm", "Agent_RegisteredForCancelListReport", "Agent_RegisteredForEimpendingReport", "Agent_RecurringACHForm", "Agent_EnrolledInCreditProgram", "Agent_DownPaymentViaCC", "Agent_DownPaymentViaACH", "Borrower_RegisteredOnWeb", "Borrower_RegisteredForEForms", "Borrower_RegisteredForCancellationWarning", "RecurringACH_TF", "Mature_Date_number", "days_ratio_notice_intent_cancel_to_end", "Borrower_Registered_Web_Date_days_to_accept", "Borrower_EForm_Date_days_to_accept", "Borrower_Reg_Cancel_Warning_Date_number_days_intent_notice", "old_customer_flag", "payments_left_percent")
dict_goal <- vector(mode="list", length=length(var_new))
names(dict_goal) <- var_new
lookup_name <- 'Borrower_Zip78045'
dict_source= list()
dict_source[['Borrower_Zip']] = '78045'

if ( grep('Borrower_Zip',lookup_name) == 1 ) 
{ 
  if (substr(lookup_name, 13,17)  ==   dict_source[[ 'Borrower_Zip' ]])
  {
    dict_goal[[ lookup_name ]] <- 1
    }
}
dict_goal[[ lookup_name ]]
####

if ( grep('Borrower_Zip',lookup_name) == 1 )
{
  # if the source dict has NA for it
  if(is.na(dict_source[[ 'Borrower_Zip' ]])){
    dict_goal[[ lookup_name ]] <- NA
  }
  # The last 5 digits of the name of indicator
  else if (substr(lookup_name, 13,17)  ==   dict_source[[ 'Borrower_Zip' ]])
  {
    dict_goal[[ lookup_name ]] <- 1
  }
  else{
    dict_goal[[ lookup_name ]] <- 0
  }
  
  
}


s <- "Borrower_ClassificationReal es"
s <- "Borrower_ClassificationTransportation"
s <- 'Borrower_SubTermxxx'
s <- 'Risk_ScoreB'
substring(s, 122, 1000)

setwd('C:\\Dropbox\\ipfs\\data')
####################################################
# Preprocess the training set and test set together#
####################################################
dat <- read.csv('Data.csv', 
                stringsAsFactors = F, 
                na.strings = c('NULL','NS',''))

print_list <- function(x){
  for (i in 1:length(x)){
    cat("'", x[i], "',", sep = '')
  }
}


BorrowerClassficationList <- unique(dat$Borrower_Classification)
print_list(BorrowerClassficationList)

BorrowerStateList <- unique(dat$Borrower_State)
print_list(BorrowerStateList)


BorrowerSubtermList <- unique(dat$Borrower_SubTerm)
print_list(BorrowerSubtermList)

BorrowerRiskScoreList <- unique(dat$Risk_Score)
print_list(BorrowerRiskScoreList)


grep('Risk_Score', var_list)


AgentStateList <- unique(dat$Agent_State)
print_list(AgentStateList)

print_list(my_model$feature_names)












