library(shiny)
# library(shinyWidgets)
library(ggplot2)
library(xgboost)
# setwd('D:\\temp\\ipfs')
function(input, output) {
  
  ########## Read in the data from the ui #######
  
  AllInputs <- reactive({
    # Use a dictionary-like data structure to store the source data
    dict_source <- vector(mode="list", length=length(names(input)))
    for(i in 1:length(names(input))){
      names(dict_source)[i] <- names(input)[i]
      dict_source[[i]] <- input[[names(input)[i]]]
    }
    ## new structure for the target data
    # var_new <- c("Borrower_StateTX", "Borrower_ClassificationTransportation", "Borrower_StateLA", "Borrower_StateFL", "Borrower_Zip71202", "Borrower_SubTermReal Estate", "Risk_ScoreC", "Agent_StateLA", "Borrower_SubTermTrucking", "Agent_StateMS", "Agent_StateTX", "Agent_ID71", "Borrower_SubTermPersonal DBA", "Borrower_ClassificationReal Estate", "Borrower_Zip39564", "Borrower_SubTermGeneral Transportation", "Borrower_SubTermGeneral Services", "Borrower_ClassificationServices", "Agent_ID115", "Agent_ID37", "Borrower_ClassificationRetail Trade", "Borrower_Zip77510", "Borrower_SubTermFinance", "Borrower_SubTermMedical", "Borrower_ClassificationGeneral", "Borrower_Zip75495", "Borrower_Zip75380", "Agent_ID1179", "Agent_ID1006", "Borrower_Zip75043", "Borrower_Zip77493", "Borrower_SubTermLegal", "Borrower_SubTermGeneral Retail", "Borrower_StateMS", "Borrower_SubTermProfessional Services", "Agent_ID90", "Borrower_ClassificationConstruction", "Borrower_SubTermGeneral Other", "Borrower_Zip39560", "Borrower_SubTermRecreational Services", "Borrower_StateIL", "Borrower_Zip39301", "Borrower_StateGA", "Agent_ID274", "Borrower_Zip70130", "Agent_ID28", "Agent_ID26", "Borrower_StateCA", "Agent_ID178", "Borrower_SubTermAviation", "Agent_ID298", "Borrower_Zip70124", "Agent_ID17", "Agent_ID113", "Risk_ScoreB", "Borrower_StateNY", "Borrower_SubTermGeneral Construction", "Borrower_SubTermOrganization", "Agent_ID27", "Agent_ID5", "Borrower_SubTermTechnical Assortment", "Agent_ID29", "Borrower_Zip70122", "Borrower_SubTermMarine", "Borrower_StateOR", "Borrower_Zip78045", "Borrower_Zip70056", "Borrower_Zip77008", "Borrower_SubTermChurch", "Borrower_StateAL", "Agent_StateFL", "Borrower_Zip78572", "Borrower_ClassificationWholesale Trade", "Borrower_Zip70065", "Agent_ID83", "Borrower_Zip39532", "Agent_ID38", "Borrower_Zip77055", "Borrower_StateNC", "Agent_StateOK", "Borrower_StateOH", "Borrower_Zip70525", "Borrower_SubTermLocations", "Agent_ID126", "Agent_ID70", "Agent_ID23", "Agent_StateAR", "Borrower_SubTermTaxi/Limo Livery", "Agent_ID65", "Default_Charge", "Late_Charge", "NSF_Charge", "Down", "AmtFin", "FinChg", "APR", "Term", "Exposure", "Borrower_CreditScore", "Borrower_EnrolledInEForms", "Agent_RegisteredOnWeb", "Agent_EnrolledInEForms", "Agent_IntentEForm", "Agent_CancelEForm", "Agent_ReinstateEForm", "Agent_PendingPolicyRequestEForm", "Agent_ACHPaymentLetterEForm", "Agent_RegisteredForCancelListReport", "Agent_RegisteredForEimpendingReport", "Agent_RecurringACHForm", "Agent_EnrolledInCreditProgram", "Agent_DownPaymentViaCC", "Agent_DownPaymentViaACH", "Borrower_RegisteredOnWeb", "Borrower_RegisteredForEForms", "Borrower_RegisteredForCancellationWarning", "RecurringACH_TF", "Mature_Date_number", "days_ratio_notice_intent_cancel_to_end", "Borrower_Registered_Web_Date_days_to_accept", "Borrower_EForm_Date_days_to_accept", "Borrower_Reg_Cancel_Warning_Date_number_days_intent_notice", "old_customer_flag", "payments_left_percent")
    var_new <- c('Borrower_StateTX','Borrower_ClassificationTransportation','Borrower_StateLA','Borrower_StateFL','Borrower_Zip71202','Borrower_SubTermReal.Estate','Risk_ScoreC','Agent_StateLA','Borrower_SubTermTrucking','Agent_StateMS','Agent_StateTX','Agent_ID71','Borrower_SubTermPersonal.DBA','Borrower_ClassificationReal.Estate','Borrower_Zip39564','Borrower_SubTermGeneral.Transportation','Borrower_SubTermGeneral.Services','Borrower_ClassificationServices','Agent_ID115','Agent_ID37','Borrower_ClassificationRetail.Trade','Borrower_Zip77510','Borrower_SubTermFinance','Borrower_SubTermMedical','Borrower_ClassificationGeneral','Borrower_Zip75495','Borrower_Zip75380','Agent_ID1179','Agent_ID1006','Borrower_Zip75043','Borrower_Zip77493','Borrower_SubTermLegal','Borrower_SubTermGeneral.Retail','Borrower_StateMS','Borrower_SubTermProfessional.Services','Agent_ID90','Borrower_ClassificationConstruction','Borrower_SubTermGeneral.Other','Borrower_Zip39560','Borrower_SubTermRecreational.Services','Borrower_StateIL','Borrower_Zip39301','Borrower_StateGA','Agent_ID274','Borrower_Zip70130','Agent_ID28','Agent_ID26','Borrower_StateCA','Agent_ID178','Borrower_SubTermAviation','Agent_ID298','Borrower_Zip70124','Agent_ID17','Agent_ID113','Risk_ScoreB','Borrower_StateNY','Borrower_SubTermGeneral.Construction','Borrower_SubTermOrganization','Agent_ID27','Agent_ID5','Borrower_SubTermTechnical.Assortment','Agent_ID29','Borrower_Zip70122','Borrower_SubTermMarine','Borrower_StateOR','Borrower_Zip78045','Borrower_Zip70056','Borrower_Zip77008','Borrower_SubTermChurch','Borrower_StateAL','Agent_StateFL','Borrower_Zip78572','Borrower_ClassificationWholesale.Trade','Borrower_Zip70065','Agent_ID83','Borrower_Zip39532','Agent_ID38','Borrower_Zip77055','Borrower_StateNC','Agent_StateOK','Borrower_StateOH','Borrower_Zip70525','Borrower_SubTermLocations','Agent_ID126','Agent_ID70','Agent_ID23','Agent_StateAR','Borrower_SubTermTaxi.Limo.Livery','Agent_ID65','Default_Charge','Late_Charge','NSF_Charge','Down','AmtFin','FinChg','APR','Term','Exposure','Borrower_CreditScore','Borrower_EnrolledInEForms','Agent_RegisteredOnWeb','Agent_EnrolledInEForms','Agent_IntentEForm','Agent_CancelEForm','Agent_ReinstateEForm','Agent_PendingPolicyRequestEForm','Agent_ACHPaymentLetterEForm','Agent_RegisteredForCancelListReport','Agent_RegisteredForEimpendingReport','Agent_RecurringACHForm','Agent_EnrolledInCreditProgram','Agent_DownPaymentViaCC','Agent_DownPaymentViaACH','Borrower_RegisteredOnWeb','Borrower_RegisteredForEForms','Borrower_RegisteredForCancellationWarning','RecurringACH_TF','Mature_Date_number','days_ratio_notice_intent_cancel_to_end','Borrower_Registered_Web_Date_days_to_accept','Borrower_EForm_Date_days_to_accept','Borrower_Reg_Cancel_Warning_Date_number_days_intent_notice','old_customer_flag','payments_left_percent')
    dict_goal <- vector(mode="list", length=length(var_new))
    names(dict_goal) <- var_new
    
    ## Loop over the new dict_goal, find the value for each one
    for (j in 1:length(dict_goal))
    {
      lookup_name <- names(dict_goal)[j]

      #@ if this name can be found
      if (lookup_name %in% names(dict_source))
      {
        dict_goal[[ lookup_name ]] <- dict_source[[ lookup_name ]]
      }
      
      
      #@ if this is Mature_Date_number
      else if (lookup_name=='Mature_Date_number'){
        dict_goal[[ lookup_name ]] <- as.numeric(dict_source[[ 'Mature_Date' ]])
      }

      #@ if this is days_ratio_notice_intent_cancel_to_end
      else if (lookup_name=='days_ratio_notice_intent_cancel_to_end'){
        if(is.na(dict_source[[ 'NoticeOfIntentToCancelDate' ]] )){
          dict_goal[[ lookup_name ]] <- 1
        }
        else{
          dict_goal[[ lookup_name ]] <-
            as.numeric((dict_source[[ 'Mature_Date' ]] - dict_source[[ 'NoticeOfIntentToCancelDate' ]]))/
            as.numeric((dict_source[[ 'Mature_Date' ]] - dict_source[[ 'Accepted_Date' ]]))
        }
      }

      #@ if this is Borrower_Registered_Web_Date_days_to_accept
      else if (lookup_name=='Borrower_Registered_Web_Date_days_to_accept'){
        dict_goal[[ lookup_name ]] <- as.numeric( dict_source[[ 'Borrower_DateRegisteredForWeb' ]] -
          dict_source[[ 'Accepted_Date' ]] )
      }

      #@ if this is Borrower_Registered_Web_Date_days_to_accept
      else if (lookup_name=='Borrower_EForm_Date_days_to_accept'){
        dict_goal[[ lookup_name ]] <- as.numeric(dict_source[[ 'Borrower_DateRegisteredForEForms' ]]) -
                                       as.numeric(dict_source[[ 'Accepted_Date' ]])
      }
      #@ if this is Borrower_Reg_Cancel_Warning_Date_number_days_intent_notice
      else if (lookup_name=='Borrower_Reg_Cancel_Warning_Date_number_days_intent_notice'){
        # if(is.na(dict_source[[ 'NoticeOfIntentToCancelDate' ]]) ||
        #          is.null(dict_source[[ 'NoticeOfIntentToCancelDate' ]])    )
        # {
        #   dict_goal[[ lookup_name ]] <- 0
        # }
        dict_goal[[ lookup_name ]] <-as.numeric( dict_source[[ 'NoticeOfIntentToCancelDate' ]] -
          dict_source[[ 'Borrower_DATERegisteredForCancellationWarning' ]])
      }
      #@ if this is old_customer_flag
      else if (lookup_name=='old_customer_flag'){
        dict_goal[[ lookup_name ]] <- 0L
      }
      
      #@ if this is payments_left_percent
      else if (lookup_name=='payments_left_percent'){
        dict_goal[[ lookup_name ]] <- (dict_source[['Term']] - dict_source[['Payments_Rcvd']]) / dict_source[['Term']]
      }
      
      # -------------Categorical variables-----------------------------
      # 
      
      
      # if the variable is Borrower_Zip indicator
      else if ( length(grep('Borrower_Zip',lookup_name)) >0 )
      {
        # if the source dict has NA for it
        if(is.na(dict_source[[ 'Borrower_Zip' ]])){
          dict_goal[[ lookup_name ]] <- NA
        }
        # The last 5 digits of the name of indicator
        else if (substr(lookup_name, 13,17)  ==   substr(dict_source[[ 'Borrower_Zip' ]], 1, 5))
        {
          dict_goal[[ lookup_name ]] <- 1L
        }
        else{
          dict_goal[[ lookup_name ]] <- 0L
        }
      }
      
      
      # if the variable is Borrower_State indicator
      else if ( length(grep('Borrower_State',lookup_name)) >0 )
      {
        # if the source dict has NA for it
        if(is.na(dict_source[[ 'Borrower_State' ]])){
          dict_goal[[ lookup_name ]] <- NA
        }
        # The last 5 digits of the name of indicator
        else if (substr(lookup_name, 15,17)  ==   dict_source[[ 'Borrower_State' ]])
        {
          dict_goal[[ lookup_name ]] <- 1L
        }
        else{
          dict_goal[[ lookup_name ]] <- 0L
        }
      }
      
      # if the variable is Borrower_Classification indicator
      else if ( length(grep('Borrower_Classification',lookup_name)) >0 )
      {
        # if the source dict has NA for it
        if(is.na(dict_source[[ 'Borrower_Classification' ]])){
          dict_goal[[ lookup_name ]] <- NA
        }
        # The last 5 digits of the name of indicator
        else if (substr(lookup_name, 24, 1000)  ==   dict_source[[ 'Borrower_Classification' ]])
        {
          dict_goal[[ lookup_name ]] <- 1L
        }
        else{
          dict_goal[[ lookup_name ]] <- 0L
        }
      }
      
      # if the variable is Borrower_SubTerm indicator
      else if ( length(grep('Borrower_SubTerm',lookup_name)) >0 )
      {
        # if the source dict has NA for it
        if(is.na(dict_source[[ 'Borrower_SubTerm' ]])){
          dict_goal[[ lookup_name ]] <- NA
        }
        
        else if (substr(lookup_name, 17, 1000)  ==   dict_source[[ 'Borrower_SubTerm' ]])
        {
          dict_goal[[ lookup_name ]] <- 1L
        }
        else{
          dict_goal[[ lookup_name ]] <- 0L
        }
      }
      
      # if the variable is risk score indicator (Only B and C showed in the var_list)
      else if ( length(grep('Risk_Score',lookup_name)) >0 )
      {
        # if the source dict has NA for it
        if(is.na(dict_source[[ 'Risk_Score' ]])){
          dict_goal[[ lookup_name ]] <- NA
        }
        else if(dict_source[[ 'Risk_Score' ]] == 'NA'){
          dict_goal[[ lookup_name ]] <- NA
        }
        else if(substr(lookup_name, 11, 1000) ==   dict_source[[ 'Risk_Score' ]] )
        {
          dict_goal[[ lookup_name ]] <- 1L
        }
        else{
          dict_goal[[ lookup_name ]] <- 0L
        }
      }
      
      
      # if the variable is Agent_State indicator
      else if ( length(grep('Agent_State',lookup_name)) >0 )
      {
        # if the source dict has NA for it
        if(is.na(dict_source[[ 'Agent_State' ]])){
          dict_goal[[ lookup_name ]] <- NA
        }
        # The last 5 digits of the name of indicator
        else if (substr(lookup_name, 12,100)  ==   dict_source[[ 'Agent_State' ]])
        {
          dict_goal[[ lookup_name ]] <- 1L
        }
        else{
          dict_goal[[ lookup_name ]] <- 0L
        }
      }
      
      else if ( length(grep('Agent_ID',lookup_name)) >0 )
      {
        # if the source dict has NA for it
        if(is.na(dict_source[[ 'Agent_ID' ]])){
          dict_goal[[ lookup_name ]] <- NA
        }
        # The last 5 digits of the name of indicator
        else if (substr(lookup_name, 9,100)  ==   dict_source[[ 'Agent_ID' ]])
        {
          dict_goal[[ lookup_name ]] <- 1L
        }
        else{
          dict_goal[[ lookup_name ]] <- 0L
        }
      }
      
      else
      {
       dict_goal[[ names(dict_goal)[j] ]] <- 'NOT found'
      }

    }
    # length(dict_goal)
   
    x_new <- matrix(nrow = 1, ncol = length(dict_goal))
    colnames(x_new) <- var_new
    for (q in 1: ncol(x_new)){
      x_new[1, q] <-  as.numeric(dict_goal[[ colnames(x_new)[q] ]])
    }
    # x_new
    xgb_model <- readRDS('model_0413_v2.RDS')
    options(scipen = 999) # For rounding
    pred_prob <- round(predict(xgb_model, x_new), 4)
    pred_contr <- predict(xgb_model, x_new, predcontrib = T)
    contr1 <- pred_contr[1,]
    contr1 <- contr1[-length(contr1)] # drop BIAS
    contr1 <- contr1[order(abs(contr1), decreasing = T)]
    contr1 <- contr1[1:10]   
    df <- data.frame(var_name = names(contr1), 
                     values = contr1)
    list(df, pred_prob)
  })
  
  output$plot <- renderPlot({
    ggplot(AllInputs()[[1]], aes(x=reorder(var_name, abs(values)), y=values)) + 
      geom_bar(stat = "identity", fill = '#EC7216') +
      coord_flip() +
      theme_minimal() +
      theme(axis.text=element_text(size=14), 
            axis.title=element_text(size=16),
            # axis.text.x = element_text(size=26),
            axis.text.y = element_text(size=18),
            title = element_text(size = 16)
            ) +
      labs(x='', y = 'feature contribution to log odds')
      
    
  })
  
  output$print_text <- renderText(
    paste0("The loan cancellation risk score for this transaction is ",
           round(AllInputs()[[2]], 4))
  )
  
  
  # RUN the model
  
  # url <- ('https://raw.githubusercontent.com/HuaiyuZhang/data_structure/master/model_try.RDS')
  # download.file(url, 'try.RDS', method = 'curl')
  
  
  
  
}
