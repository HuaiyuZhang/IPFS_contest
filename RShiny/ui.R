library(shiny)
library(shinyWidgets)
library(ggplot2)

fluidPage(
  ####### THE PLOT ##########
  fluidRow(column(width = 2),
           mainPanel(
             # h1("Loan Cancellation Prediction"),
             h2(textOutput('print_text')),
             
             plotOutput('plot')
           ) 
  ),
  ######## THE INPUT AREA############
  fluidRow(column(width = 4),
           tabsetPanel(
             tabPanel(
               "Transaction information", 
               
               splitLayout(
                 
                 dateInput("Accepted_Date", 
                           "Accepted_Date:",
                           value = '2017-05-25'),
                 dateInput("Mature_Date", 
                           "Mature_Date:",
                           value = '2018-03-25'),
                 numericInput(inputId = "Default_Charge",
                              label = "Default_Charge",
                              value = 100),
                 numericInput(inputId = "Late_Charge",
                              label = "Late_Charge:",
                              value = 6),
                 numericInput(inputId = "NSF_Charge",
                              label = "NSF_Charge:",
                              value = 0),
                 numericInput(inputId = "Premium",
                              label = "Premium:",
                              value = 0)
               ),
               splitLayout(
                 numericInput(inputId = "Down",
                              label = "Down",
                              value = 357.9),
                 
                 
                 numericInput(inputId = "APR",
                              label = "APR",
                              value = 13.59),
                 numericInput(inputId = "Term",
                              label = "Term",
                              value = 10),
                 
                 numericInput(inputId = "Payments_Rcvd",
                              label = "Payments_Rcvd",
                              value = 9),
                 numericInput(inputId = "AmtFin",
                              label = "AmtFin",
                              value = 2028.1),
                 numericInput(inputId = "FinChg",
                              label = "FinChg:",
                              value = 128.5)
                 
               ),
               splitLayout(
                 dateInput("NoticeOfIntentToCancelDate", 
                           "NoticeOfIntentToCancelDate",
                           value = '2017-07-30'
                 )
               )
               
             ),
             tabPanel(
               "Borrower Information",
               splitLayout(
                 selectInput(inputId = "Borrower_State",
                             label = "Borrower_State:",
                             choices = c('CA','TX','NC','FL','LA','GA','KY','AZ','NY','OR','UT','MS','TN','NJ','NV','MA','VA','OK','AL','CO','MT','PA','AR','NM','WY','IL','OH','DE','MO','SC','KS','DC','MI','ID','WA','WV','IN','CT','SD','AB','ND','WI','VI','IA','MN','ME','NE','ON','MD','NH','RI','PR','MP','VT','BC','HI','AK')),
                 textInput(inputId = "Borrower_Zip",
                           label = "Borrower_Zip:",
                           value = NA),
                 checkboxInput("Borrower_EnrolledInEForms", 
                               "Borrower_EnrolledInEForms" ),
                 
                 numericInput(inputId = "Exposure",
                              label = "Exposure:",
                              value = 0)
               ),
               splitLayout(
                 
                 
                 checkboxInput("Borrower_RegisteredForEForms", 
                               "Borrower_RegisteredForEForms" ),
                 
                 dateInput("Borrower_DateRegisteredForEForms", 
                           "Borrower_DateRegisteredForEForms:",
                           value = '2017-01-01'),
                 checkboxInput("Borrower_RegisteredOnWeb", 
                               "Borrower_RegisteredOnWeb" ),
                 dateInput("Borrower_DateRegisteredForWeb", 
                           "Borrower_DateRegisteredForWeb:",
                           value = '2017-01-01')
                 
                         ),
               splitLayout(
                 
                 checkboxInput("Borrower_RegisteredForCancellationWarning", 
                               "Borrower_RegisteredForCancellationWarning" ),
                 
                 dateInput("Borrower_DATERegisteredForCancellationWarning", 
                           "Borrower_DATERegisteredForCancellationWarning:",
                           value = '2017-01-01'),
                 
                 
                 
                 selectInput(inputId = "BankruptcyFlag",
                             label = "BankruptcyFlag:",
                             choices = c(NA, 'Can Cancel Stay Lifted', 'Cannot Cancel Bankruptcy')),
                 checkboxInput("RecurringACH_TF", 
                               "RecurringACH_TF" )),
               splitLayout(
                 selectInput(inputId = "Borrower_Classification",
                             label = "Borrower_Classification:",
                             choices = c('Services','Real Estate','Transportation','General','Construction','Retail Trade','Agriculture','Government','Manufacturing','Mining','Wholesale Trade')),
                 
                 selectInput(inputId = "Borrower_SubTerm",
                             label = "Borrower_SubTerm:",
                             choices = c('General Other','Trucking','Personal DBA','General Construction','Real Estate','General Services','General Transportation','General Fin/Ins/Real','Roofing','Medical','Marine','Locations','Technical Assortment','General Blended','Finance','City','Organization','Church','Auto Retail Trade','Restaurant/Catering','Professional Services','Specialist','Grocery','Recreational Services','Cleaning Services','Oil','Builder','Terraforming','Salvage','Educational Services','Forestry','General Retail','Legal','Drilling','School','Metallic','General Manufacturing','Mechanical','Insurance','Ranching','Grooming Services','Sporting Goods','Nonmetallic','Structure','Communications','General Wholesale','Distributor','Aviation','County','Materials','Alcohol','Taxi/Limo Livery','Janitorial Services','Resources','Operations','Farming','Textile','Directional','Gas','Excavation','Agriculture','Town','Coating/Plating','Importer','Parts','Chemical','Fitness Services','Exploration','Road Work','General Mining','Exporter','Retail Jewelery','Fishing','Well','Congress','Wireline','Parish','Tribe','General Government','Sanitary Service','Nation','Geophysical')),
                 selectInput(inputId = "Borrower_CreditScore",
                             label = "Borrower_CreditScore:",
                             choices = c(0, 1, 2, 3, 4, 5)
                 ),
                 
                 selectInput(inputId = "Risk_Score",
                             label = "Risk_Score:",
                             choices = c('NA','A','B','C','D'))
               )
               
             ),
             
             tabPanel(
               "Agent Information",
               
               splitLayout(
                 textInput(inputId = "Agent_ID",
                           label = "Agent_ID:",
                           value = NA),
                 selectInput(inputId = "Agent_State",
                             label = "Agent_State:",
                             choices = c( 'TX','MS','CO','LA','OK','AR','AL','TN','NM','YT','FL','NE','MO','IN','GA','CA','AB')),
                 checkboxInput("Agent_IntentEForm",
                               "Agent_IntentEForm" )
                 # switchInput(inputId = "Agent_IntentEForm","Agent_IntentEForm", value = F)
               ),
               splitLayout(
                 
                 
                 checkboxInput("Agent_CancelEForm", 
                               "Agent_CancelEForm" ),
                 checkboxInput("Agent_ReinstateEForm", 
                               "Agent_ReinstateEForm" ),
                 checkboxInput("Agent_PendingPolicyRequestEForm", 
                               "Agent_PendingPolicyRequestEForm" ),
                 checkboxInput("Agent_ACHPaymentLetterEForm", 
                               "Agent_ACHPaymentLetterEForm" ),
                 checkboxInput("Agent_RecurringACHForm", 
                               "Agent_RecurringACHForm" )
               ),
               
               splitLayout(
                 
                 checkboxInput("Agent_RegisteredForCancelListReport", 
                               "Agent_RegisteredForCancelListReport" ),
                 checkboxInput("Agent_RegisteredForEimpendingReport", 
                               "Agent_RegisteredForEimpendingReport" ),
                 
                 checkboxInput("Agent_EnrolledInCreditProgram", 
                               "Agent_EnrolledInCreditProgram" ),
                 
                 checkboxInput("Agent_DownPaymentViaCC", 
                               "Agent_DownPaymentViaCC" ),
                 checkboxInput("Agent_DownPaymentViaACH", 
                               "Agent_DownPaymentViaACH" )
                 
                 
               )
               
               
               
               
             )
             
             
           )
           
           
        )
  
)

    
    
  
