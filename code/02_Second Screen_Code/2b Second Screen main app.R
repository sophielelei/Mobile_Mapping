

rm(list=ls())
#=======================================================================
# HOW TO RUN
# Click run app on the top right
# Click "next" once to precede to screening. 
# When you get bored close it down and everything is already saved - then upload to git
# To start again, just select all and run again.
#=======================================================================

#-----------------------------------------------------------------------
# AS A ONE OFF - CHANGE THIS TO THE FILE YOU ARE WORKING ON
# easiest way is to copy paste file.choose() into the console
# choose the file and it will type out the location
#-----------------------------------------------------------------------
#Workingfile <- "~/Desktop/mobile_money/M_mapping/data/MainScreeningData_Screen2.rData"
#Workingfile <- "/Users/hlg5155/Documents/GitHub/greatrex-lab/Project Bibliometric Mapping/Mobile_Mapping/data/MainScreeningData_Screen2.rData"
Workingfile <- "REPLACE WITH YOURS"
   
#-----------------------------------------------------------------------
# Make sure you have these libraries
#-----------------------------------------------------------------------
#Analysis libraries
library(DT)      ;  library(magrittr)
library(stringr) ;  library(tidyverse)
library(RISmed)  ;  library(bibliometrix)
library(bib2df)  ;  library(knitr)

#Shiny libraries
library(shiny)   ;  library(rmdformats)
library(fresh)   ;  library(shinythemes)  
library(shinyWidgets)
library(shinyEffects)
library(shinydashboard)
library(shinydashboardPlus)

#-----------------------------------------------------------------------
# THEMES!  
# Here is where you can edit all the colors/fonts/anything you want 
# Then the space where you can edit the word  highlighting colors
# Also see styles.css in the www sub-directory for the accordion menu colors
#-----------------------------------------------------------------------
#--------------------------------------- 
# Create a custom theme
#--------------------------------------- 
mytheme <- create_theme(
   #--------------------------------------- 
   # These are the 'standard color names' in shiny 
   # I've set them to the default, but you can edit as needed
   #--------------------------------------- 
   adminlte_color(
      light_blue = "#3c8dbc", # try #434C5E for example
      red        = "#dd4b39",
      green      = "#00a65a",
      aqua       = "#00c0ef",
      yellow     = "#f39c12",
      blue       = "#001F3F",
      navy       = "#001F3F",
      teal       = "#39CCCC",
      olive      = "#3D9970",
      lime       = "#01FF70",
      orange     = "#FF851B",
      fuchsia    = "#F012BE",
      purple     = "#605ca8",
      maroon     = "#D81B60",
      gray_lte   = "#d2d6de",
   ),
   # You can also set individual colors for any individual thing
   adminlte_sidebar(
      width = "600px", # Make sure this is 25 more than the width in the UI code
      #dark_bg = "#D8DEE9",
      #dark_hover_bg = "#81A1C1",
      #dark_color = "#2E3440"
   ),
   adminlte_global(
      #content_bg = "#FFF",
      #box_bg = "#D8DEE9", 
      #info_box_bg = "#D8DEE9"
   )
) 

#---------------------------------------
# HIGHLIGHTING GROUPS
#---------------------------------------
highlight_groups <- list(
   list(
      name = "finance",
      color = "#fdf29a",
      words = c("pesa", "money", "cash", "credit", "currency", "funds", 
                "payment", "remittance", "wealth", "income", "savings", 
                "transaction", "wage gap", "wage", "financial access", 
                "financial service", "financial inclusion", "banking", 
                "financial", "economy", "funding")
   ),
   list(
      name = "technology",
      color = "#d4f9c6",
      words = c("mobile", "digitization", "technology", "smartphones", "phone", 
                "tech", "technologies", "digital", "online", "knowledge networks", 
                "internet", "ict", "e-commerce", "artificial intelligence", 
                "platform", "platforms", "social media", "computer", "laptop", 
                "laptops", "tool", "tools", "network")
   ),
   list(
      name = "geography",
      color = "#b7e9f6",
      words = c("Kenya", "Africa", "East-Africa", "Sub-Saharan Africa", 
                "sub saharan", "Sub Sahara", "West", "North", "South", 
                "East", "Global South", "Global North", "Uganda", 
                "Tanzania", "Rwanda", "Ethiopia", "Nigeria", "Ghana")
   ),
   list(
      name = "gender",
      color = "#e7d0f5",
      words = c("gender", "female", "women", "girl", "female employment", 
                "women's income", "equality", "feminism", "empowerment", 
                "equity", "female-led", "women rights", "education", 
                "gender mainstreaming", "Gen", "Fem", "Gir")
   ),
   list(
      name = "entrepreneurship",
      color = "#ffd497",
      words = c("self-employment", "entrepreneurial", "entrepreneur", 
                "entrepreneurs", "entrepreneurship", "entrepr", "service", 
                "start-ups", "startup", "grassroot", "small-scale", 
                "large-scale", "enterprise", "companies", "compan", 
                "business", "trade", "work")
   ),
   list(
      name = "redflags",
      color = "#fbd1d1",
      words = c("systematic review", "clinic", "agri", "farmer", "herder", 
                "pasture", "maternal", "neo", "health")
   )
)



#=======================================================================
# DATA PREPROCESSING
#=======================================================================
#-----------------------------------------------------------------------
# Load the data, this will load "data_bib" in the environment.
# you can click on it and take a look
#-----------------------------------------------------------------------
load(Workingfile)

#-----------------------------------------------------------------------
# Sort so screened data is at the bottom
#-----------------------------------------------------------------------
data_bib <- data_bib[with(data_bib, order(Screen2_Assessed)), ]

#-----------------------------------------------------------------------
# Set up the text highlighting
#-----------------------------------------------------------------------
sophiehighlight <- function(text) {
   for (group in highlight_groups) {
      color <- group$color
      words <- group$words
      
      for (word in words) {
         pattern <- regex(word, ignore_case = TRUE)
         replacement <- function(match) {
            paste0('<span style="background-color:', color, '">', match, '</span>')
         }
         text <- str_replace_all(text, pattern, replacement)
      }
   }
   
   return(text)
}

#=======================================================================
# GUI - Front-end. Adjust themes above, other than "skin", which ALSO adjusts the themes
# see here - https://rinterface.github.io/shinydashboardPlus/articles/more-skins.html 
# and here - https://www.inwt-statistics.com/blog/best-practice-development-of-robust-shiny-dashboards-as-r-packages 
# (this is where the one in themes came from)
#=======================================================================
ui = dashboardPage(
   skin="blue",
   title = "Accordion Test",
   freshTheme=mytheme,
   header=dashboardHeader(),
   sidebar=dashboardSidebar(collapsed=TRUE),
   controlbar=dashboardControlbar(width=575,collapsed=FALSE,overlay=FALSE,
                                  
                                  hr(),
                                  
                                  ## Button_Discard
                                  materialSwitch(
                                     inputId = "Button_Discard",
                                     label = "CLICK IF PAPER ISN'T RELEVANT",
                                     value = FALSE,
                                     status = "danger"
                                  ),
                                  
                                  #--------------------------------------------
                                  # METHODS THREE WAYS OF DOING THIS! 
                                  # CHOOSE YOUR FAVOURITE 
                                  # AND COMMENT OUT/REMOVE THE REST
                                  #--------------------------------------------
                                  ## Button_Methods
                                  virtualSelectInput(
                                     inputId = "Button_Methods",
                                     label = "METHODS:",
                                     choices = list(
                                        "CONCEPTUAL" = c(
                                           "Literature review"=1,
                                           "Framework/Opinion-piece"=2
                                        ),
                                        "QUALITATIVE" = c(
                                           "Interview",
                                           "Survey",
                                           "Focus-groups",
                                           "Economic Research games"
                                        ),
                                        "ARCHIVE/TEXT BASED" = c(
                                           "Archival research (newspapers etc)",
                                           "Policy Analysis",
                                           "Content/Thematic Analysis"
                                        ),
                                        "STATISTICS" = c(
                                           "Randomised Controlled Trial",
                                           "Other statistical analysis (regression etc)"
                                        ),
                                        "MODELLING/GIS" = c(
                                           "Computer simulation/modelling",
                                           "GIS/mapping",
                                           "Big-data analysis (social media)"
                                        ) ),
                                     search = TRUE,
                                     multiple = TRUE,
                                     disableOptionGroupCheckbox = TRUE,
                                     width = "100%",   
                                     showValueAsTags = TRUE
                                  ),
                                  
                                  #--------------------------------------------
                                  # DFI THEME
                                  #--------------------------------------------
                                  # Button_DFITheme
                                  checkboxGroupButtons(
                                     inputId = "Button_DFITheme",
                                     label = "DFI THEME:",
                                     choices = c("Mobile Money", 
                                                 "Gender",
                                                 "Financial Inclusion",
                                                 "Entrepeneurship",
                                                 "Technology"),
                                     status = "primary",
                                     individual = TRUE
                                  ),
                                  
                                  #accordion(
                                  #   id = "accordion_gender",
                                  #   accordionItem(
                                  #      title = "--- Gender -------------------------------------------",
                                  #      collapsed = TRUE,
                                  #      status = "primary",
                                  
                                  #--------------------------------------------
                                  # GAD THEME
                                  #--------------------------------------------
                                  # Button_GAD
                                  shinyWidgets::pickerInput(
                                     inputId = "Button_GAD",
                                     label = "GAD THEME:",
                                     choices = c("Changing power relations between women & men" = 1, 
                                                 "Structural barriers to financial inclusion" = 2, 
                                                 "Enabling agency / empowerment" = 3,
                                                 "Changing Gender Roles" = 4,
                                                 "Access / control over resources" = 5, 
                                                 "Risk, security, and violence" = 6, 
                                                 "Gender & Youth" = 7, 
                                                 "Gender & Elderly" = 8),
                                     multiple = TRUE,
                                     width = "fit",
                                     options = list( `multiple-separator` = " | ",
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count",
                                                     `pickerOptions(container` = "body")
                                  ),
                                  
                                  #--------------------------------------------
                                  # TECHNOLOGY TYPE
                                  #--------------------------------------------
                                  # Button_TechType
                                  shinyWidgets::pickerInput(
                                     inputId = "Button_TechType",
                                     label = "TECH TYPE:",
                                     choices = c("Phone" = 1, 
                                                 "Computer" = 2, 
                                                 "Not Specified" = 3),
                                     multiple = TRUE,
                                     width = "fit",
                                     options = list( `multiple-separator` = " | ",
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count",
                                                     `pickerOptions(container` = "body")
                                  ),
                                  
                                  #),
                                  #accordionItem(
                                  #   title = "--- Finance -------------------------------------------",
                                  #   status = "warning",
                                  #   collapsed = FALSE,
                                  #   "This is some text!"
                                  #--------------------------------------------
                                  # INCLUSIVE DEVELOPMENT THEMES
                                  #--------------------------------------------
                                  # Button_IncDev
                                  shinyWidgets::pickerInput(
                                     inputId = "Button_IncDev",
                                     label = "Inclusive Dev", 
                                     choices = c("Economic Empowerment", 
                                                 "Gender Equality & Social Inclusion (GESI)", 
                                                 "Poverty Reducation",
                                                 "Informal Economy",
                                                 "Social Norms & Cultural Factors"),
                                     multiple = TRUE,
                                     width = "fit",
                                     options = list( `multiple-separator` = " | ",
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count",
                                                     `pickerOptions(container` = "body")
                                  ),
                                  
                                  #)
                                  
                                  #--------------------------------------------
                                  # MOBILE MONEY TYPE
                                  #--------------------------------------------
                                  # Button_MMType
                                  shinyWidgets::pickerInput(
                                     inputId = "Button_MMType",
                                     label = "Mobile Money", 
                                     choices = list("MPesa" = 1, 
                                                    "Borrow" = 2, 
                                                    "Remittance" = 3,
                                                    "Credit" = 4,
                                                    "Women's Savings Group Use" = 5,
                                                    "Transactions"= 6,
                                                    "Access to Government Money" = 7,
                                                    "Access to NGOs" = 8,
                                                    "Platform Visibility (e.g., Influencer)" = 9, 
                                                    "Access to International Markets" = 10, 
                                                    "Link to Advertising/Communication" = 11, 
                                                    "Other [leave a note]" = 12, 
                                                    "Not Specified" = 13),
                                     multiple = TRUE,
                                     width = "fit",
                                     options = list( `multiple-separator` = " | ",
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count",
                                                     `pickerOptions(container` = "body")
                                  ),
                                  #--------------------------------------------
                                  #ENTREPRENEUR TYPE WIDGET
                                  #--------------------------------------------
                                  # Button_EntrePType
                                   shinyWidgets::pickerInput(
                                     inputId = "Button_EntrePType",
                                     label = "Entrepreneur Type:",
                                     choices = c("Microentrepreneurs, Small-Scale Traders, Market Stalls (Using MM for transactions)" = 1, 
                                                 "SMEs(e.g., Salons, Own Establishment Building; Using MM for transactions, supply chains, social media, advertising, brand partnerships)" = 2, 
                                                 "Collective/Cooperative Entrepreneurship (Women groups selling on Etsy; Using MM for group savings, loans, collective investments)" = 3,
                                                 "Accessing new entrepreneurship opportunities becasue of mobile platforms;livelihoods dependent on MM (e.g Individual Influencer/Uber driver)" = 4,
                                                 "Bigger Corperations" = 5), 
                                     multiple = TRUE,
                                     width = "fit",
                                     options = list( `multiple-separator` = " | ",
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count",
                                                     `pickerOptions(container` = "body")
                                  ),
                                  #--------------------------------------------
                                  #ENTREPRENEUR SECTOR WIDGET 
                                  #--------------------------------------------
                                  # Button_EntrePSec
                                   shinyWidgets::pickerInput(
                                     inputId = "Button_EntrePSec",
                                     label = "Entrepreneur Sector:",
                                     choices = c("Agriculture" = 1, 
                                                 "Beauty/Wellness" = 2, 
                                                 "Creative/Film" = 3,
                                                 "Handicrafts" = 4),
                                     multiple = TRUE,
                                     width = "fit",
                                     options = list( `multiple-separator` = " | ",
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count",
                                                     `pickerOptions(container` = "body")
                                  ),
                                  
                                  #--------------------------------------------
                                  # GEOGRAPHY WIDGET 
                                  #--------------------------------------------
                                  # Button_Geography
                                  shinyWidgets::pickerInput(
                                     inputId = "Button_Geography",
                                     label = "Geography:",
                                     choices = c("East-Africa" = 1, 
                                                 "Rest of Africa" = 2, 
                                                 "Not Africa 'Global South'" = 3,
                                                 "Study is clearly based in a rural location" = 4, 
                                                 "Study is clearly based in a city" = 5, 
                                                 "Not specified"= 6),
                                     multiple = TRUE,
                                     width = "fit",
                                     options = list( `multiple-separator` = " | ",
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count",
                                                     `pickerOptions(container` = "body")
                                  ),
                                 
                                  #OTHER WIDGET 
                                  shinyWidgets::pickerInput(
                                     inputId = "Button_Other",
                                     label = "Other Themes:",
                                     choices = c("Training/Education" = 1, 
                                                 "Policy" = 2, 
                                                 "Climate Change" = 3,
                                                 "Sustainability" = 4),
                                     multiple = TRUE,
                                     width = "fit",
                                     options = list( `multiple-separator` = " | ",
                                                     `actions-box` = TRUE,
                                                     `selected-text-format` = "count",
                                                     `pickerOptions(container` = "body")
                                  ),
                                  #--------------------------------------------------------------------
                                  # Notes
                                  fluidRow(textInput(inputId = "notesField", label = "Notes", value = "")),
                                  
                                  #--------------------------------------------------------------------
                                  # The next button
                                  fluidRow(actionButton("nextButton", "Next", width = "100px", 
                                                        style="color: #fff; background-color: #4bbf73; border-color: #2e6da4"))
   ),
   # MAIN BODY--------------------------------------------
   body=dashboardBody(
      includeCSS("www/styles.css"), # Link to style sheet
      DT::dataTableOutput("table"),
      hr(),
      # More info on screening classifications
      verbatimTextOutput("moreInfo"),
      htmlOutput("count"),
      htmlOutput("total")      
   ),
)


#=======================================================================
# Server
#=======================================================================
server <-  function(input,output,session){
   
   #--------------------------------------------------------------------
   # Create a "reactive value" which allows us to play with the output of a button click
   values <- reactiveValues(); values$count <- 0
   
   #--------------------------------------------------------------------
   # At the same time on a next click (bloody shiny), 
   # select the row you care about and highlight it
   
   highlighter <- eventReactive( 
      # not sure if the problems with the update button have to do with eventReactive, but most of my googling for the error
      # lead me to conversations about ignoreNull and 
      # some of these buttons will update from the database if there is a value there (others will not because i couldnt
      # figure out these update buttons)
      {input$nextButton}, #ignoreNULL = FALSE, ignoreInit = FALSE,
      {
         updateMaterialSwitch(session=session, inputId="Button_Discard",value=FALSE)
         updateVirtualSelect(session=session, inputId="Button_Methods", selected = character(0))
         updateCheckboxGroupButtons(session=session, inputId="Button_DFITheme", selected = character(0))
         updatePickerInput(session=session, inputId="Button_GAD", selected = character(0))
         updatePickerInput(session=session, inputId="Button_TechType", selected = character(0))
         updatePickerInput(session=session, inputId="Button_IncDev", selected = 0)
         updatePickerInput(session=session, inputId="Button_MMType", selected = character(0))
         updatePickerInput(session=session, inputId="Button_EntrePType", selected = 0)
         updatePickerInput(session=session, inputId="Button_EntrePSec", selected = 0)
         updatePickerInput(session=session, inputId="Button_Geography", selected = 0)
         updatePickerInput(session=session, inputId="Button_Other", selected = 0)
         updateTextInput(session=session, inputId="notesField", value = "")
         
         save(list="data_bib",file=Workingfile)
         
         #-----------------------------------------------------
         # If the row number is not at the end, increment up
         # THIS IS *REALLY BAD CODING*, ADDED IN BECAUSE IT WANTS TO RECALCULATE THE VALUE.
         # if(sum(c(input$discardButton,input$rainButton,input$modelButton, input$socialButton))>0){
         if(values$count != nrow(data_bib)){
            #-----------------------------------------------------
            # move to the next row
            values$count <- values$count + 1
            
            #-----------------------------------------------------
            # choose that row in the table
            YourData <- data_bib[values$count,c("TI","AB")]
            YourData2 <- sophiehighlight(YourData)
            
            #-----------------------------------------------------
            # Output to data_bib         
            data_bib$Screen2_Assessed    [values$count-1] <<- TRUE
            data_bib$Screen2_Reject      [values$count-1] <<- input$Button_Discard
            
            # Button_Methods
            if (length(input$Button_Methods) <= 0) {
               data_bib$Screen2_Methods[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_Methods[values$count - 1] <<- str_c(input$Button_Methods, collapse = "_")
            }
            
            # Button_DFITheme
            if (length(input$Button_DFITheme) <= 0) {
               data_bib$Screen2_DFITheme[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_DFITheme[values$count - 1] <<- str_c(input$Button_DFITheme, collapse = "_")
            } 
            
            # Button_GAD
            if (length(input$Button_GAD) <= 0) {
               data_bib$Screen2_GAD[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_GAD[values$count - 1] <<- str_c(input$Button_GAD, collapse = "_")
            }
            
            # Button_TechType
            if (length(input$Button_TechType) <= 0) {
               data_bib$Screen2_TechType[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_TechType[values$count - 1] <<- str_c(input$Button_TechType, collapse = "_")
            }              
           
            # Button_IncDev
            if (length(input$Button_GAD) <= 0) {
               data_bib$Screen2_IncDev[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_IncDev[values$count - 1] <<- str_c(input$Button_IncDev, collapse = "_")
            }
            
            # Button_MMType
            if (length(input$Button_MMType) <= 0) {
               data_bib$Screen2_MMType[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_MMType[values$count - 1] <<- str_c(input$Button_MMType, collapse = "_")
            }  
            
            # Button_EntrePType
            if (length(input$Button_EntrePType) <= 0) {
               data_bib$Screen2_EntrePType[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_EntrePType[values$count - 1] <<- str_c(input$Button_EntrePType, collapse = "_")
            }
            
            # Button_EntrePSec
            if (length(input$Button_EntrePSec) <= 0) {
               data_bib$Screen2_EntrePSec[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_EntrePSec[values$count - 1] <<- str_c(input$Button_EntrePSec, collapse = "_")
            }             
            
            # Button_Geography
            if (length(input$Button_Geography) <= 0) {
               data_bib$Screen2_Geography[values$count - 1] <<- 0
            } else {
               data_bib$Screen2_Geography[values$count - 1] <<- str_c(input$Button_Geography, collapse = "_")
            }      
            
            # notes
            data_bib$Screen2_Notes       [values$count-1] <<- input$notesField
            return(YourData2)
         }
         
         
         #-----------------------------------------------------
         # Or put the final row
         else{
            YourData <- data_bib[ nrow(data_bib),c("TI","AB")]
            YourData2 <- sophiehighlight(YourData)
            
            #-----------------------------------------------------
            # Output to data_bib         
            data_bib$Screen2_Assessed   [nrow(data_bib)] <<- TRUE
            data_bib$Screen2_Reject     [nrow(data_bib)] <<- input$discardButton
            data_bib$Screen2_Methods    [nrow(data_bib)] <<- str_c(input$Button_Methods, collapse = '_')
            data_bib$Screen2_DFITheme   [nrow(data_bib)] <<- str_c(input$Button_DFITheme, collapse = '_')
            data_bib$Screen2_GAD        [nrow(data_bib)] <<- str_c(input$Button_GAD, collapse = '_')
            data_bib$Screen2_TechType   [nrow(data_bib)] <<- str_c(input$Button_TechType, collapse = '_')
            data_bib$Screen2_IncDev     [nrow(data_bib)] <<- str_c(input$Button_IncDev, collapse = '_')
            data_bib$Screen2_MMType     [nrow(data_bib)] <<- str_c(input$Button_MMType, collapse = '_')
            data_bib$Screen2_EntrePType [nrow(data_bib)] <<- str_c(input$Button_EntrePType, collapse = '_')
            data_bib$Screen2_EntrePSec  [nrow(data_bib)] <<- str_c(input$Button_EntrePSec, collapse = '_')
            data_bib$Screen2_Geography  [nrow(data_bib)] <<- str_c(input$Button_Geography, collapse = '_')
            data_bib$Screen2_Notes      [nrow(data_bib)] <<- input$notesField
            
            
    
            return(YourData2)
         }
      })  
   
   #--------------------------------------------------------------------
   # Output the table to the GUI
   
   output$table <- DT::renderDataTable({ data <- highlighter()}, escape = FALSE,options = list(dom = 't',bSort=FALSE)) 
   output$covidence <- renderUI({HTML(paste(highlightCovidence(), sep = '<br/>'))})
   output$reject <- renderUI({ HTML(paste(highlightExclude(), sep = '<br/>'))})
   
   #--------------------------------------------------------------------
   # More info about the screening selections
   hr()
   output$moreInfo <- renderText({
      paste(
         "Assessment Definitions:",
         "• Microentrepreneurs, Small-Scale Traders, Market Stalls (Using MM for transactions)",
         "• SMEs (e.g., Salons, Own Establishment Building; Using MM for transactions, supply chains, social media, advertising, brand partnerships)",
         "• Collective/Cooperative Entrepreneurship (Women groups selling on Etsy; Using MM for group savings, loans, collective investments)",
         "• Accessing new entrepreneurship opportunities because of mobile platforms; livelihoods dependent on MM (e.g. Individual Influencer, Uber driver)",
         "• Bigger Corporations",
         sep = "\n"
      )
   })
   hr()
   output$count <- renderUI({ HTML(paste("You have reviewed", (values$count - 2) ,"papers in this session")) })
   output$total <- renderUI({HTML(paste("In total, we have reviewed", (sum(data_bib$Screen2_Assessed, na.rm = TRUE)), "of", (length(data_bib$Screen2_Assessed))))})
}

#=======================================================================
# Run the server
#=======================================================================
shinyApp(ui = ui, server = server)

