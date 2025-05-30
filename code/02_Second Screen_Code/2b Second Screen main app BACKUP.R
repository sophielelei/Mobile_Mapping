library(shiny)   ;  library(tidyverse)
library(DT)      ;  library(magrittr)
library(stringr) ;  library(shinyWidgets)
library(RISmed)  ;  library(bibliometrix)
library(bib2df)  ;  library(knitr)
library(shinythemes)
library(rmdformats)



# THIS CODE OVERWRITES YOUR INPUT DATA. MAKE A COPY BEFORE YOU START 
# (or even better - immedietly after you finish screening)
# Place backup in data/03_backup-screening-data 
# title backup file 'XXXX-screeningData-YY'
# XXXX - month and day 
# YY - initials

#=======================================================================
# How to run
# Press Ctrl-A to select all, then run.  Shiny should start up
# Click "next" once to precede to screening. 
# When you get bored close it down and everything is already saved - then upload to git
# To start again, just select all and run again.
#=======================================================================

rm(list=ls())
# adjust this to the appropriate folder on your system
Workingfile <- "~/Desktop/mobile_money/M_mapping/data/MainScreeningData_Screen2.rData"
#Workingfile <- "/Users/hlg5155/Documents/GitHub/greatrex-lab/Project Bibliometric Mapping/Mobile_Mapping/data/MainScreeningData_Screen2.rData"

load(Workingfile)

#=======================================================================
# Sort so screened data is at the bottom
#=======================================================================
## extra check to make sure screen 1 is complete - 
## everything in Screen1_Assessed is either true or false
data_bib <- data_bib[with(data_bib, order(Screen2_Assessed)), ]


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

#Check up to here (everything works!) - March 31 2025 

#=======================================================================
# GUI function
#=======================================================================
ui <- fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #9c8484;
         }
         
         #table {
            background-color: #f4f3ef;
            font-size: 12px; /* Increase font size here */
         }

        '))),
  #--------------------------------------------------------------------
  # Feel free to change the theme to serve your preferences, you'll be staring at this for a while
  # so might as well make it look good.
  #theme = shinytheme("flatly"),
  theme = 'bootstrap.css',
  #--------------------------------------------------------------------
  # This creates a shiny sidebar.  There are other options
  sidebarLayout(
    sidebarPanel(id = "sidebar",
      
      #--------------------------------------------------------------------
      # Not Relevent, we made a mistake
      fluidRow(
        materialSwitch(inputId="discardButton", label="Not Relevant",value=FALSE,width="100%",status="danger")),
      
      #--------------------------------------------------------------------
      # DFI Theme
      fluidRow(
        checkboxGroupButtons(inputId = "DFI_Theme", 
                           label = h5("DFI Theme"), 
                           choices = c("Mobile Money" = 1, 
                                       "Gender" = 2, 
                                       "Financial Inclusion" = 3,
                                       "EntreP" = 4,
                                       "Technology" = 5),
                           checkIcon = list(
                             yes = tags$i(class = "fa fa-circle", 
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-circle-o", 
                                         style = "color: steelblue")),
                           status = "info",
                           selected = NULL)),
      #--------------------------------------------------------------------
      # GAD Theme
      fluidRow(
         checkboxGroupButtons(inputId = "GAD_Theme", 
                              label = h5("GAD Theme"), 
                              choices = c("Changing Power Relations Between Women & Men" = 1, 
                                          "Structural Barriers to Financial Inclusion" = 2, 
                                          "Enabling Agency and Empowerment" = 3,
                                          "Transformation of Gender Roles" = 4,
                                          "Access to and Control Over Resources" = 5, 
                                          "Risk, Security, and Violence" = 6, 
                                          "Gender & Youth" = 7, 
                                          "Gender & Elderly" = 8),
                              checkIcon = list(
                                 yes = tags$i(class = "fa fa-circle", 
                                              style = "color: steelblue"),
                                 no = tags$i(class = "fa fa-circle-o", 
                                             style = "color: steelblue")),
                              status = "info",
                              selected = NULL)),
      
      #--------------------------------------------------------------------
      # Inclusive Development Theme
      fluidRow(
         checkboxGroupButtons(inputId = "ID", 
                              label = h5("Inclusive Development"), 
                              choices = c("Economic Empowerment" = 1, 
                                          "Gender Equality & Social Inclusion (GESI)" = 2, 
                                          "Poverty Reducation" = 3,
                                          "Informal Economy" = 4,
                                          "Social Norms & Cultural Factors" = 5),
                              checkIcon = list(
                                 yes = tags$i(class = "fa fa-circle", 
                                              style = "color: steelblue"),
                                 no = tags$i(class = "fa fa-circle-o", 
                                             style = "color: steelblue")),
                              status = "info",
                              selected = NULL)),
      
      #--------------------------------------------------------------------
      # Financial Inclusion Theme
      fluidRow(
         checkboxGroupButtons(inputId = "FI", 
                              label = h5("Financial Inclusion"), 
                              choices = c("Access to Financial Services" = 1, 
                                          "Usage & Adoption Patterns" = 2, 
                                          "Affordability & Costs" = 3,
                                          "Financial Capability & Literacy" = 4,
                                          "Trust, Privacy & Security" = 5), 
                              checkIcon = list(
                                 yes = tags$i(class = "fa fa-circle", 
                                              style = "color: steelblue"),
                                 no = tags$i(class = "fa fa-circle-o", 
                                             style = "color: steelblue")),
                              status = "info",
                              selected = NULL)),     
      
      #--------------------------------------------------------------------
      # Mobile Money (MM) Type
      fluidRow(
         #MM Type    
         checkboxGroupInput("MM_Type", 
                            label = h5("MM Type"), 
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
                            selected = NULL)),
         #--------------------------------------------------------------------
         # Tech Type
         fluidRow(
            #Tech Type    
            checkboxGroupInput("Tech_Type", 
                               label = h5("Tech Type"), 
                               choices = list("Phone" = 1, 
                                              "Computer" = 2, 
                                              "Digital Platforms (i.e. websites)" = 3,
                                              "Not Specified" = 4),
                               selected = NULL)),
            #--------------------------------------------------------------------
            # EntreP Type
            fluidRow(
               #EntreP Type    
               checkboxGroupInput("EntreP_Type", 
                                  label = h5("EntreP Type"), 
                                  choices = list("MicroEntrepreneurs, Small-Scale Traders, Market Stalls(Using MM for transactions)" = 1, 
                                                 "SMEs (e.g., Salons, Own Establishment Buildings)" = 2, 
                                                 "Collective/Cooperative Entrepreneurship (Women groups selling on Etsy; Using MM for group savings, loans, collective investments)" = 3,
                                                 "Individual Influencer/Uber driver, etc. (Accessing new entrepreneurship opportunities becasue of mobile platforms;livelihoods dependent on MM" = 4,
                                                 "Big Corporations" = 5),
                                  selected = NULL)),
               
             
               #--------------------------------------------------------------------
               # EntreP Sector
               fluidRow(
                  #EntreP Sector    
                  checkboxGroupInput("EntreP_Sec", 
                                     label = h5("EntreP Sector"), 
                                     choices = list("Agriculture" = 1, 
                                                    "Hair" = 2, 
                                                    "Film/Creative" = 3,
                                                    "Handicrafts" = 4,
                                                    "Not Specified" = 5),
                                     selected = NULL),
                  
                  #--------------------------------------------------------------------
                  # Methods
                  checkboxGroupInput("methodsGroup", 
                                     label = h5("Methods"), 
                                     choices = list("Literature Review" = 1, 
                                                    "Interviews/surveys"= 2,
                                                    "Social media or crowd sourcing" = 3,
                                                    "Machine learning" = 4, 
                                                    "Mapping & GIS" = 5,
                                                    "Simulations or scenarios" = 6,
                                                    "Community guidance & tools" = 7),
                                     selected = NULL)),  
               
               #--------------------------------------------------------------------
               # Geography
               fluidRow(
                  column(width = 6,
                         selectInput("geoSelect", 
                                     label = h5("Geography"), 
                                     choices = list("East-Africa" = 1, 
                                                    "Rest of Africa" = 2, 
                                                    "Not Africa 'Global South'" = 3,
                                                    "Study is clearly based in rural location" = 4, 
                                                    "Study is clearly based in a city" = 5), 
                                     selected = NULL))),        
                  
               
                  #--------------------------------------------------------------------
                  # Other Themes
                  fluidRow(
                     #Other   
                     checkboxGroupInput("Other_Themes", 
                                        label = h5("Other Themes"), 
                                        choices = list("Training/Education" = 1, 
                                                       "Policy" = 2, 
                                                       "Climate Change" = 3,
                                                       "Sustainability" = 4),
                                        selected = NULL)),
        
        
        #--------------------------------------------------------------------
        # Notes
        hr(),
        fluidRow(textInput(inputId = "notesField", label = "Notes", value = "")),
        
        #--------------------------------------------------------------------
        # The next button
        hr(),
        fluidRow(actionButton("nextButton", "Next", width = "100px", 
                              style="color: #fff; background-color: #4bbf73; border-color: #2e6da4"))
                  ),
        
        #--------------------------------------------------------------------
        # Where the main data resides. 
        # The table itself and whether it has been pre-screened
        # Additional information for screening
        mainPanel(
           DT::dataTableOutput("table"),
           hr(),
           # More info on screening classifications
           verbatimTextOutput("moreInfo"),
           htmlOutput("count"),
           htmlOutput("total")
        ), 
        
        #--------------------------------------------------------------------
        # Where the sidebar sits (left or right). 
        position="right"
               )
            )
        
        
      #Need to change the label names below fix
      #the bracket things above and match the labels with the buttons from 2A set up!!! 
      #March 31 2025 - 1:20PM 

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
      updateMaterialSwitch(session=session, inputId="discardButton",value=FALSE)
      updateCheckboxGroupButtons(session=session, inputId="DFI_Theme", selected = character(0))
      updateCheckboxGroupButtons(session=session, inputId="GAD_Theme", selected = character(0))
      updateCheckboxGroupButtons(session=session, inputId="ID", selected = character(0))
      updateCheckboxGroupButtons(session=session, inputId="FI", selected = character(0))
      updateCheckboxGroupInput(session=session, inputId="MM_Type", selected = 0)
      updateCheckboxGroupInput(session=session, inputId="Tech_Type", selected = 0)
      updateCheckboxGroupInput(session=session, inputId="EntreP_Type", selected = 0)
      updateCheckboxGroupInput(session=session, inputId="EntreP_Sec", selected = 0)
      updateCheckboxGroupInput(session=session, inputId="methodsGroup", selected = 0)
      updateSelectInput(session=session, inputId="geoSelect", selected = as.numeric(data_bib$Screen2_Geography)[values$count+1])
      updateTextInput(session=session, inputId="notesField", value = "")
      
      #Below is from old code. 
      
      #updateCheckboxGroupInput(session=session, inputId="buzzGroup", selected = 0)
      #updateCheckboxGroupInput(session=session, inputId="methodsGroup", selected = 0)
      #updateSelectInput(session=session, inputId="geoSelect", selected = as.numeric(data_bib$Screen2_Location)[values$count+1])
      #updateSelectInput(session=session, inputId="techSelect", selected = as.numeric(data_bib$Screen2_TypesTech)[values$count+1])
      #updateSelectInput(session=session, inputId="entrePSelect", selected = as.numeric(data_bib$Screen2_TypesEntreP)[values$count+1])
      #updateSelectInput(session=session, inputId="mmSelect", selected = as.numeric(data_bib$Screen2_TypesMM)[values$count+1])
      #updateTextInput(session=session, inputId="notesField", value = "")
      
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
           data_bib$Screen2_Reject      [values$count-1] <<- input$discardButton
           

           if(length(input$DFI_Theme) <= 0){
              data_bib$Screen2_DFI           [values$count-1] <<- 0
           } else{
              data_bib$Screen2_DFI[values$count-1] <<- str_c(input$DFI_Theme, collapse = '_')
           }
           
           if(length(input$GAD_Theme) <= 0){
              data_bib$Screen2_GAD        [values$count-1] <<- 0
           } else{
              data_bib$Screen2_GAD     [values$count-1] <<- str_c(input$GAD_Theme, collapse = '_')
           }
           
           if(length(input$ID) <= 0){
              data_bib$Screen2_IncDev        [values$count-1] <<- 0
           } else{
              data_bib$Screen2_IncDev     [values$count-1] <<- str_c(input$ID, collapse = '_')
           }
           
           if(length(input$FI) <= 0){
              data_bib$Screen2_FinancInc        [values$count-1] <<- 0
           } else{
              data_bib$Screen2_FinancInc     [values$count-1] <<- str_c(input$FI, collapse = '_')
           }
           
           if(length(input$MM_Type) <= 0){t
              data_bib$Screen2_MMType[values$count-1] <<- 0
           } else{
              data_bib$Screen2_MMType[values$count-1] <<- input$MM_Type
           }
         
           if(length(input$Tech_Type) <= 0){t
              data_bib$Screen2_TechType[values$count-1] <<- 0
           } else{
              data_bib$Screen2_TechType[values$count-1] <<- input$Tech_Type
           }  
           
           if(length(input$EntreP_Type) <= 0){t
              data_bib$Screen2_EntrePType[values$count-1] <<- 0
           } else{
              data_bib$Screen2_EntrePType[values$count-1] <<- input$EntreP_Type
           }
           
           if(length(input$EntreP_Sec) <= 0){t
              data_bib$Screen2_EntrePSec[values$count-1] <<- 0
           } else{
              data_bib$Screen2_EntrePSec[values$count-1] <<- input$EntreP_Sec
           }
           if(length(input$methodsGroup) <= 0){
              data_bib$Screen2_Methods     [values$count-1] <<- 0
           } else{
              data_bib$Screen2_Methods     [values$count-1] <<- str_c(input$methodsGroup, collapse = '_')
           }  
           if(length(input$geoSelect) <= 0){
              data_bib$Screen2_Geography       [values$count-1] <<- 0
           } else {
              data_bib$Screen2_Geography       [values$count-1] <<- input$geoSelect
           }
           
           
           
           if(length(input$buzzGroup) <= 0){
              data_bib$Screen2_Buzzwords     [values$count-1] <<- 0
           } else{
              data_bib$Screen2_Buzzwords     [values$count-1] <<- str_c(input$buzzGroup, collapse = '_')
           }
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
          data_bib$Screen2_DFI        [nrow(data_bib)] <<- str_c(input$DFI_Theme, collapse = '_')
          data_bib$Screen2_GAD        [nrow(data_bib)] <<- str_c(input$GAD_Theme, collapse = '_')
          data_bib$Screen2_MMType     [nrow(data_bib)] <<- str_c(input$MM_Type, collapse = '_')
          data_bib$Screen2_TechType   [nrow(data_bib)] <<- str_c(input$Tech_Type, collapse = '_')
          data_bib$Screen2_FinancInc  [nrow(data_bib)] <<- str_c(input$FI, collapse = '_')
          data_bib$Screen2_Methods    [nrow(data_bib)] <<- str_c(input$methodsGroup, collapse = '_')
          data_bib$Screen2_EntrePType [nrow(data_bib)] <<- str_c(input$EntreP_Type, collapse = '_')
          data_bib$Screen2_EntrePSec  [nrow(data_bib)] <<- str_c(input$EntreP_Sec, collapse = '_')
          data_bib$Screen2_Other      [nrow(data_bib)] <<- str_c(input$Other_Themes, collapse = '_')
          data_bib$Screen2_Geography  [nrow(data_bib)] <<- input$geoSelect
          data_bib$Screen2_Notes      [nrow(data_bib)] <<- input$notesField
          
          
          #data_bib$Screen2_TypesEntreP[nrow(data_bib)] <<- input$entrePSelect
          #data_bib$Screen2_TypesTech  [nrow(data_bib)] <<- input$techSelect
          #data_bib$Screen2_TypesMM    [nrow(data_bib)] <<- input$mmSelect
          
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
    paste("Assessment Definitions:
          Main Topic - select papers main topic(s) can be multiple. 
          Tropes  - selct (if any) tropes paper is trying to convey (i.e. women can't use tech - gender imbalance)
          Buzzwords - select all buzzwords papers has. 
          ", 
          "Flash Flood Type Definitions: 
          Not specified - flood type not definied or explicitly stated, unclear
          Rainfall - runoff, cloudburst, pluvial, caused by heavy precipitation (no river involved)
          Dam/levee breach - anything to do with dams or levees
          Speedy river - river height changes rapidly, fast onset riverine flood
          Landslide/mudslide - explicitly mentions landslide/mudslide or debris in water
          Snowmelt - caused by melting snow
          ",
          "General Planning - select if the paper is not explicitly related to a single event and is about preparing or planning for future events",
          "If the paper is about impacts in general, not related to a specific event, make sure 'general' is clicked before selecting the impacts",
          "General Science - select if paper is about science-based general planning - ie developing geophysical risk maps",
          sep="\n")
  })
  hr()
  output$count <- renderUI({ HTML(paste("You have reviewed", (values$count - 2) ,"papers in this session")) })
  output$total <- renderUI({HTML(paste("In total, we have reviewed", (sum(data_bib$Screen3_Assessed_v2, na.rm = TRUE)), "of", (length(data_bib$Screen3_Assessed_v2))))})
}

#=======================================================================
# Run the server
#=======================================================================
shinyApp(ui = ui, server = server)

