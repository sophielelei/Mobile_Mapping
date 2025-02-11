#=======================================================================
# How to run
# Press Ctrl-A to select all, then run.  Shiny should start up
#
# IT WILL LOOK EMPTY.  You will have to press "next" to get to the first one
# 
# When you get bored close it down and everything is already saved - then upload to git
# To start again, just select all and run again.
#
# If you get errors, try closing R shiny and starting again. This genuinely works.
#=======================================================================
#-----------------------------------------------------------------------
# Clear the workspace and Load libraries
#-----------------------------------------------------------------------
rm(list=ls())
library(shiny)   ;  library(tidyverse)
library(DT)      ;  library(rmdformats)
library(stringr) ;  library(shinyWidgets)
library(RISmed)  ;  library(bibliometrix)
library(bib2df)  ;  library(knitr)
library(magrittr);  library(shinythemes)


#-----------------------------------------------------------------------
# Set up directories
# Annoyingly, it wants this to be hardwired. It should be where the main github folder is
#-----------------------------------------------------------------------
maindir     <- "~/Desktop/mobile_money/M_mapping"
Workingfile <- paste(maindir,"data/MainScreeningData_Screen1.RData",sep="/")



#-----------------------------------------------------------------------
# Load the data
#-----------------------------------------------------------------------
load(Workingfile)
# Load the saved dataset from the specified file using readRDS.
# This retrieves the data_bib object to continue screening from where it was last saved.

names(data_bib)

#-----------------------------------------------------------------------
# Back-up dataset to one that is date/timestamped
#-----------------------------------------------------------------------
backupname <- paste("MainScreeningData_Screen1_",format(Sys.time(), "%Y%m%d-%H%M"),".RData",sep="")
save(data_bib, file = paste(maindir,"data/03_Screen1Backup",backupname,sep="/"))


#-----------------------------------------------------------------------
# If screening has already started, sort data so screened data is at the bottom
# Once each row is screened, the column screen1_assessed is set to TRUE for that paper
# So check if *any* of the "Screen1_assessed" rows are marked TRUE
#-----------------------------------------------------------------------
if(length(which(data_bib$Screen1_Assessed == TRUE)) > 1){
   # If so, sort to get them at the bottom
   data_bib <- data_bib[with(data_bib, order(Screen1_Assessed,Screen1_Reject)), ]
}


#=======================================================================
# Highlighting Rules
# Need to put in colorblind friendly options
# This could be made into a better function, where maybe we add in these options in an excel file
#=======================================================================
wordHighlightyellow   <- function(SuspWord,colH = "#FFE9A8") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgreen    <- function(SuspWord,colH = "#BEDDBA") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightblue     <- function(SuspWord,colH = "#A3C4D9") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightred      <- function(SuspWord,colH = "#CFA6B6") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightdarkblue <- function(SuspWord,colH = "#BAB4D4") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightgrey     <- function(SuspWord,colH = grey(0.9)) {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}
wordHighlightdarkred  <- function(SuspWord,colH = "#feb24c") {paste0('<span style="background-color:',colH,'">',SuspWord,'</span>')}


helenhighlight <- function(YourData){
   YourData %<>% str_replace_all(regex("pesa", ignore_case = TRUE), wordHighlightyellow)
   YourData %<>% str_replace_all(regex("money", ignore_case = TRUE), wordHighlightyellow)
   
   YourData %<>% str_replace_all(regex("mobile", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("entrepreneurial", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("entrepr", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("digitization", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("technology", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("digital", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("digital health", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("knowledge networks", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("internet banking", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("digital finance", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("self-employment", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("financial access", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("financial service", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("financial inclusion", ignore_case = TRUE), wordHighlightgreen)
   YourData %<>% str_replace_all(regex("systematic review", ignore_case = TRUE), wordHighlightgreen)
   
   
   YourData %<>% str_replace_all(regex("Kenya", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("Africa", ignore_case = TRUE), wordHighlightblue)
   YourData %<>% str_replace_all(regex("East-Africa", ignore_case = TRUE), wordHighlightblue)
   
   YourData %<>% str_replace_all(regex("gender ", ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("women ", ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("girl", ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("female employment", ignore_case = TRUE), wordHighlightdarkblue)
   YourData %<>% str_replace_all(regex("women's income", ignore_case = TRUE), wordHighlightdarkblue)
   
   
   YourData %<>% str_replace_all(regex("health", ignore_case = TRUE), wordHighlightdarkred)
   YourData %<>% str_replace_all(regex("clinic", ignore_case = TRUE), wordHighlightdarkred)
   YourData %<>% str_replace_all(regex("agri", ignore_case = TRUE), wordHighlightdarkred)
   YourData %<>% str_replace_all(regex("farmer", ignore_case = TRUE), wordHighlightdarkred)
   YourData %<>% str_replace_all(regex("herder", ignore_case = TRUE), wordHighlightdarkred)
   YourData %<>% str_replace_all(regex("pasture", ignore_case = TRUE), wordHighlightdarkred)
   
   
   
   
   
   YourData %<>% str_replace_all(regex(" compilation",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("atlas",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[1],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[2],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[3],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[4],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[6],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[7],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[8],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[9],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[10],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[11],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex(month.name[12],ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("190",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("191",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("192",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("193",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("194",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("195",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("196",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("197",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("198",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("199",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("200",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("201",ignore_case = TRUE), wordHighlightred)
   YourData %<>% str_replace_all(regex("202",ignore_case = TRUE), wordHighlightred)
}


#=======================================================================
# GUI function
#=======================================================================
ui <- fluidPage(
   
   
   #--------------------------------------------------------------------
   # Feel free to change the theme to serve your preferences, you'll be staring at this for a while
   # so might as well make it look good.
   theme = shinytheme("darkly"),
   #--------------------------------------------------------------------
   # Set up styles, this was me just messing around until I got something that
   # was easy on my eyes with chatgpt
   
   tags$head(
      tags$style(HTML("
      body {
        background-color: #212121; /* Overall background color */
        color: #E8E8E8; /* Default text color */
      }
      .custom-datatable-container {
        background-color: #E8E8E8; /* Light grey background for the DataTable */
        padding: 15px;
      }
      .custom-verbatim {
        font-family: 'Menlo-regular', Courier, monospace; /* Monospaced font */
        background-color: #2F2F2F; /* Background color for verbatim text output */
        color: #BBBBBB; /* Text color for verbatim text output */
        border-radius: 5px; /* Optional: Rounded corners */
        font-size: 0.7em; /* Decrease font size (adjust as needed) */
        overflow: auto; /* Ensure long text is scrollable */
      }
      .custom-header-indent {
        margin-left: 0pt;
        color: #41b6c4 /* header color
      }
    "))
   ),
   
   
   #--------------------------------------------------------------------
   # This creates a shiny sidebar.  There are other options
   sidebarLayout(
      sidebarPanel(
        #--------------------------------------------------------------------
         # Gold star
         #--------------------------------------------------------------------
        hr(),
        fluidRow(column( width = 12, tags$header(class = "custom-header-indent",tags$h4(tags$b("Keep paper"))))),
        fluidRow( column(width = 12, materialSwitch(inputId = "goldstarButton", label = "Key paper! MUST INCLUDE",  value = FALSE,  width = "100%",  status = "danger"))),
        fluidRow( column(width = 12, materialSwitch(inputId = "VagueButton", label = HTML("Vaguely Interesting"),  value = FALSE,  width = "100%",  status = "danger"))),
        
        #--------------------------------------------------------------------
        # Discard
        #--------------------------------------------------------------------
         fluidRow(column( width = 12, tags$header(class = "custom-header-indent",tags$h4(tags$b("Discard Paper"))))),
         #fluidRow( column(width = 12, materialSwitch(inputId = "discardButton", label = "REMOVE: Paper off-topic",  value = FALSE,  width = "100%",  status = "danger"))),
         fluidRow( column(width = 12, materialSwitch(inputId = "DiscardEntrePButton", label = HTML("REMOVE: Not focused on Entrepreneurship,<br> Ex. On health or agriculture"),  value = FALSE,  width = "100%",  status = "danger"))),
         hr(),
         fluidRow(( column(width = 12,actionButton("nextButton", "Next"))),
       
         #--------------------------------------------------------------------
         # Basic methods
         fluidRow(column( width = 12, tags$header(class = "custom-header-indent",tags$h4(tags$b("Paper Characteristics"))))),
         fluidRow( column(width = 12, materialSwitch(inputId = "WomenButton", label = "Women can't use tech",  value = FALSE,  width = "100%",  status = "danger"))),
         fluidRow( column(width = 12, materialSwitch(inputId = "EntrePButton", label = "Tech empowers women",  value = FALSE,  width = "100%",  status = "danger"))),
         #fluidRow( column(width = 12, materialSwitch(inputId = "EAfricaButton", label = "About East-Africa?",  value = FALSE,  width = "100%",  status = "danger"))),
        #fluidRow( column(width = 12, materialSwitch(inputId = "ExampleButton", label = "IGNORE",  value = FALSE,  width = "100%",  status = "danger"))),
         hr(),
        
         #--------------------------------------------------------------------
         # The next button
         fluidRow(column( width = 12, tags$header(class = "custom-header-indent",tags$h4(tags$b("Issues/Next"))))),
         fluidRow( column(width = 12, materialSwitch(inputId = "missingdetailButton", label = "ISSUE: Abstract missing",  value = FALSE,  width = "100%",  status = "danger"))),
         fluidRow(textInput(inputId = "notesField", label = "Notes", value = "")),
         hr())
         #fluidRow(( column(width = 12,actionButton("nextButton", "Next"))))
      ),
      
      #--------------------------------------------------------------------
      # Where the main data resides. 
      # The table itself and whether it has been pre-screened
      # Additional information for screening
      mainPanel(
         hr(),
         div(class = "spacer"),
         div(
            class = "custom-datatable-container",
            DT::dataTableOutput("table")
         ),
         div(class = "spacer"),
         hr(),
         div(class = "spacer"),
         # Use a custom textOutput instead of verbatimTextOutput
         div(class = "custom-verbatim",
             uiOutput("moreInfo")
         ),
         div(class = "spacer"),
         htmlOutput("count"),
         htmlOutput("total")
      ), 
      
      #--------------------------------------------------------------------
      # Where the sidebar sits (left or right). 
      position="right"
   )
)

#=======================================================================
# Server
#=======================================================================
server <-  function(input,output,session){
   
   #--------------------------------------------------------------------
   # Create a "reactive value" which allows us to play with the output of a button click
   values <- reactiveValues(); values$count <- 0
   
   #--------------------------------------------------------------------
   # Add the session end handler to save progress 
   session$onSessionEnded(function() {
      message("Session is ending. Saving progress...")  # Debugging message
      save(list="data_bib", file=Workingfile)
      message("Progress saved successfully.")           # Debugging message
   })
   
   #--------------------------------------------------------------------
   # At the same time on a next click (bloody shiny), 
   # select the row you care about and highlight it
   highlighter <- eventReactive(
      {input$nextButton 
      },
      {
         updateMaterialSwitch(session=session, inputId="goldstarButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="missingdetailButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="VagueButton",value=FALSE)
         # updateMaterialSwitch(session=session, inputId="discardButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="DiscardEntrePButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="WomenButton",value=FALSE)
         updateMaterialSwitch(session=session, inputId="EntrePButton",value=FALSE)
         #updateMaterialSwitch(session=session, inputId="EAfricaButton",value=FALSE)
         #updateMaterialSwitch(session=session, inputId="ExampleButton",value=FALSE)
         updateTextInput(session=session, inputId="notesField", value = "")
         save(list="data_bib",file=Workingfile)
         
         print("-------------------------------------------------------")
         print(paste("Current Count: ", values$count))
         print(data_bib[values$count, c("TI")])
         print(data_bib[values$count, c("Screen1_Assessed","Screen1_VaguelyRelevent","Screen1_RejectNOTEntreP")])
         
         
         #-----------------------------------------------------
         # If the row number is not at the end, increment up
         # THIS IS *REALLY BAD CODING*, ADDED IN BECAUSE IT WANTS TO RECALCULATE THE VALUE.
         # if(sum(c(input$discardButton,input$rainButton,input$modelButton,input$socialButton))>0){
         if(values$count != nrow(data_bib)){
            #-----------------------------------------------------
            # move to the next row
            values$count <- values$count + 1
            
            #-----------------------------------------------------
            # choose that row in the table
            YourData <- data_bib[values$count,c("TI","AB")]
            
            if (is.null(YourData) || nrow(YourData) == 0) {
               print("YourData is empty!")
               return(NULL)  # Return NULL to prevent further processing
            }
            
            YourData2 <- helenhighlight(YourData)
           
            #-----------------------------------------------------
            # Output to data_bib   
            # Set that the screening has been assessed.
            data_bib$Screen1_Assessed      [values$count-1] <<- TRUE
            data_bib$Screen1_GoldStar      [values$count-1]       <<- input$goldstarButton
        #    data_bib$Screen1_Reject        [values$count-1]              <- input$discardButton
            data_bib$Screen1_RejectNOTEntreP[values$count-1] <<-input$DiscardEntrePButton
            data_bib$Screen1_VaguelyRelevent[values$count-1] <<- input$VagueButton
            data_bib$Screen1_Women         [values$count-1]  <<- input$WomenButton
            data_bib$Screen1_EntreP         [values$count-1] <<- input$EntrePButton
            #data_bib$Screen1_EAfrica        [values$count-1]  <<- input$EAfricaButton
           # data_bib$Screen1_Example         [values$count-1]  <<- input$ExampleButton
            data_bib$Screen1_MissingDetails[values$count-1]  <<-input$missingdetailButton
            data_bib$Screen1_Notes         [values$count-1]  <<- input$notesField

     
            
            return(YourData2)
         }
         #-----------------------------------------------------
         # Or put the final row
         else{
            YourData <- data_bib[ nrow(data_bib),c("TI","AB")]
            YourData2 <- helenhighlight(YourData)
            
            #-----------------------------------------------------
            # Output to data_bib         
            data_bib$Screen1_Assessed       [nrow(data_bib)] <<- TRUE
            data_bib$Screen1_GoldStar       [nrow(data_bib)] <<- input$goldstarButton
            #data_bib$Screen1_Reject         [nrow(data_bib)] <<- input$discardButton
            #data_bib$Screen1_RejectRain     [nrow(data_bib)] <<- input$rainButton
            data_bib$Screen1_RejectNOTEntreP [nrow(data_bib)]  <<- input$DiscardEntrePButton
            data_bib$Screen1_VaguelyRelevent [nrow(data_bib)]    <<- input$VagueButton
            data_bib$Screen1_Women          [nrow(data_bib)] <<- input$WomenButton
            data_bib$Screen1_EntreP          [nrow(data_bib)] <<- input$EntrePButton
            #data_bib$Screen1_EAfrica         [nrow(data_bib)] <<- input$EAfricaButton
            #data_bib$Screen1_Example          [nrow(data_bib)] <<- input$ExampleButton
            data_bib$Screen1_MissingDetails [nrow(data_bib)] <<- input$missingdetailButton
            data_bib$Screen1_Notes          [nrow(data_bib)] <<- input$notesField
            return(YourData2)
         }
      })  
   
   
   
   
   #--------------------------------------------------------------------
   # Output the table to the GUI
   
   output$table <- DT::renderDataTable({ data <- highlighter()}, escape = FALSE,options = list(dom = 't',bSort=FALSE)) 
   output$reject <- renderUI({ HTML(paste(highlightExclude(), sep = '<br/>'))})
   
   #--------------------------------------------------------------------
   # More info about the screening selections
   hr()
   output$moreInfo <- renderUI({
      HTML(paste(
         "<strong>HOW TO SCREEN</strong><br><br>",
         "<div style='margin-left: 20px;'>", # Indent the entire section
         "<strong>1. GOLD STAR</strong> [YES means] - The paper is extremely relevant to our analysis<br>",
         "<div style='margin-left: 160px;'>AKA it directly studies mobile money, entrepreneurship and women in East-Africa<br></div><br>",
         
         "<strong>2. DISCARD: [YES] -</strong> Discard this paper. Off-topic/Not relevant at all<br>",
         "<div style='margin-left: 160px;'>No relevant content is discussed in this paper.<br></div><br>",
         
         #"<strong>3. DISCARD_RAIN: [YES] -</strong> Discard this paper because it's just about<br>>",
         #"<div style='margin-left: 160px;'>hydrology/water/rain in general, AKA this paper<br<br></div><br>",
         #"<div style='margin-left: 160px;'>does not study floods as a hazard.<br></div><br>",
         
         "<strong>4. TOPICS:</strong><br><br>", 
         "If you're keeping the paper, then use these toggles if it matches one of these families:<br><br>",
         "<div style='margin-left: 40px;'><strong>A.</strong> Women don't use tech [YES] - anywhere this is mentioned, even if the paper is discarded<br></div>",
         "<div style='margin-left: 40px;'><strong>A.</strong> Women empowered by tech [YES] - anywhere this is mentioned, even if the paper is discarded<br></div>",
         "</div>",
         
         "<strong>2. MISSING DETAILS/NOTES: [YES] -</strong> The abstract is missing and needs to be added<br>",
         "<div style='margin-left: 160px;'>This needs to be added before proceeding with the analysis.<br></div><br>",
         
         sep = ""
      ))
   })
   hr()
   output$count <- renderUI({ HTML(paste("You have reviewed", (values$count - 2) ,"papers in this session")) })
   output$total <- renderUI({HTML(paste("In total, we have reviewed", (sum(data_bib$Screen1_Assessed, na.rm = TRUE)), "of", (length(data_bib$Screen1_Assessed))))})
}

#=======================================================================
# Run the server
#=======================================================================

# Run the application 
shinyApp(ui = ui, server = server)





screenedsummary <- data_bib[which(data_bib$Screen1_Assessed == TRUE),c(1,40,60:70)]

write.csv(screenedsummary,"./data/Todays_screenedsummary.csv")

