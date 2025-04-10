library(DT)      ;  library(magrittr)
library(stringr) ;  library(tidyverse)
library(RISmed)  ;  library(bibliometrix)
library(bib2df)  ;  library(knitr)
library(shiny)   ;  library(rmdformats)
library(shinythemes)
library(shinyWidgets)
library(shinyEffects)
library(shinydashboard)
library(shinydashboardPlus)


ui <- dashboardPage(
   dashboardHeader(title = "Accordion Test"),
   
   dashboardSidebar(
      includeCSS("www/styles.css"),  # Custom styles
      
      materialSwitch(
         inputId = "discardButton",
         label = "Not Relevant",
         value = FALSE,
         status = "danger"
      ),
      
      accordion(
         id = "screening_panels",
         
         accordionItem(
            title = "Accordion 1 Item 1",
            status = "primary",     # Changed to primary
            collapsed = TRUE,
            solidHeader = TRUE,
            "This is some text!"
         ),
         
         accordionItem(
            title = "Accordion 1 Item 2",
            status = "primary",     # Changed to primary
            collapsed = FALSE,
            solidHeader = TRUE,
            "This is some more text!"
            
         )
      )
   ),
   
   dashboardBody(
      #includeCSS("www/styles.css")  # Ensure styles are loaded in body too
   )
)

server <- function(input, output) {}

shinyApp(ui, server)