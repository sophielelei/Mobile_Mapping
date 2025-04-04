library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(bslib)
library(fresh)

# Create a custom theme if you want 
mytheme <- create_theme(
   adminlte_color(
      #light_blue = "#434C5E"
   ),
   adminlte_sidebar(
      width = "600px",
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



shinyApp(
   ui = dashboardPage(
      skin="blue",
      title = "Accordion Test",
      freshTheme=mytheme,
      header=dashboardHeader(),
      sidebar=dashboardSidebar(collapsed=TRUE),
      controlbar=dashboardControlbar(width=575,collapsed=FALSE,overlay=FALSE,
                                     hr(),
                                     materialSwitch(
                                        inputId = "discardButton",
                                        label = "DISCARD PAPER - Not Relevant",
                                        value = FALSE,
                                        status = "danger"
                                     ),
                                     accordion(
                                        id = "screening_panels",
                                        accordionItem(
                                           title = "Menu 2",
                                           status = "primary",
                                           collapsed = FALSE,
                                           "This is some text!"
                                        )
                                     )
      ),
      body=dashboardBody(
         includeCSS("www/styles.css"), # Link to style sheet
         
         
      ),
   ),
   server = function(input, output, session) {}
)



