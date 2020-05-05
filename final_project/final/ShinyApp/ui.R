#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
shinyUI(fluidPage(  
    titlePanel("Covid deaths and campaign contributions"), ## Setting the title
    sidebarLayout( 
        sidebarPanel(
            selectInput(inputId = "stateSelect", # Different input
                        label = "Choose state to display",
                        choices= c(state.abb, "DC"),
                        multiple=FALSE
            ),
            checkboxInput(inputId = "fullTrend", label="Show full trend?", 
                          value = FALSE)),
        mainPanel(  ## putting the main output here
            plotOutput("trendPlot"),
            plotOutput("fullTrendPlot")
        ) # Close main panel
    ) 
))


