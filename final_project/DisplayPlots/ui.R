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
    titlePanel("2016 Senate Polling"), ## Setting the title
    sidebarLayout( 
        sidebarPanel(
            selectInput(inputId = "stateSelect", # Different input
                        label = "Choose state to display",
                        choices=c("MO"="Missouri",
                                  "FL"="Florida",
                                  "NC"="North Carolina",
                                  "IA"="Iowa"
                        ),
                        multiple=FALSE
            )), ## End of sidebar
        mainPanel(  ## putting the main output here
            plotOutput("trendPlot")
        ) # Close main panel
    ) 
))


