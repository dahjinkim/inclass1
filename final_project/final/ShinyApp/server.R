#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readr)
library(dplyr)
library(ggplot2)


library(shiny)
shinyServer(function(input, output) {
  
    weeklyData<-read.csv("../fullweekly.csv")

  # This will be updated when options are changed.
    passDataByState<-reactive({
        Weekly<-weeklyData %>%
          filter(state == input$stateSelect) %>%
          mutate(week = as.Date(week))
        return(Weekly)
    })
    passDataFullTrend<- reactive({
      
        Weekly<-weeklyData %>%
          mutate(week = as.Date(week))
        return(Weekly)
    })
    # Now we prepare the output
    output$trendPlot <-renderPlot({ # Note the brace
        thePlot<-ggplot(passDataByState()) +  # Note passData() is a function
          geom_line(mapping=aes(x=week, y=log(committee), color="Committee"))+
          geom_line(mapping= aes(x=week, y=log(deathsum), color="Deaths")) +
          geom_line(mapping=aes(x=week, y=log(individual), color="Individual"))+
          scale_x_date(limits=c(as.Date("2020-1-01"),as.Date("2020-4-28" ))) +
          labs(title=paste("Contribution and COVID19 Deaths"), y="Logged Value", x="Month") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          scale_color_manual(name = "legend", values = c("Committee" = "darkgreen", "Deaths" = "black", "Individual" = "darkorange"), labels = c("Committee", "Deaths", "Individual"))
        print(thePlot)
        return(thePlot)
    })
    output$fullTrendPlot <-renderPlot({
      if (input$fullTrend) {
        full <- ggplot(passDataFullTrend())+
          geom_point(mapping= aes(x=week, y=log(deathsum), color = "Deaths"), alpha=0.5)+
          geom_smooth(mapping = aes(x=week, y=log(deathsum), color = "Deaths"), method=loess) +
          geom_point(mapping=aes(x=week, y=log(committee), color="Committee"), alpha=0.5)+
          geom_smooth(mapping=aes(x=week, y=log(committee), color="Committee"), method=loess) +
          geom_point(mapping=aes(x=week, y=log(individual), color="Individual"), alpha=0.5)+
          geom_smooth(mapping=aes(x=week, y=log(individual), color="Individual"), method=loess) +
          scale_x_date(limits=c(as.Date("2020-01-01"),as.Date("2020-4-28" )))+
          labs(title="Full Trend", y="Logged Value", x="Month") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          scale_color_manual(name = "legend", values = c("Committee" = "darkgreen", "Deaths" = "black", "Individual" = "darkorange"), labels = c("Committee", "Deaths", "Individual"))
        print(full)
        return(full)
        }
      
    })
}) # Close function/shinyServer call

# weeklyData1$week <- as.Date(weeklyData1$week)
# class(weeklyData1$week)
# dim(weeklyData1)
