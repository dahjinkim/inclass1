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
# setwd("~/GitHub/jmontgome/ry.github.io/PDS/Datasets/SenateForecast")
working_dir = "C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/group_work/final_project/"
weeklyData<-read.csv(paste0(working_dir, "/fullweekly.csv")) 


library(shiny)
shinyServer(function(input, output) {
    ## This will just be executed once
    working_dir = "C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/group_work/final_project/"
    weeklyData<-read.csv(paste0(working_dir, "/fullweekly.csv")) 
    # This will be updated when options are changed.
    passData<-reactive({ 
        WeeklyForState<-weeklyData %>%
            filter(state==input$stateSelect) %>%
            # select(c("Poll", "daysLeft", "Candidateidentifier")) %>% 
            # group_by(Candidateidentifier)
        return(WeeklyForState)
    })
    # Now we prepare the output
    output$trendPlot <-renderPlot({ # Note the brace
        thePlot<-ggplot(passData()) +  # Note passData() is a function
            geom_line(mapping= aes(x=week, y=log(deathsum)))+
            geom_line(mapping=aes(x=week, y=log(committee), color="Committee"))+
            geom_line(mapping=aes(x=week, y=log(individual), color="Individual"))+
            scale_x_date(limits=c(as.Date("2020-1-01"),as.Date("2020-4-28" ))) + 
            labs(title="Contribution and COVID19 Deaths (New York)", y="Logged Value") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_color_manual(name = "Contribution", values = c("Committee" = "darkgreen", "Individual" = "darkorange"), labels = c("Committee", "Individual"))
        print(thePlot)
    })
}) # Close function/shinyServer call

