#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse);
library(readxl);
library(plotly);


air <- read_delim("Chicago.csv", delim = ",") %>% select(-c(X, city, date, time, season, year)) %>% drop_na();

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    getLmColData <- reactive({
        
        LmColData <- air %>% select(input$xlmcol)

    })
    
    getColData <- reactive({
        colData <- air %>% select(input$varSum)
    })
    
    output$summaryText <- renderText({
        #get filtered data
        summary(getColData())
    })
    
    selectedClusterData <- reactive({
        air[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedClusterData(), input$clusters)
    })
    
    output$lmResults <- renderUI({
        # fit <- lm(air$death ~ getLmColData()[[1]])
        fit <- lm(death ~ eval(parse(text = input$xlmcol)), air)
        withMathJax(
            paste0(
                "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
                ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
                ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3)
            ),
            br(),
            if(input$xlmcol == 'temp') {
                paste0(predict(fit, data.frame( temp = c(input$lmPrediction))))
            } else if (input$xlmcol == 'pm10') {
                paste0(predict(fit, data.frame( pm10 = c(input$lmPrediction))))
            } else if (input$xlmcol == 'o3') {
                paste0(predict(fit, data.frame( o3 = c(input$lmPrediction))))
            } else if (input$xlmcol == 'dewpoint') {
                paste0(predict(fit, data.frame( dewpoint = c(input$lmPrediction))))
            }
        )
    })

    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

        par(mar = c(5.1, 4.1, 0, 1))
        
        plot(selectedClusterData(),
             col = clusters()$cluster)
        
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    
    output$summaryPlot <- renderPlotly({

        if(input$varSum == 'pm10'){
            fig <- plot_ly(y = air$pm10, type = "box", quartilemethod="exclusive")
        } else if (input$varSum == 'dewpoint' ){
            fig <- plot_ly(y = air$dewpoint, type = "box", quartilemethod="exclusive")
        } else if (input$varSum == 'temp') {
            fig <- plot_ly(y = air$temp, type = "box", quartilemethod="exclusive")
        } else if (input$varSum == 'o3') {
            fig <- plot_ly(y = air$o3, type = "box", quartilemethod="exclusive")
        } else if (input$varSum == 'death') {
            fig <- plot_ly(y = air$death, type = "box", quartilemethod="exclusive")
        }
        fig

    })
    

})
