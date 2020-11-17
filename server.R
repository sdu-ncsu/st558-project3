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
library(caret);


air <- read_delim("Chicago.csv", delim = ",") %>% select(-c(X, city, date, time, season, year)) %>% drop_na();

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    getFilteredData <- reactive({
        if(input$logical == 'gt') {
            filteredData <- air %>% filter(eval(parse(text = input$subsetVar)) > input$filterValue)
        } else {
            filteredData <- air %>% filter(eval(parse(text = input$subsetVar)) < input$filterValue)
        }
            
    })
    
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
    
    
    getRfFit <- eventReactive(input$mtry, {
        set.seed(92)
        trainIndex <- createDataPartition(air$death, 
                                          p = 0.8, list = FALSE)
        
        airTrain <- air[as.vector(trainIndex),];
        airTest <- air[-as.vector(trainIndex),];
        
        my_grid <- expand.grid(mtry = input$mtry:input$mtry)
        
        rfFit <- train(death ~ ., data = airTrain, 
                       preProcess =c("center", "scale"),
                       method = "rf",
                       tuneGrid = my_grid,
                       trControl = trainControl(method = "cv", number = 4))
        
        testPerformance <- predict(rfFit, newdata = airTest)
        testResult <- postResample(testPerformance, obs = airTest$death)
        
        return(list("fit" = rfFit, "testResult" = testResult))
        
    })
    
    output$rfResults <- renderDataTable({
        getRfFit()[[1]]$results
    })
    
    output$rfResults <- renderUI({
        withMathJax(
            paste0("RSME = ", getRfFit()[[1]]$results[[2]]),
            br(),
            paste0("\\( R^2 = \\)", getRfFit()[[1]]$results[[3]]),
            br(),
            paste0("\\( MAE = \\)", getRfFit()[[1]]$results[[4]])
            
        )
    })
    
    output$rfTestResults <- renderUI({
        withMathJax(
            paste0("RSME = ", getRfFit()[[2]][[1]]),
            br(),
            paste0("\\( R^2 = \\)", getRfFit()[[2]][[2]]),
            br(),
            paste0("\\( MAE = \\)", getRfFit()[[2]][[3]])
            
        )
    })
    
    output$rfPredictResults <- renderUI({
        
        newValues = data.frame("death"=100, "temp" = input$rfPredictTemp, "dewpoint" = input$rfPredictDewpoint, "pm10" = input$rfPredictPm10, "o3"=input$rfPredictO3)
        withMathJax(
            paste0("Death = ", predict(getRfFit()[[1]], newValues))
        )
        })
    
    output$lmResults <- renderUI({
        fit <- lm(death ~ eval(parse(text = input$xlmcol)), air)
        withMathJax(
            h3('Linear Regression Information'),
            paste0(
                "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
                ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
                ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3)
            ),
            br(),
            h3('Prediction'),
            if(input$xlmcol == 'temp') {
                paste0("Death = ", predict(fit, data.frame( temp = c(input$lmPrediction))))
            } else if (input$xlmcol == 'pm10') {
                paste0("Death = ",predict(fit, data.frame( pm10 = c(input$lmPrediction))))
            } else if (input$xlmcol == 'o3') {
                paste0("Death = ",predict(fit, data.frame( o3 = c(input$lmPrediction))))
            } else if (input$xlmcol == 'dewpoint') {
                paste0("Death = ",predict(fit, data.frame( dewpoint = c(input$lmPrediction))))
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

        if(input$overHundred) {
            newAir <- air %>% filter(death > 100)
        } else {
            newAir <- air
        }
        
        if(input$varSum == 'pm10'){
            fig <- plot_ly(y = newAir$pm10, type = "box", quartilemethod="exclusive")
        } else if (input$varSum == 'dewpoint' ){
            fig <- plot_ly(y = newAir$dewpoint, type = "box", quartilemethod="exclusive")
        } else if (input$varSum == 'temp') {
            fig <- plot_ly(y = newAir$temp, type = "box", quartilemethod="exclusive")
        } else if (input$varSum == 'o3') {
            fig <- plot_ly(y = newAir$o3, type = "box", quartilemethod="exclusive")
        } else if (input$varSum == 'death') {
            fig <- plot_ly(y = newAir$death, type = "box", quartilemethod="exclusive")
        }
        fig

    })
    
    output$filteredTable <- renderDataTable({
        getFilteredData()
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("filtered_data", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(getFilteredData(), file, row.names = FALSE)
        }
    )
    

})
