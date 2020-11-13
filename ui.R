#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
#
library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(plotly)

air <- read_delim("Chicago.csv", delim = ",") %>% select(-c(X, city, date, time, season, year)) %>% drop_na();

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Death in Chicago"),

    tabsetPanel(
        tabPanel("Introduction", fluid = TRUE,

                         h2('This app analyzes the chicago data set from st558. The app is divided into different tabs, which a user
                            may use to navigate'),
                         h3('The first tab is a data exploration page, which shows common numerical and graphical summaries'),
                         h3('The second tab contains a clustering analysis where the user can specify aspects of the model'),
                         h3('The third tab is a page for modelling'),
                         h3('The fourth tab is a page that allows the user to scroll through the data, subset it, and save the data to a file')

        ),
        tabPanel("Data Exploration", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         h3('Select the variable to summarize'),
                         
                         selectInput("varSum", "Variables",
                                     c("dewpoint" = "dewpoint",
                                       "temp" = "temp",
                                       "pm10" = "pm10",
                                       "o3" = "o3",
                                       "death" = "death")),
                        ),
                     
                     mainPanel(fluidRow(
                         plotlyOutput(outputId = "summaryPlot"),
                         textOutput("summaryText"),

                     )
                     )
                 )
        ),
        
        tabPanel("Clustering", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(

                             selectInput('xcol', 'X Variable', names(air)),
                             selectInput('ycol', 'Y Variable', names(air),
                                         selected=names(air)[[2]]),
                             numericInput('clusters', 'Cluster count', 3,
                                          min = 1, max = 9)


                     ),
                     
                     mainPanel(fluidRow(
                         plotOutput('plot1')
                         
                     )
                     )
                 )
        ),
        
        tabPanel("Modelling", fluid = TRUE,
                 sidebarLayout(
                     
                     sidebarPanel(
                         HTML('<h3>Models for Predicting <a href="https://en.wikipedia.org/wiki/Death"><strong>Death</strong></a></h3>'),
                         selectInput("modelType", "Model Type",
                                     c("Linear Model" = "lm",
                                       "Random Forest" = "randomForest")
                                     ),
                         conditionalPanel(
                             "input.modelType=='lm'",
                             selectInput('xlmcol', 'X Variable', names(air)[-1]),
                             sliderInput("lmPrediction", "Predicted Value of X Variable:",  
                                         min = 1, max = 150, value = 20)
                             
                         ),

                         
                         
                         
                     ),
                     
                     mainPanel(fluidRow(
                         uiOutput("lmResults")
                         
                     )
                     )
                 )
        )
    )
    
))

