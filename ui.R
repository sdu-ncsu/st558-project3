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
library(shinycssloaders)

air <- read_delim("Chicago.csv", delim = ",") %>% select(-c(X, city, date, time, season, year)) %>% drop_na();

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(HTML("<h2>Death in <a href='https://en.wikipedia.org/wiki/Chicago'><strong><i>Chicago</i></strong></a></h2>")),

    tabsetPanel(
        tabPanel("Introduction", fluid = TRUE,

                         h4('This app analyzes the chicago data set from st558. The app is divided into different tabs, which a user
                            may use to navigate'),
                        
                        HTML('<ul>
                                <li>The first tab is a data exploration page, which shows common numerical and graphical summaries</li>
                                <li>The second tab contains a clustering analysis where the user can specify aspects of the model</li>
                                <li>The third tab is a page for modelling</li>
                                <li>The fourth tab is a page that allows the user to scroll through the data, subset it, and save the data to a file</li>
                             </ul>')

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
                         checkboxInput("overHundred", "Only values where death is greater than 100", FALSE),
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
                         plotlyOutput(outputId = "plot1Plotly")
                         
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
                         conditionalPanel(
                             "input.modelType=='randomForest'",
                             
                             sliderInput("mtry", "(mtry) Number of variables to be sampled at each split time",  
                                         min = 1, max = 6, value = 1),
                             h4('Parameters for New Prediction'),
                             sliderInput("rfPredictTemp", "Temperature",  
                                         min = -3, max = 90, value = 1),
                             sliderInput("rfPredictPm10", "PM10",  
                                         min = 0, max = 130, value = 1),
                             sliderInput("rfPredictO3", "O3",  
                                         min = 0, max = 55, value = 1),
                             sliderInput("rfPredictDewpoint", "Dewpoint",  
                                         min = -10, max = 80, value = 1)
                             
                             
                         ),

                         
                         
                         
                     ),
                     
                     mainPanel(fluidRow(
                         conditionalPanel(
                            "input.modelType=='lm'",
                            uiOutput("lmResults")
                         ),
                         conditionalPanel(
                             "input.modelType=='randomForest'",
                             h3("Cross Validated Fit Results on Training Data"),
                             uiOutput("rfResults") %>% withSpinner(color="#0dc5c1"),
                             h3("Fit Results on Test Data"),
                             uiOutput("rfTestResults"), 
                             h3('Prediction'),
                             uiOutput("rfPredictResults") 
                         ),
                         
                     )
                     )
                 )
        ),
        
        tabPanel("Data Viewing", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(

                         selectInput('subsetVar', 'Variable to Subset', names(air)),
                         radioButtons("logical", "Type of Subset",
                                      c("Greater Than" = "gt",
                                        "Less Than" = "lt"
                                        )),
                         numericInput("filterValue", "Value:", 10, min = -100, max = 300),
                         downloadButton("downloadData", "Download")


                     ),

                     mainPanel(fluidRow(
                         dataTableOutput("filteredTable")
                     ))
                 )
        )
    )
    
))

