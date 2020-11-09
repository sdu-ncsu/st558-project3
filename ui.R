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


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Deaths in Chicago as Affected by Weather"),

    tabsetPanel(
        tabPanel("Map", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
                     mainPanel(
                         h3('This data set comes from the '),
                     )
                 )
        ),
        tabPanel("plot", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel(fluidRow(
                         column(7,  plotlyOutput("")),
                         column(5, plotlyOutput(""))   
                     )
                     )
                 )
        )
    )
    
    # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("distPlot")
    #     )
    # )
))


ui = fluidPage(
    tabsetPanel(
        tabPanel("Map", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
                     mainPanel(
                         htmlOutput("Attacks")
                     )
                 )
        ),
        tabPanel("plot", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel(fluidRow(
                         column(7,  plotlyOutput("")),
                         column(5, plotlyOutput(""))   
                     )
                     )
                 )
        )
    )
)
