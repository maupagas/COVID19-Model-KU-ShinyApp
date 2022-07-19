# Load the libraries requires
library(shiny)
library(deSolve)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(xlsx)
library(dplyr)
library(plyr)
library(reshape2)

source("loadParametersXlsx.R")
source("newModelCOVID.R")
source("ratesCOVID.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    ##### WRITE ALL HTML CODE HERE  #####################
    
    
    #App title ----
    tags$title("Model Simulator COVID-19"),  #Tab title
    
    fluidRow(
        column(6, offset = 4, selectInput(inputId = "language", label = "", 
                                          choices = c("English" = 1, "Espa\u00F1ol" = 2), selected = 1)),  style="float:right"),
    uiOutput('header'),
    hr(),


    #CSS-Styles
    tags$head(
        tags$style(HTML("@import;
          h1 {
            # font-family: 'Lobster', cursive; 
            margin-left: 20%;
            font-weight: 500;
            line-height: 1.1;
            color: #1c4f9d;
          }
          img {vertical-align: middle;}
           # p {
           #   font-family: 'Lobster', bold;
           #   font-weight: 50;
           #   size:40
           #   line-height: 1.1;
           #   color: #b10a05;
           # }

    "))
    ),
    
    
    ###### END OF TAGS FOR HTML CODE HERE ####################################
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(

            #Number the inputs required
            uiOutput("headerSidebar"),
            # Input1: Slider for the simulation time
            uiOutput("tFinal"),
            uiOutput("capICU"),
            uiOutput("ni_h"),
            uiOutput("lpa"),
            uiOutput("pTest"),
            uiOutput("numOfPlot"),
            uiOutput("choicesPlot"),
            uiOutput("choicesAges"),    
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Histogram ----
            plotOutput(outputId = "COVID",
                       click = "plot_click",
                       hover = "plot_hover"),
            verbatimTextOutput("info")
        )
    )
    
    

)

# # Create Shiny app ----
# shinyApp(ui = ui, server = server)
