library(tidyverse)
library(shiny)
library(gtrendsR)
library(plotly)
library(ggplot2)
library(lubridate)
library(data.table)
library(tidyr)
library(tidytext)
library(stopwords)
library(ggwordcloud)
library(qdapRegex)
library(ggthemes)
library(ggiraph)



## UI

ui <- fluidPage(
   fluidRow(
     
    column(3,
            "Input 1",
            textInput("artistname", label = "Page Name"),
            textInput("accesstoken", label = "Enter Token"),
            
            actionButton("stimulate", "Stimulate!")),
    
     column(8, plotOutput("plot2"))
   ),
   fluidRow(
     column(4, tableOutput("table1")),
     column(5, plotOutput("plot1"))
   ),
 )




