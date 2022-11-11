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


## Server

server <- function(input, output, session) {
  
  ## calculations
  webdata <- eventReactive(input$stimulate, {getinsta(input$artistname, input$accesstoken)})
  youtubedata <- eventReactive(input$stimulate, {getcaptions(input$artistname, input$accesstoken)})
  likedata <- eventReactive(input$stimulate, {getlikes(input$artistname, input$accesstoken)})
  
  #  combineddata <- reactive(rbind(webdata()$interest_over_time, youtubedata()$interest_over_time))
  
  
  output$table1 <- renderTable({
    
    webdata()
    
    ##plotting
  }, res= 96)
  
  output$plot1 <- renderPlot({
    
    youtubedata()
    
  })
  
  output$plot2 <- renderPlot({
    
    likedata() %>% ggplot(., aes(x = rowid, y = likeslist)) +
      #interactive elements of layers (points, lines and text at final)
      geom_line_interactive(size = 1.2, alpha = 0.4) +
      geom_point_interactive(
        aes(tooltip = rowid), #specifies tooltip for ggiraph
        fill = "white",
        size = 2.5,
        stroke = 1.5,
        shape = 21) + labs(y = "Post Likes", x = "Post Number")
  
})
  
}
