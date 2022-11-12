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



## Creating functions

getinsta <- function(instapage, token){
  
  insta <- httr::GET(paste("https://graph.facebook.com/v15.0/17841454311301277?fields=business_discovery.username(",instapage,"){followers_count,media_count,media{caption,like_count,comments_count}}&access_token=",token, sep = "" ))           
  
  instatext <- rawToChar(insta$content)
  
  instajson <- jsonlite::fromJSON(instatext, flatten = TRUE)
  
  likes <- mean(instajson$business_discovery$media$data$like_count)
  
  comments <- mean(instajson$business_discovery$media$data$comments_count)
  
  df <- data.frame(AverageLikes = likes, AverageComments = comments, PageName = instapage, Date = as.character(Sys.Date()))
  
  
}


getcaptions <- function(instapage, token){
  
  insta <- httr::GET(paste("https://graph.facebook.com/v15.0/17841454311301277?fields=business_discovery.username(",instapage,"){followers_count,media_count,media{caption,like_count,comments_count}}&access_token=",token, sep = "" ))
  
  instatext <- rawToChar(insta$content)
  
  instajson <- jsonlite::fromJSON(instatext, flatten = TRUE)
  
  captions <- instajson$business_discovery$media$data$caption
  
  ## remove hashtags
  reduced <- sub("(?:\\s*#\\w+)+\\s*$", "", captions)
  
  reduced <- as.data.frame(reduced)
  
  reduced <- data.table(reduced)
  
  
  ## tokenizing
  
  first <- reduced %>% 
    select(reduced) %>%
    tibble::rowid_to_column() %>%
    unnest_tokens(word, reduced)
  
  
  # remove punctuations
  
  first$word <- gsub("[[:punct:]]", "", first$word)
  
  #remove stop words
  first <- first %>%
    anti_join(get_stopwords(source = "stopwords-iso"))
  
  ## Here you have to join all other pages too
  
  # count
  first <- first %>% count(word, sort = TRUE)
  
  # remove words with count less than 3
  
  first <- first[!(first$n < 2)]
  
  # Wordcloud plot
  
  wordplot <- first %>%  ggplot(aes(label = word, color = factor(first$n), size = n)) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = 16)
  
  wordplot
  
  
}

getlikes <- function(instapage, token){
  
  insta <- httr::GET(paste("https://graph.facebook.com/v15.0/17841454311301277?fields=business_discovery.username(",instapage,"){followers_count,media_count,media{caption,like_count,comments_count}}&access_token=",token, sep = "" ))           
  
  instatext <- rawToChar(insta$content)
  
  instajson <- jsonlite::fromJSON(instatext, flatten = TRUE)
  
  likeslist <- instajson$business_discovery$media$data$like_count
  likedf <- as.data.frame(likeslist)
  likedf <- rowid_to_column(likedf, var = "rowid")
  
  
}

## Server

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
