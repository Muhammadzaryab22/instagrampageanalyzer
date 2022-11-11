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
