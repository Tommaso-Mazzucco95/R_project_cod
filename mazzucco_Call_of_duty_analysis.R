#Set working directory
setwd("~/CLass Solution/Text Data/hult_NLP_student_tommaso/cases/Call of Duty E-Sport/teamTimeline")
#Libraries
library(tm)
#library(qdap)
library(lexicon)
library(dplyr)
library(fst)
library(pbapply)
library(mgsub)
library(radarchart)
library(tidytext)
library(reshape2)
library(wordcloud)
library(viridisLite)
library(RColorBrewer)
library(stringr)
library(plotrix)
library(ggplot2)
library(ggthemes)
library(ggalt)
library(tidyverse)
library(tidytext)
library(radarchart)
library(gridExtra) 
library(stringi)

#supporting functions
#source('~/Desktop/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')
#emoji <- read.csv('emojis.csv')
#stopwords
stops <- c(stopwords('english'),'cod','game','player','match','league','modern',
           'warfare')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  #corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

#function tokens
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


#Upload the Data
cod <- read_fst("student_TeamTimelines.fst")
table(cod$screen_name)


# Select the only two teams we are interested in 
selected_teams <- cod[cod$screen_name %in% c("Immortals","LAGuerrillas"),]


#via the v corpus function we clean the data also throwing out our stopwords
txtCorpus <- VCorpus(VectorSource(selected_teams$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)


#Create TDM according to control tokenize and make it into a matrix
TDM  <- TermDocumentMatrix(txtCorpus, 
                              control=list(tokenize=bigramTokens))
smallerTDM <- removeSparseTerms(TDM, 0.999)
cod_TDMm <- as.matrix(smallerTDM)

#Create corpus also for hashtags related to the teams

hashtags_teams <- VCorpus(VectorSource(selected_teams$hashtags))
hashtags_TDM  <- TermDocumentMatrix(hashtags_teams, 
                                   control=list(tokenize=bigramTokens))
hashtags_matrix <- as.matrix(hashtags_TDM)


#'?'

#row sum for hashtags
cod_sum_hashtags <- sort(rowSums(hashtags_matrix), decreasing = TRUE)
cod_sum_hashtags   <- data.frame(word = names(cod_sum_hashtags), freq = cod_sum_hashtags)
#row sum for term matrix
cod_sum_TDM <- sort(rowSums(cod_TDMm), decreasing = TRUE)
cod_sum_TDM   <- data.frame(word = names(cod_sum_TDM), freq = cod_sum_TDM)


#General word cloud
set.seed(1234)
wordcloud(cod_sum_TDM$word,
          cod_sum_TDM$freq,
          max.words    = 60,
          random.order = FALSE,
          colors ='darkgreen',
          min.freq     = 15)


#most frequent words

cod_sum_TDM%>%
  filter(cod_sum_TDM$freq>40) %>% 
  ggplot(., aes(x=word, y=freq, fill=freq)) + 
  geom_bar(stat="identity")+
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.20, size=1.0)


#5 words
cod_sum_TDM%>% 
  slice_max(cod_sum_TDM$freq, n=5) %>% 
  ggplot(., aes(x=word, y=freq, fill=freq))+
  geom_col(position="identity")


#hashtags plot

cod_sum_hashtags %>%
  filter(cod_sum_hashtags$freq>15) %>% 
  ggplot(., aes(x=word, y=freq, fill=freq)) + 
  geom_bar(stat="identity")+
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.20, size=1.0)

#sent

source("~/CLass Solution/Text Data/hult_NLP_student_tommaso/lessons/Z_otherScripts/ZZZ_supportingFunctions.R")
textB <- cleanMatrix(pth        = "student_TeamTimelines.fst",
                     columnName = 'text',
                     collapse   = T,
                     customStopwords = stops,
                     type = 'TDM', # TDM or DTM
                     wgt = 'weightTf')






tmp      <- as.DocumentTermMatrix(textB, weighting = weightTf ) 
tidyCorp <- tidy(tmp)
tidyCorp
dim(tidyCorp)

# Get bing lexicon
# "afinn", "bing", "nrc", "loughran"
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent <- inner_join(tidyCorp, bing, by=c('document' = 'word'))
bingSent

# Quick Analysis
table(bingSent$sentiment, bingSent$count)
aggregate(count~sentiment,bingSent, sum)


#Plot overal sentiment
bingSent%>% 
  ggplot(., aes(sentiment, count, fill = sentiment)) +
  geom_col()






