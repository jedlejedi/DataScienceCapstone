library(tm)
library(dplyr)

source("Utils.R")

get_predictor <- function() {
  
  if(!exists("df_model_3g")) {
    load("model_data.RData")
  }
  
  if(!exists("df_model_2g")) {
    df_model_2g <- group_by(df_model_3g, Term1, Term2, .drop = TRUE) %>% 
      summarise(Occurence = sum(Occurence)) %>% 
      ungroup()
  }
  
  predict_next_word <- function(str) {
    
    terms <- get_clean_terms(str)
    
    n <- length(terms)
    
    if(n == 0) {
      return ("")
    }
    
    term1 <- ""
    term2 <-terms[n]
    
    if(n > 1) {
      term1 <-terms[n-1]  
    }

    
    
    
    res <- filter(df_model_3g, 
                  Term1 == term1, 
                  Term2 == term2) %>% 
      arrange(desc(Occurence))%>%
      select(Term = Term3, Occurence)
    
    if(nrow(res) == 0) {
      res <- filter(df_model_2g, 
                    Term1 == term2) %>%
        arrange(desc(Occurence)) %>% 
        select(Term = Term2, Occurence)
    }
    
    
    if(nrow(res) > 0) {
      return(res[1,]$Term)
    }  
    else {
      return("a")
    }
  }
  
  predict_next_word
}


quiz <- function(term1, term2, w1, w2, w3, w4) {
  res <- filter(df3_m1, 
         Term1 == term1, 
         Term2 == term2, 
         Term3 == w1 | 
           Term3 == w2 | 
           Term3 == w3 | 
           Term3 == w4) %>% arrange(desc(Occurence))
  if(nrow(res) == 0) {
    res <- filter(df2_m1, 
                  Term1 == term2, 
                  Term2 == w1 | 
                    Term2 == w2 | 
                    Term2 == w3 | 
                    Term2 == w4) %>% arrange(desc(Occurence))
  }
  head(res)
}


quiz1 <- function() {
  quiz("case", "of", "cheese", "beer", "sode", "pretzels")
  quiz("mean", "the", "best", "most", "universe", "world")
  quiz("me","the", "saddest", "smelliest", "bluest", "happiest")
  quiz("but","the", "defense", "referees", "players", "crowd")
  quiz("at","the", "mall", "beach", "grocery", "movies")
  quiz("on","my", "phone", "horse", "way", "motorcycle")
  quiz("quite","some", "thing", "time", "weeks", "years")
  quiz("his","little", "eyes", "toes", "fingers", "ears")
  quiz("during","the", "hard", "sad", "worse", "bad")
  quiz("must","be", "asleep", "callous", "insensitive", "insane")
}


# quiz2 <- function() {
#   quiz("i", "d", "die", "eat", "give", "sleep")
#   quiz("about", "his", "financial", "spiritual", "marital", "horticultural")
#   quiz("monkeys","this", "weekend", "decade", "morning", "month")
#   quiz("reduce","your", "stress", "sleepiness", "happyness", "hunger")
#   quiz("take","a", "walk", "picture", "minute", "look")
#   quiz("settle","the", "case", "matter", "account", "incident")
#   quiz("in","each", "toe", "arm", "hand", "finger")
#   quiz("to","the", "side", "middle", "centre", "top")
#   quiz("from","playing", "outside", "daily", "inside", "weekly")
#   quiz("sandlers","s", "pictures", "novels", "stories", "movie")
# }