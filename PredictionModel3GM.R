library(tm)
library(dplyr)

source("Utils.R")

get_predictor3gm <- function() {
  
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
    
    t <- get_last_terms(terms, 2)
    
    term1 <- t[1]
    term2 <- t[2]

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
      return("i")
    }
  }
  
  predict_next_word
}
