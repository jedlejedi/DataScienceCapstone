library(dplyr)

source("Utils.R")

get_predictor3gs <- function() {
  
  if(!exists("df_model3gs")) {
    load("model3gs_data.RData")
  }

  df2 <- select(df_model3gs, Term1, Term2, Occurence = TotalOccurence)
  
  df2 <- group_by(df2, Term1, .drop = TRUE)
  df2 <- arrange(df2, Term1, Occurence)
  df2 <- summarise(df2, TotalOccurence = sum(Occurence), Term2 = last(Term2), Occurence = last(Occurence))
  df2 <- ungroup(df2)

  df1 <- select(df2, Term1, Occurence = TotalOccurence) %>% 
      arrange(Occurence) %>%
      summarise(Term1 = last(Term1), Occurence = last(Occurence))

  get_possible_next_words <- function(term1, term2) {
    
    res <- filter(df_model3gs,
                  Term1 == term1,
                  Term2 == term2) %>%
      select(Term = Term3, Occurence, Term1, Term2)
    
    if(nrow(res) > 0) {
      return(res)
    }
    
    res <- filter(df2,
                  Term1 == term2) %>%
      select(Term = Term2, Occurence, Term1)
    
    if(nrow(res) > 0) {
      return(res)
    }
    
    res <- select(df1, Term = Term1, Occurence)
    
    return(res)
  }
  
  predict_next_word <- function(str) {
    
    terms <- get_clean_terms(str)
    
    t <- get_last_terms(terms, 2)
    
    ret <- get_possible_next_words(t[1], t[2])
    
    ret[1,]$Term
  }
  
  predict_next_word
}


