library(dplyr)

source("Utils.R")

get_predictor4gs <- function() {
  
  if(!exists("df_model4gs")) {
    load("model4gs_data.RData")
  }
  
  df3 <- select(df_model4gs, Term1, Term2, Term3, Occurence = TotalOccurence)
  
  df3 <- group_by(df3, Term1, Term2, .drop = TRUE)
  df3 <- arrange(df3, Term1, Term2, Occurence)
  df3 <- summarise(df3, TotalOccurence = sum(Occurence), Term3 = last(Term3), Occurence = last(Occurence))
  df3 <- ungroup(df3)
  
  df2 <- select(df3, Term1, Term2, Occurence = TotalOccurence)
  
  df2 <- group_by(df2, Term1, .drop = TRUE)
  df2 <- arrange(df2, Term1, Occurence)
  df2 <- summarise(df2, TotalOccurence = sum(Occurence), Term2 = last(Term2), Occurence = last(Occurence))
  df2 <- ungroup(df2)

  df1 <- select(df2, Term1, Occurence = TotalOccurence) %>% 
    arrange(Occurence) %>%
    summarise(Term1 = last(Term1), Occurence = last(Occurence))
  
  get_possible_next_words <- function(term1, term2, term3) {
    
    res <- filter(df_model4gs,
                  Term1 == term1,
                  Term2 == term2,
                  Term3 == term3) %>%
      select(Term = Term4, Occurence, Term1, Term2, Term3)
    
    if(nrow(res) > 0) {
      return(res)
    }
    
    res <- filter(df3,
                  Term1 == term2,
                  Term2 == term3) %>%
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
    
    t <- get_last_terms(terms, 3)
    
    ret <- get_possible_next_words(t[1], t[2], t[3])
    
    ret[1,]$Term
  }
  
  predict_next_word
}
  


take_quiz3 <- function() {
  print(quiz("and", "i", "d", "die", "eat", "give", "sleep"))
  print(quiz("me", "about", "his", "financial", "spiritual", "marital", "horticultural"))
  print(quiz("arctic", "monkeys","this", "weekend", "decade", "morning", "month"))
  print(quiz("helps", "reduce","your", "stress", "sleepiness", "happyness", "hunger"))
  print(quiz("to", "take","a", "walk", "picture", "minute", "look"))
  print(quiz("to", "settle","the", "case", "matter", "account", "incident"))
  print(quiz("groceries", "in","each", "toe", "arm", "hand", "finger"))
  print(quiz("bottom","to","the", "side", "middle", "centre", "top"))
  print(quiz("bruises", "from","playing", "outside", "daily", "inside", "weekly"))
  print(quiz("adam", "sandlers","s", "pictures", "novels", "stories", "movie"))
}


test_optimise <- function() {
  field_names <- c("Term1", "Term2","Term3","Term4", "Occurence")
  
  term1 <- c("the", "the", "the", "the")
  term2 <- c("dog", "dog", "cat", "dog")
  term3 <- c("is", "is", "is", "is")
  term4 <- c("sleeping", "barking", "eating", "eating")
  occurence <- c(2, 461, 23, 51)
  
  
  tdf <- data.frame(term1, term2, term3, term4, occurence, stringsAsFactors = FALSE)
  
  names(tdf) <- field_names
  
  expected_rows <- c(3,2)
  expected_result <- tdf[expected_rows,]
  row.names(expected_result) <- 1:nrow(expected_result)
  
  TotalOccurence <- c(23, 514)
  
  expected_result <- bind_cols(expected_result, data.frame(TotalOccurence))
  
  result <- group_by(tdf, Term1, Term2, Term3, .drop = TRUE) %>% 
    arrange(Term1, Term2, Term3, Occurence) %>%
    summarise(TotalOccurence = sum(Occurence), Term4 = last(Term4), Occurence = last(Occurence)) %>%
    ungroup()
  
  
  head(result)
  
  print(all.equal(result, expected_result))
  
}

