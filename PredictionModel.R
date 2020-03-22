library(tm)
library(dplyr)

df3 <- df_train


df2 <- group_by(df3, Term1, Term2, .drop = TRUE) %>% summarise(Occurence = sum(Occurence)) %>% ungroup()

get_possible_next_words <- function(term1, term2) {
  res <- filter(df3, 
                Term1 == term1, 
                Term2 == term2) %>% 
    arrange(desc(Occurence))%>%
    select(Term = Term3, Occurence)
  
  if(nrow(res) == 0) {
    res <- filter(df2, 
                  Term1 == term2) %>%
      arrange(desc(Occurence)) %>% 
      select(Term = Term2, Occurence)
  }
  
  res
}

predict_next_word <- function(term1, term2) {
  
  ret <- get_possible_next_words(term1, term2)
  
  if(nrow(ret) > 0) {
    return(ret[1,]$Term)
  }  
  else {
    return("a")
  }
  
}

quiz <- function(term1, term2, w1, w2, w3, w4) {
  res <- filter(df3, 
         Term1 == term1, 
         Term2 == term2, 
         Term3 == w1 | 
           Term3 == w2 | 
           Term3 == w3 | 
           Term3 == w4) %>% arrange(desc(Occurence))
  if(nrow(res) == 0) {
    res <- filter(df2, 
                  Term1 == term2, 
                  Term2 == w1 | 
                    Term2 == w2 | 
                    Term2 == w3 | 
                    Term2 == w4) %>% arrange(desc(Occurence))
  }
  head(res)
}


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

# quiz("i", "d", "die", "eat", "give", "sleep")
# quiz("about", "his", "financial", "spiritual", "marital", "horticultural")
# quiz("monkeys","this", "weekend", "decade", "morning", "month")
# quiz("reduce","your", "stress", "sleepiness", "happyness", "hunger")
# quiz("take","a", "walk", "picture", "minute", "look")
# quiz("settle","the", "case", "matter", "account", "incident")
# quiz("in","each", "toe", "arm", "hand", "finger")
# quiz("to","the", "side", "middle", "centre", "top")
# quiz("from","playing", "outside", "daily", "inside", "weekly")
# quiz("sandlers","s", "pictures", "novels", "stories", "movie")
