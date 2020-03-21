library(dplyr)

df3 <- group_by(df_train, Term1, Term2, Term3, .drop = TRUE) %>%
  summarise(Occurence = sum(Occurence)) %>%
  ungroup()

# df4 <- left_join(df_train, df3, by = c("Term1" = "Term1", "Term2" = "Term2", "Term3" = "Term3")) %>%
#   filter(Occurence == MaxOccurence)


df2 <- group_by(df3, Term1, Term2, .drop = TRUE) %>%
  summarise(Occurence = sum(Occurence)) %>%
  ungroup()

df1 <- group_by(df2, Term1, .drop = TRUE) %>%
  summarise(Occurence = sum(Occurence)) %>%
  ungroup()


get_possible_next_words <- function(term1, term2, term3) {
  
  res <- filter(df_train,
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
                Term1 == term3) %>%
    select(Term = Term2, Occurence, Term1)

  if(nrow(res) > 0) {
    return(res)
  }

  res <- select(df1, Term = Term1, Occurence)

  return(res)
}

# predict_next_word <- function(term1, term2) {
# 
#   ret <- get_possible_next_words(term1, term2)
# 
#   ret[1,]$Term
# }

quiz <- function(term1, term2, term3, w1, w2, w3, w4) {
  
  res <- filter(df_train, 
                Term1 == term1, 
                Term2 == term2, 
                Term3 == term3, 
                Term4 == w1 | 
                  Term3 == w2 | 
                  Term3 == w3 | 
                  Term3 == w4) %>% arrange(desc(Occurence))
  
  if(nrow(res) > 0) {
    return(res)
  }

  res <- filter(df3, 
                Term1 == term2, 
                Term2 == term3, 
                Term3 == w1 | 
                  Term3 == w2 | 
                  Term3 == w3 | 
                  Term3 == w4) %>% arrange(desc(Occurence))
  
  if(nrow(res) > 0) {
    return(res)
  }
  
  res <- filter(df2, 
                Term1 == term3, 
                Term2 == w1 | 
                  Term2 == w2 | 
                  Term2 == w3 | 
                  Term2 == w4) %>% arrange(desc(Occurence))

  
  if(nrow(res) > 0) {
    return(res)
  }
  
  filter(df1, 
         Term1 == w1 | 
           Term1 == w2 | 
           Term1 == w3 | 
           Term1 == w4) %>% arrange(desc(Occurence))
  
}


# quiz("case", "of", "cheese", "beer", "sode", "pretzels")
# quiz("mean", "the", "best", "most", "universe", "world")
# quiz("me","the", "saddest", "smelliest", "bluest", "happiest")
# quiz("but","the", "defense", "referees", "players", "crowd")
# quiz("at","the", "mall", "beach", "grocery", "movies")
# quiz("on","my", "phone", "horse", "way", "motorcycle")
# quiz("quite","some", "thing", "time", "weeks", "years")
# quiz("his","little", "eyes", "toes", "fingers", "ears")
# quiz("during","the", "hard", "sad", "worse", "bad")
# quiz("must","be", "asleep", "callous", "insensitive", "insane")

quiz("and", "i", "d", "die", "eat", "give", "sleep")
quiz("me", "about", "his", "financial", "spiritual", "marital", "horticultural")
quiz("arctic", "monkeys","this", "weekend", "decade", "morning", "month")
quiz("helps", "reduce","your", "stress", "sleepiness", "happyness", "hunger")
quiz("to", "take","a", "walk", "picture", "minute", "look")
quiz("to", "settle","the", "case", "matter", "account", "incident")
quiz("groceries", "in","each", "toe", "arm", "hand", "finger")
quiz("bottom","to","the", "side", "middle", "centre", "top")
quiz("bruises", "from","playing", "outside", "daily", "inside", "weekly")
quiz("adam", "sandlers","s", "pictures", "novels", "stories", "movie")
