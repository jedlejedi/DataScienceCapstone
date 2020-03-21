options(java.parameters = "-Xmx2048m")

library(tm)
library(RWeka)
library(tidyr)
library(dplyr)
library(qdapDictionaries)

training_set_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work/training"

cleanPunctation <- function(x) {
  x1 <- gsub("[^A-Za-z]", " ", x)
  x1
}

create_corpus <- function(source_folder) {
  c <- VCorpus(DirSource(source_folder))
  c1 <- tm_map(c, content_transformer(cleanPunctation))
  c1 <- tm_map(c1, stripWhitespace)
  c1 
}

create_trigram_dataframe <- function(c) {
  
  TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
  
  tdm <- TermDocumentMatrix(c, control = list(tokenize = TrigramTokenizer))
  
  m <- as.matrix(tdm)
  rm(tdm)
  
  Terms <- rownames(m)
  Occurence <- unname(m)
  rm(m)
  
  df <- data.frame(Terms, Occurence)
  rm(Terms, Occurence)
  
  df3 <- separate(df, Terms, c("Term1","Term2","Term3"), " ")
  rm(df)
  
  data(GradyAugmented)
  
  # Remove trigrams that contains non-english words
  df_clean <- filter(df3, 
         Term1 %in% GradyAugmented, 
         Term2 %in% GradyAugmented, 
         Term3 %in% GradyAugmented)
  
  df_clean
}

c <- create_corpus(training_set_data_folder)

df3 <- create_trigram_dataframe(c)

test_cleanPunctation <- function() {
  print(cleanPunctation('ask”') == 'ask ')
  print(cleanPunctation("¿what if i was shallow?") == " what if i was shallow ")
  print(cleanPunctation("— but not impossible —") == "  but not impossible  ")
  print(cleanPunctation("I'd like") == "I d like")
  print(cleanPunctation("— but not impossible —") == "  but not impossible  ")
  print(cleanPunctation("☀😁💛school")  == "   school")
}