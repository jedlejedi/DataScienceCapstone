options(java.parameters = "-Xmx2048m")

library(tm)
library(RWeka)
library(tidyr)
library(dplyr)
library(qdapDictionaries)

training_set_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work/training"
test_set_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work/testing"


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

create_ngram_dataframe <- function(c, n) {
  
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  
  tdm <- TermDocumentMatrix(c, control = list(tokenize = Tokenizer))
  
  m <- as.matrix(tdm)
  rm(tdm)
  
  Terms <- rownames(m)
  Occurence <- unname(m)
  rm(m)
  
  df <- data.frame(Terms, Occurence)
  rm(Terms, Occurence)
  
  field_names <- paste0("Term", 1:n)
  
  df <- separate(df, Terms, field_names, " ")

  data(GradyAugmented)
  
  # Remove trigrams that contains non-english words
  df_clean <- filter(df, 
         Term1 %in% GradyAugmented, 
         Term2 %in% GradyAugmented, 
         Term3 %in% GradyAugmented, 
         Term4 %in% GradyAugmented)
  
  df_clean
}

df_train <- create_ngram_dataframe(create_corpus(training_set_data_folder), 4)
#df_test <- create_trigram_dataframe(create_corpus(test_set_data_folder))

test_cleanPunctation <- function() {
  print(cleanPunctation('ask”') == 'ask ')
  print(cleanPunctation("¿what if i was shallow?") == " what if i was shallow ")
  print(cleanPunctation("— but not impossible —") == "  but not impossible  ")
  print(cleanPunctation("I'd like") == "I d like")
  print(cleanPunctation("— but not impossible —") == "  but not impossible  ")
  print(cleanPunctation("☀😁💛school")  == "   school")
}