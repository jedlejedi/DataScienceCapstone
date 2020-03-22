options(java.parameters = "-Xmx2048m")

library(tm)
library(RWeka)
library(tidyr)
library(dplyr)
library(qdapDictionaries)

data(GradyAugmented)

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

  remove_non_english_words(df)
}

remove_non_english_words <- function(df) {
  filter_at(df, vars(starts_with("Term")), all_vars((. %in% GradyAugmented)))
}


df_train <- create_ngram_dataframe(create_corpus(training_set_data_folder), 3)
#df_test <- create_trigram_dataframe(create_corpus(test_set_data_folder))

test_cleanPunctation <- function() {
  print(cleanPunctation('ask”') == 'ask ')
  print(cleanPunctation("¿what if i was shallow?") == " what if i was shallow ")
  print(cleanPunctation("— but not impossible —") == "  but not impossible  ")
  print(cleanPunctation("I'd like") == "I d like")
  print(cleanPunctation("— but not impossible —") == "  but not impossible  ")
  print(cleanPunctation("☀😁💛school")  == "   school")
}


test_remove_non_english_words <- function() {
  
  field_names <- c("Term1", "Term2","Term3","ATerm4", "Occurence")
  
  term1 <- c("the", "the", "the", "the")
  term2 <- c("dog", "dogaaa", "cat", "dog")
  term3 <- c("is", "is", "is", "is")
  term4 <- c("barking", "barking", "barking", "barkingasa")
  occurence <- c(12, 1, 456, 3)
  
  tdf <- data.frame(term1, term2, term3, term4, occurence, stringsAsFactors = FALSE)
  
  names(tdf) <- field_names
  
  expected_rows <- c(1,3,4)
  expected_result <- tdf[expected_rows,]
  row.names(expected_result) <- 1:nrow(expected_result)
  
  result <- remove_non_english_words(tdf)
  
  all.equal(result, expected_result)
}