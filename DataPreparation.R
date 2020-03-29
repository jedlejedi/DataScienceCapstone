options(java.parameters = "-Xmx2048m")

library(tm)
library(RWeka)
library(tidyr)
library(dplyr)

source("Utils.R")

training_set_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work/training"
test_set_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work/testing"

create_corpus <- function(source_folder) {
  c <- VCorpus(DirSource(source_folder))
  c1 <- tm_map(c, content_transformer(remove_invalid_terms))
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

create_model1_files <- function() {
  df_model_3g <- group_by(df_train, Term1, Term2, Term3, .drop = TRUE) %>% 
    summarise(Occurence = sum(Occurence)) %>% 
    ungroup()
  
  save(df_model_3g, file = "model_data.RData")
}

start_time <- Sys.time()
df_train <- create_ngram_dataframe(create_corpus(training_set_data_folder), 4)
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
df_test <- create_ngram_dataframe(create_corpus(test_set_data_folder), 4)
end_time <- Sys.time()
print(end_time - start_time)


create_model1_files()





