options(java.parameters = "-Xmx2048m")

library(tm)
library(RWeka)
library(tidyr)
library(dplyr)
library(tictoc)

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

create_model_files <- function() {
  df_model_3g <- group_by(df_train, Term1, Term2, Term3, .drop = TRUE) %>% 
    summarise(Occurence = sum(Occurence)) %>% 
    ungroup()
  
  save(df_model_3g, file = "model_data.RData")
}


create_model2_files <- function() {
  
  tic("create 4 term dataframe")
  df_model4gs <- group_by(df_train, Term1, Term2, Term3, .drop = TRUE)
  df_model4gs <- arrange(df_model4gs, Term1, Term2, Term3, Occurence)
  df_model4gs <- summarise(df_model4gs, TotalOccurence = sum(Occurence), Term4 = last(Term4), Occurence = last(Occurence))
  df_model4gs <- ungroup(df_model4gs)
  
  save(df_model4gs, file = "model4gs_data.RData")
  toc()
  
  tic("create 3 term dataframe")
  df_model3gs <- select(df_model4gs, Term1, Term2, Term3, Occurence = TotalOccurence)
  
  df_model3gs <- group_by(df_model3gs, Term1, Term2, .drop = TRUE)
  df_model3gs <- arrange(df_model3gs, Term1, Term2, Occurence)
  df_model3gs <- summarise(df_model3gs, TotalOccurence = sum(Occurence), Term3 = last(Term3), Occurence = last(Occurence))
  df_model3gs <- ungroup(df_model3gs)
  
  save(df_model3gs, file = "model3gs_data.RData")
  toc()
}

tic("create training set")
df_train <- create_ngram_dataframe(create_corpus(training_set_data_folder), 4)
toc()

tic("create training step")
df_test <- create_ngram_dataframe(create_corpus(test_set_data_folder), 4)
toc()


create_model_files()
create_model2_files()





