library(qdapDictionaries)
library(dplyr)

data(GradyAugmented)

remove_invalid_terms <- function(x) {
  x1 <- gsub("[^A-Za-z]", " ", x)
  x1
}

test_remove_invalid_terms <- function() {
  print(remove_invalid_terms('ask”') == 'ask ')
  print(remove_invalid_terms("¿what if i was shallow?") == " what if i was shallow ")
  print(remove_invalid_terms("— but not impossible —") == "  but not impossible  ")
  print(remove_invalid_terms("I'd like") == "I d like")
  print(remove_invalid_terms("— but not impossible —") == "  but not impossible  ")
  print(remove_invalid_terms("☀😁💛school")  == "   school")
}

remove_non_english_words <- function(df) {
  filter_at(df, vars(starts_with("Term")), all_vars((. %in% GradyAugmented)))
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
  
  print(all.equal(result, expected_result))
}

get_clean_terms <- function(str) {
  res <- remove_invalid_terms(str)
  res <- trimws(res)
  strsplit(res, " ")[[1]]
}

test_get_clean_terms <-  function() {
  print(5 == sum(get_clean_terms("¿what if i was shallow?") == c("what", "if", "i", "was", "shallow")))
}

test_utils <- function() {
  test_remove_invalid_terms()
  test_remove_non_english_words()
  test_get_clean_terms()
}
