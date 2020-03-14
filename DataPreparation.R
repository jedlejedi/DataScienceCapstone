raw_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/raw/en_US"
work_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work"

files <- list.files(raw_data_folder)

fc_output <- file(paste(work_data_folder, "en_US_sample.txt", sep = "/"))

for (f in files) {
  fc <- file(paste(raw_data_folder, f, sep = "/"))
  lines <- readLines(fc);
  close(fc)
  num_line <- length(lines)
  print(c(f, num_line))
  lines <- sample(lines, num_line * 0.1)
  writeLines(lines, fc_output)
}
close(fc_output)


cleanPunctation <- function(x) {
  x1 <- gsub('“', '"', x)
  gsub("’", "'", x1)
}

removeSpecialCharacters <- function(x) {
  x1 <- gsub("😂|⁰", " ", x)
  gsub("♥", " ", x1)
}

library(tm)

c <- VCorpus(DirSource(work_data_folder))
c1 <- tm_map(c, removeNumbers)
c1 <- tm_map(c1, content_transformer(removeSpecialCharacters))
c1 <- tm_map(c1, content_transformer(cleanPunctation))
c1 <- tm_map(c1, removePunctuation)
c1 <- tm_map(c1, stripWhitespace)

tdm <- TermDocumentMatrix(c1)

findFreqTerms(tdm, 5)

library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

tdm2 <- TermDocumentMatrix(c1, control = list(tokenize = BigramTokenizer))

m <- as.matrix(tdm2)

t2 <- strsplit(rownames(m), " ")

df <- data.frame(matrix(unlist(t2), nrow=length(t2), byrow=T), unname(m))

names(df) <- c("Term1", "Term2", "Occurence")

library(dplyr)

findNextTerm <- function(term1, term2) {
  head(filter(df, Term1 == term1, Term2 == term2) %>% arrange(desc(Occurence)))  
}

quiz3 <- function(term1, term2) {
  filter(df, Term1 == term1, Term2 == term2[1] | Term2 == term2[2] | Term2 == term2[3] | Term2 == term2[4]) %>% arrange(desc(Occurence))
}

