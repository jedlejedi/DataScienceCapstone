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