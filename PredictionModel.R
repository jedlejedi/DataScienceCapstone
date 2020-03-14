library(tm)

tdm1 <- TermDocumentMatrix(c)

m1 <- as.matrix(tdm1)


library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

tdm2 <- TermDocumentMatrix(c, control = list(tokenize = BigramTokenizer))

m2 <- as.matrix(tdm2)

t2 <- strsplit(rownames(m2), " ")

df <- data.frame(matrix(unlist(t2), nrow=length(t2), byrow=T), unname(m2))

names(df) <- c("Term1", "Term2", "Occurence")

library(dplyr)

findNextTerm <- function(term1, term2) {
  head(filter(df, Term1 == term1, Term2 == term2) %>% arrange(desc(Occurence)))  
}

quiz3 <- function(term1, term2) {
  filter(df, Term1 == term1, Term2 == term2[1] | Term2 == term2[2] | Term2 == term2[3] | Term2 == term2[4]) %>% arrange(desc(Occurence))
}

quiz3("in", c("toe", "arm", "hand", "finger"))