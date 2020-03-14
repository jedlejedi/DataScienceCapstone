library(tm)

raw_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/raw/en_US"
work_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work"

create_data_file <- function(input_folder, output_file, sample = FALSE) {
  
  files <- list.files(raw_data_folder)
  
  fc_output <- file(output_file)
  
  for (f in files) {
    fc <- file(paste(raw_data_folder, f, sep = "/"))
    lines <- readLines(fc);
    close(fc)
    num_line <- length(lines)
    print(c(f, num_line))
    if(sample) {
      lines <- sample(lines, num_line * 0.1)
    }
    writeLines(lines, fc_output)
  }
  close(fc_output)  
  
}

cleanPunctation <- function(x) {
  x1 <- gsub('“', '"', x)
  gsub("’", "'", x1)
}

removeSpecialCharacters <- function(x) {
  x1 <- gsub("😂|⁰", " ", x)
  gsub("♥", " ", x1)
}

create_corpus <- function(source_folder)  {
  c <- VCorpus(DirSource(source_folder))
  c1 <- tm_map(c, removeNumbers)
  c1 <- tm_map(c1, content_transformer(removeSpecialCharacters))
  c1 <- tm_map(c1, content_transformer(cleanPunctation))
  c1 <- tm_map(c1, removePunctuation, ucp = FALSE)
  c1 <- tm_map(c1, stripWhitespace)
  c1
}

sample_file <- paste(work_data_folder, "en_US_sample.txt", sep = "/")

create_data_file(raw_data_folder, sample_file, TRUE)

c <- create_corpus(work_data_folder)
