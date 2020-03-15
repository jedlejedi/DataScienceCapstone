library(tm)

raw_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/raw/en_US"
work_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work"

create_data_file <- function(input_folder, output_folder, sample = FALSE) {
  
  files <- list.files(raw_data_folder)
  
  fc_output <- file(paste(output_folder, "en_US.txt", sep = "/"))
  
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
  x1 <- gsub('“|”', '"', x)
  x1 <- gsub("’|‘", "'", x1)
  x1 <- gsub("…|'", " ", x1)
  x1 <- gsub("¿|¡", " ", x1)
  x1 <- gsub("—|–|―", " ", x1)
  x1 <- gsub("♥|⁰|•", " ", x1)
  x1
}

create_corpus <- function(source_folder)  {
  c <- VCorpus(DirSource(source_folder))
  c1 <- tm_map(c, removeNumbers)
  c1 <- tm_map(c1, content_transformer(cleanPunctation))
  c1 <- tm_map(c1, removePunctuation, ucp = FALSE)
  c1 <- tm_map(c1, stripWhitespace)
  c1
}

sample_file_folder <- paste(work_data_folder, "sample", sep = "/")
full_file_folder <- paste(work_data_folder, "full", sep = "/")

# create_data_file(raw_data_folder, sample_file_folder, TRUE)
# c <- create_corpus(sample_file_folder)

#create_data_file(raw_data_folder, full_file_folder, FALSE)
c <- create_corpus(full_file_folder)

# tdm1 <- TermDocumentMatrix(c)
# 
# m1 <- as.matrix(tdm1)

test_cleanPunctation <- function() {
  print(cleanPunctation('ask”') == 'ask"')
  print(cleanPunctation("¿what if i was shallow?") == " what if i was shallow?")
  print(cleanPunctation("— but not impossible —") == "  but not impossible  ")
  print(cleanPunctation("I'd like") == "I d like")
  print(cleanPunctation("— but not impossible —") == "  but not impossible  ")
}