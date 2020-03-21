raw_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/raw/en_US"
work_data_folder <- "/home/julien/data-science/DataScienceCapstone/data/work"

if(!dir.exists(work_data_folder)) {
  stop("Work folder doesn't exists")
}

training_set_folder <- paste(work_data_folder, "training", sep = "/")
test_set_folder <- paste(work_data_folder, "testing", sep = "/")

if(!dir.exists(training_set_folder)) {
  dir.create(training_set_folder)
}

if(!dir.exists(test_set_folder)) {
  dir.create(test_set_folder)
}

source_files <- list.files(raw_data_folder)

fc_training <- file(paste(training_set_folder, "training", sep = "/"))
fc_testing <- file(paste(test_set_folder, "testing", sep = "/"))

for (f in source_files) {
  fc <- file(paste(raw_data_folder, f, sep = "/"))
  lines <- readLines(fc);
  close(fc)
  num_line <- length(lines)
  
  print(c(f, num_line))
  
  training_index <- sample(1:num_line, num_line * 0.8)
  test_index <- setdiff(1:num_line, training_index)
  
  writeLines(lines[training_index], fc_training)
  writeLines(lines[test_index], fc_testing)
}
close(fc_training)
close(fc_testing)
  