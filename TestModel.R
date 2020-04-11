library(tictoc)

source("PredictionModel3GS.R")
source("PredictionModel4GS.R")
source("PredictionModel3GM.R")

index <- sample(1:nrow(df_test), 1000)
t <- df_test[index,]

assess_model <- function(factory) {
  tic("initialisation")
  p <- factory()
  toc()
  tic("prediction")
  result <- t %>% rowwise() %>% mutate(Prediction = p(paste(Term1, Term2, Term3)))
  toc()
  print(sum(result$Term4 == result$Prediction) / nrow(result))
}

print("3 gram model (multiple match)")
assess_model(get_predictor3gm)

print("4 gram model (single match)")
assess_model(get_predictor4gs)

print("3 gram model (single match)")
assess_model(get_predictor3gs)

