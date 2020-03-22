
index <- sample(1:nrow(df_test), 1000)
t <- df_test[index,]

start_time <- Sys.time()
result <- t %>% rowwise() %>% mutate(Prediction = predict_next_word3(Term1, Term2, Term3))
end_time <- Sys.time()

print(sum(result$Term4 == result$Prediction) / nrow(result))
print(end_time - start_time)


start_time <- Sys.time()
result <- t %>% rowwise() %>% mutate(Prediction = predict_next_word2(Term1, Term2))
end_time <- Sys.time()

print(sum(result$Term3 == result$Prediction) / nrow(result))
print(end_time - start_time)

start_time <- Sys.time()
result <- t %>% rowwise() %>% mutate(Prediction = predict_next_word(Term1, Term2))
end_time <- Sys.time()

print(sum(result$Term3 == result$Prediction) / nrow(result))
print(end_time - start_time)

