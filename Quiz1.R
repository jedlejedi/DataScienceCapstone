con_twit_us <- file("final/en_US/en_US.twitter.txt", "r")
lines_twit_us <- readLines(con_twit_us)

con_news_us <- file("final/en_US/en_US.news.txt", "r")
lines_news_us <- readLines(con_news_us)

con_blogs_us <- file("final/en_US/en_US.blogs.txt", "r")
lines_blogs_us <- readLines(con_blogs_us)


#Question 2
length(lines_twit_us)

#Question 3
max(nchar(lines_twit_us))

max(nchar(lines_news_us))

max(nchar(lines_blogs_us))


# Question 4
love <- grepl("love", lines_twit_us)
hate <- grepl("hate", lines_twit_us)

sum(love) / sum(hate)

# Question 5
biostats <- grepl("biostats", lines_twit_us)
lines_twit_us[biostats]

#Question 6
matches <- grepl("^A computer once beat me at chess, but it was no match for me at kickboxing$", lines_twit_us)
sum(matches)
