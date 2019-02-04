library(syuzhet)

corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Cleaned")
metadata <- NULL



for(i in 1:length(the_dirs)) {
  long_result <- NULL
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)) {
    text_v <- get_text_as_string(file.path(corpus, the_dirs[i], the_files[x]))
    sentiment_v <- get_sentiment(text_v, method = "syuzhet")
    sum_sentiment_v <- sum(sentiment_v)
    mean_sentiment_v <- mean(sentiment_v)
    sentiment_t <- data.frame(sum_sentiment_v, mean_sentiment_v)
    long_result <- rbind(long_result, sentiment_t)
    # creating metadata
    metadata <- rbind(metadata, data.frame(the_dirs[i], the_files[x], sentiment_t))
  }
}

