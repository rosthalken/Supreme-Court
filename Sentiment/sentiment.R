library(tokenizers)
library(syuzhet)
library(dplyr)
library(readr)
library(stringi)
corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Cleaned")
metadata <- NULL


for(i in 1:length(the_dirs)) {
  sentiment_result <- NULL
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)) {
    text_v <- get_text_as_string(file.path(corpus, the_dirs[i], the_files[x]))
    sentiment_v <- get_sentiment(text_v, method = "syuzhet")
    # sum_sentiment_v <- sum(sentiment_v)
    # mean_sentiment_v <- mean(sentiment_v)
    # sentiment_t <- data.frame(sum_sentiment_v, mean_sentiment_v)
    # sentiment_result <- rbind(sentiment_result, sentiment_t)
    
    # Creating metadata
    metadata <- rbind(metadata, data.frame(the_dirs[i], the_files[x], sentiment_t))
  }
  temp_name <- paste("Sentiment/RData/", the_dirs[i], ".RData", sep="")
  save(long_result, file=temp_name)
}

# Adding case year to metadata
load("Data/case_year.RData")
metadata <- merge(metadata, case_year, by = c("Author","Case"))
save(metadata, file="Sentiment/Data/metadata.RData")


long_sentiment <- NULL
rdata_files <- dir("Sentiment/RData")
for(i in 1:length(rdata_files)){
  load(file.path("Sentiment/RData", rdata_files[i]))
  long_sentiment <- rbind(long_sentiment, sentiment_result)
  # OR just immediately do the adding by case
  justice_mean <- mean(sentiment_v)
}
save(long_form, file="Sentiment/Data/long_form.RData")

