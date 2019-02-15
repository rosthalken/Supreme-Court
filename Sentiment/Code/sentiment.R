library(tokenizers)
library(syuzhet)
library(dplyr)
library(readr)
library(stringi)
corpus <- "SupremeCourtCorpusFinalEncoded"
load("Data/metadata.RData")
the_dirs <- dir(corpus, pattern = ".Cleaned")
metadata_sentiment <- NULL
year_l <- NULL

for(i in 1:length(the_dirs)) {
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)) {
    year <- metadata$Year
    year_df <- data.frame(the_dirs[i], the_files[x], year)
  }
}

for(i in 1:length(the_dirs)) {
  sentiment_result <- NULL
  author_sentiment_df <- NULL
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)) {
    text_v <- get_text_as_string(file.path(corpus, the_dirs[i], the_files[x]))
    sentiment_v <- get_sentiment(text_v, method = "syuzhet")
    year <- metadata$Year
   # year_df <- data.frame(the_dirs[i], the_files[x], year)
   # year <- year_df$year
    sentiment_df <- data.frame(the_dirs[i], the_files[x], sentiment_v, year, stringsAsFactors = FALSE)
    colnames(sentiment_df) <- c("Author", "File_Name","Sentiment", "Year")
    sentiment_result <- rbind(sentiment_result, sentiment_df)
  }
  temp_name <- paste("Sentiment/RData/", the_dirs[i], ".RData", sep="")
  mean_sentiment <- mean(sentiment_df$Sentiment)
  #author_sentiment <- data.frame(the_dirs[i], mean(sentiment_v), stringsAsFactors = FALSE)
  # author_sentiment_df <- data.frame(sentiment_result$Author, mean(sentiment_v), stringsAsFactors = FALSE)
  # colnames(author_sentiment_df) <- c("Author", "Sentiment")
  #author_sentiment_df <- rbind(author_sentiment_df, author_sentiment)
  sentiment_result <- data.frame(sentiment_result, mean_sentiment, stringsAsFactors = FALSE)
  #colnames(sentiment_result) <- c("Author", "File_Name", "File_Sentiment", "Author_Sentiment")
  save(sentiment_result, file=temp_name)
}



long_sentiment <- NULL
rdata_files <- dir("Sentiment/RData")
for(i in 1:length(rdata_files)){
  load(file.path("Sentiment/RData", rdata_files[i]))
  #mean_v <- mean(sentiment_result)
  long_sentiment <- rbind(long_sentiment, sentiment_result)
}
save(long_sentiment, file="Sentiment/Data/long_sentiment.RData")

sentiment_author <- select(long_sentiment, Author, Author_Sentiment)
