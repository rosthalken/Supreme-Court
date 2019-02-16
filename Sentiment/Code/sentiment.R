library(tokenizers)
library(syuzhet)
library(dplyr)
library(readr)
library(stringi)
library(ggplot2)
corpus <- "SupremeCourtCorpusFinalEncoded"
#load("Data/metadata.RData")
load("Data/case_year.RData")
the_dirs <- dir(corpus, pattern = ".Cleaned")
metadata_sentiment <- NULL

sentiment_result <- NULL

for(i in 1:length(the_dirs)) {
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)) {
    text_v <- get_text_as_string(file.path(corpus, the_dirs[i], the_files[x]))
    sentences <- get_sentences(text_v)
    sentiment_n <- get_sentiment(sentences, method = "syuzhet")
    mean_sent <- mean(sentiment_n)
    year <- filter(case_year, File_Name == the_files[x], Author == the_dirs[i]) %>%
            select(Year)
    theyear <- year$Year
    sentiment_df <- data.frame(the_dirs[i], x, the_files[x], mean_sent, theyear, stringsAsFactors = FALSE)
    colnames(sentiment_df) <- c("Author","File_ID","File_Name","Mean Sentiment", "Year")
    sentiment_result <- rbind(sentiment_result, sentiment_df)
    cat(i, " ", x, "\r")
  }
}



save(sentiment_result, file="Sentiment/Data/sentiment_result.RData")

sentiment_groups <- group_by(sentiment_result, Author, Year) %>%
  summarise(year_sent = mean(`Mean Sentiment`))


# ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
#   geom_line(aes(linetype=Author)) +
#   geom_smooth() +
#   facet_grid(rows = vars(Author))

plotted_sentiment <- ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth() 

vertical_sentiment <- plotted_sentiment + facet_grid(Author ~ .)

sp + facet_wrap( ~ day, ncol=2)

grid_sentiment <- plotted_sentiment + facet_wrap(~ Author, ncol=3)


#   temp_name <- paste("Sentiment/RData/", the_dirs[i], ".RData", sep="")
#   mean_sentiment <- mean(sentiment_df$Sentiment)
#   #author_sentiment <- data.frame(the_dirs[i], mean(sentiment_v), stringsAsFactors = FALSE)
#   # author_sentiment_df <- data.frame(sentiment_result$Author, mean(sentiment_v), stringsAsFactors = FALSE)
#   # colnames(author_sentiment_df) <- c("Author", "Sentiment")
#   #author_sentiment_df <- rbind(author_sentiment_df, author_sentiment)
#   sentiment_result <- data.frame(sentiment_result, mean_sentiment, stringsAsFactors = FALSE)
#   #colnames(sentiment_result) <- c("Author", "File_Name", "File_Sentiment", "Author_Sentiment")
#   save(sentiment_result, file=temp_name)
# }



# long_sentiment <- NULL
# rdata_files <- dir("Sentiment/RData")
# for(i in 1:length(rdata_files)){
#   load(file.path("Sentiment/RData", rdata_files[i]))
#   #mean_v <- mean(sentiment_result)
#   long_sentiment <- rbind(long_sentiment, sentiment_result)
# }
# save(long_sentiment, file="Sentiment/Data/long_sentiment.RData")
#
# sentiment_author <- select(long_sentiment, Author, Author_Sentiment)
