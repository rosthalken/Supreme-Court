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
  sentiment_result <- NULL
  for(x in 1:length(the_files)) {
    text_v <- get_text_as_string(file.path(corpus, the_dirs[i], the_files[x]))
    sentences <- get_sentences(text_v)
    sentiment_n <- get_sentiment(sentences, method = "syuzhet")
    mean_sent <- mean(sentiment_n)
    # year <- filter(metadata, Text_ID == 1, Author == the_dirs[3]) %>%
    #   select(Year)
    year <- filter(case_year, File_Name == the_files[x], Author == the_dirs[i]) %>%
      select(Year)
    theyear <- year$Year
    sentiment_df <- data.frame(the_dirs[i], x, the_files[x], mean_sent, theyear, stringsAsFactors = FALSE)
    colnames(sentiment_df) <- c("Author","File_ID","File_Name","Mean Sentiment", "Year")
    sentiment_result <- rbind(sentiment_result, sentiment_df)
    cat(i, " ", x, "\r")
  }
  temp_name <- paste("Sentiment/RData/", the_dirs[i], ".RData", sep="")
  save(sentiment_result, file=temp_name)
}

rdata_files <- dir("Sentiment/RData")
for(i in 1:length(rdata_files)){
  load(file.path("Sentiment/RData", rdata_files[i]))
  sentiment_linear <- group_by(sentiment_result, Author, Year) %>%
    summarise(year_sent = mean(`Mean Sentiment`))
  author_plotted <- ggplot(data=sentiment_linear, aes(x=Year, y=year_sent, group=Author)) +
    geom_line(aes(linetype=Author)) +
    geom_smooth()
  temp_name <- paste("Sentiment/Results/", sentiment_result$Author[1], ".pdf", sep="")
  #save(author_plotted, file=temp_name)
  ggsave(temp_name)
}



hist(Temperature, col="violet")
dev.off()

