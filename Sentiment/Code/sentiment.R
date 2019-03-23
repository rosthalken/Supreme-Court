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
    author <- gsub("Cleaned", "", the_dirs[i])
    sentiment_df <- data.frame(author, x, the_files[x], mean_sent, theyear, stringsAsFactors = FALSE)
    colnames(sentiment_df) <- c("Author","File_ID","File_Name","Mean_Sentiment", "Year")
    sentiment_result <- rbind(sentiment_result, sentiment_df)
    cat(i, " ", x, "\r")
  }
}



save(sentiment_result, file="Sentiment/Data/sentiment_result.RData")
write.csv(sentiment_result, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Sentiment/Results/sentiment_result.csv")
load("Sentiment/Data/sentiment_result.RData")



# Adding mean of sentiment throughout career
sentiment_mean <- group_by(sentiment_result, Author) %>%
  summarise(author_sent = mean(`Mean Sentiment`))
sentiment_sd <- group_by(sentiment_result, Author) %>%
  summarise(author_sent = sd(`Mean Sentiment`))
mean_sd <- data.frame(sentiment_mean, sentiment_sd)
mean_sd <- subset(mean_sd, select = -c(Author.1))
colnames(mean_sd) <- c("Author", "Mean Sentiment", "Standard Deviation")
write.csv(mean_sd, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Sentiment/Results/sentiment_mean_sd.csv")

new <- rbind(sentiment_result, sentiment_mean)


sentiment_groups <- group_by(sentiment_result, Author, Year) %>%
  summarise(year_sent = mean(`Mean Sentiment`))
write.csv(sentiment_groups, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Sentiment/Results/sentiment_groups.csv")




# ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
#   geom_line(aes(linetype=Author)) +
#   geom_smooth() +
#   facet_grid(rows = vars(Author))

plotted_sentiment <- ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth() 

# all_sentiment <- ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
#   geom_line(aes(linetype=Author)) +
#   geom_smooth(aes(colour = Author), se = FALSE)

smooth_sentiment <- ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
  #geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE) +
  ylim(-.20, .20) +
  xlab("Year") +
  ylab("Sentiment")


grid_sentiment <- plotted_sentiment + 
  facet_wrap(~ Author, ncol=3) +
  theme(legend.position="none")+
  xlab("Year") +
  ylab("Sentiment")


