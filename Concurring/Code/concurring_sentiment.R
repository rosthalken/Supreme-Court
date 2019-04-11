library(tokenizers)
library(syuzhet)
library(dplyr)
library(readr)
library(stringi)
library(ggplot2)
corpus <- "Concurring/ConcurringFiles"
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
    colnames(sentiment_df) <- c("Author","File_ID","File_Name","Mean Sentiment", "Year")
    sentiment_result <- rbind(sentiment_result, sentiment_df)
    cat(i, " ", x, "\r")
  }
}



save(sentiment_result, file="Concurring/Data/sentiment_result.RData")

load("Concurring/Data/sentiment_result.RData")
clean_df <- as.data.frame(table(sentiment_result$Author))
write.csv(clean_df, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Concurring/Results/by_author_concur.csv")

sentiment_groups <- group_by(sentiment_result, Author, Year) %>%
  summarise(year_sent = mean(`Mean Sentiment`))

# mean for entire career
sentiment_concur_mean <- group_by(sentiment_result, Author) %>%
  summarise(author_sent = mean(`Mean Sentiment`))
sentiment_concur_sd <- group_by(sentiment_result, Author) %>%
  summarise(author_sd = sd(`Mean Sentiment`))
mean_sd <- data.frame(sentiment_concur_mean, sentiment_concur_sd)
mean_sd <- subset(mean_sd, select = -c(Author.1))
write.csv(mean_sd, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Concurring/Results/sentiment_concur_mean.csv")

plotted_sentiment <- ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth() 

all_sentiment <- ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE)

# Good
smooth_sentiment <- ggplot(data=sentiment_groups, aes(x=Year, y=year_sent, group=Author)) +
  #geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE) +
  xlab("Year") +
  ylab("Sentiment")

# Good
grid_sentiment <- plotted_sentiment + 
  facet_wrap(~ Author, ncol=3) +
  theme(legend.position="none")+
  xlab("Year") +
  ylab("Sentiment")

# Divided by gender
sentiment_result$Gender="M"
sentiment_result[which(sentiment_result$Author %in% c("Ginsburg", "OConnor")), "Gender"] <- "F"
table(sentiment_result$Gender)

sentiment_gender_groups <- group_by(sentiment_result, Gender, Year) %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_gender_mean <- group_by(sentiment_result, Gender) %>%
  summarise(author_sent = mean(`Mean Sentiment`))
colnames(sentiment_gender_mean) <- c("Author", "Mean Sentiment")


smooth_gender_sentiment <- ggplot(data=sentiment_gender_groups, aes(x=Year, y=year_sent, group=Gender)) +
  geom_line(aes(linetype=Gender)) +
  #geom_smooth(aes(colour = Gender), se = FALSE) +
  geom_smooth(aes(colour = Gender)) +
  xlab("Year") +
  ylab("Sentiment")

# Men only
sentiment_breyer <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Breyer") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_kennedy <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Kennedy") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_rehnquist <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Rehnquist") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_souter <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Souter") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_stevens <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Stevens") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_scalia <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Scalia") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_thomas <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Thomas") %>%
  summarise(year_sent = mean(`Mean Sentiment`))


sentiment_male <- rbind(sentiment_breyer, sentiment_kennedy, sentiment_rehnquist, sentiment_souter, sentiment_stevens, sentiment_scalia, sentiment_thomas)

smooth_sentiment_male <- ggplot(data=sentiment_male, aes(x=Year, y=year_sent, group=Author)) +
  #geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE) +
  xlab("Year") +
  ylab("Sentiment") +
  ylim(-.05, .2)

# Women only
sentiment_ginsburg <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Ginsburg") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_oconnor <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "OConnor") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_female <- rbind(sentiment_oconnor, sentiment_ginsburg)

smooth_sentiment_female <- ggplot(data=sentiment_female, aes(x=Year, y=year_sent, group=Author)) +
  #geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE) +
  xlab("Year") +
  ylab("Sentiment") +
  ylim(-.05, .2)
