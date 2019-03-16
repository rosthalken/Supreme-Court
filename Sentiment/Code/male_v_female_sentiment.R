load("Sentiment/Data/sentiment_result.RData")

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
