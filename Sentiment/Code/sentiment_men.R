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

plotted_sentiment_male <- ggplot(data=sentiment_male, aes(x=Year, y=year_sent, group=Author)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth() +
  xlab("Year") +
  ylab("Sentiment")

all_sentiment_male <- ggplot(data=sentiment_male, aes(x=Year, y=year_sent, group=Author)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE)

smooth_sentiment_male <- ggplot(data=sentiment_male, aes(x=Year, y=year_sent, group=Author)) +
  #geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE) +
  xlab("Year") +
  ylab("Sentiment") +
  ylim(-.05, .25) +
  xlim(1970, 2020)

plotted_sentiment_male <- ggplot(data=sentiment_male, aes(x=Year, y=year_sent)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth() +
  xlab("Year") +
  ylab("Sentiment")

smooth_sentiment_male <- ggplot(data=sentiment_male, aes(x=Year, y=year_sent)) +
  #geom_line(aes(linetype=Author)) +
  geom_smooth() +
  xlab("Year") +
  ylab("Sentiment")
