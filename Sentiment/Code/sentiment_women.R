sentiment_ginsburg <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "Ginsburg") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_oconnor <- group_by(sentiment_result, Author, Year) %>%
  filter(Author == "OConnor") %>%
  summarise(year_sent = mean(`Mean Sentiment`))

sentiment_female <- rbind(sentiment_oconnor, sentiment_ginsburg)

sentiment_female$Gender="Female"

plotted_sentiment_female <- ggplot(data=sentiment_female, aes(x=Year, y=year_sent, group=Author)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth() 

all_sentiment_female <- ggplot(data=sentiment_female, aes(x=Year, y=year_sent, group=Author)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE)

smooth_sentiment_female <- ggplot(data=sentiment_female, aes(x=Year, y=year_sent, group=Author)) +
  #geom_line(aes(linetype=Author)) +
  geom_smooth(aes(colour = Author), se = FALSE) +
  xlab("Year") +
  ylab("Sentiment") +
  ylim(-.05, .3) +
  xlim(1970, 2020)

# Added stuff (look at sentiment_result):
plotted_sentiment_female <- ggplot(data=sentiment_female, aes(x=Year, y=year_sent)) +
  geom_line(aes(linetype=Author)) +
  geom_smooth() 

smooth_sentiment_female <- ggplot(data=sentiment_female, aes(x=Year, y=year_sent)) +
  #geom_line(aes(linetype=Author)) +
  geom_smooth() +
  xlab("Year") +
  ylab("Sentiment")



both_sentiment <- ggplot(data=sentiment_female, aes(x=Year, y=year_sent)) +
  ggplot(data=sentiment_male, aes(x=Year, y=year_sent)) +
  xlab("Year") +
  ylab("Sentiment")
