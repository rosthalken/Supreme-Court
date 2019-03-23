library(syuzhet)
sentence <- "But, he says, what Bronte tells you is they are not the same. Each one of those families is different, and they each have a story to tell. Each of those stories involves something about human passion. Each of those stories involves a man, a woman, children, families, work, lives. And you get that sense out of the book. So sometimes, I have found literature very helpful as a way out of the tower. "
sentences_actual <- get_sentences(sentence)
sentiment_test <- get_sentiment(sentences_actual, method = "syuzhet")
mean(sentiment_test)
