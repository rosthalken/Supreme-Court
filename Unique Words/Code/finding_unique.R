library(tokenizers)
library(syuzhet)
library(dplyr)
library(readr)
library(stringi)
library(tidyr)
corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Cleaned")
#metadata <- NULL


for(i in 1:length(the_dirs)){
  unique_df <- NULL
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)){
    text_v <- get_text_as_string(file.path(corpus, the_dirs[i], the_files[x]))
    # remove numbers
    text_v <- gsub("\\d+", " ", text_v)
    text_v <- gsub("to the location of the note in the document", " ", text_v)
    text_v <- gsub("Link to the location of the note in the document", " ", text_v)
    text_v <- gsub("to the text of the note", " ", text_v)
    text_v <- gsub("Link to the text of the note", " ", text_v)
    text_v <- gsub("LEdHN", " ", text_v)
    text_v <- gsub("HN", " ", text_v)
    text_v <- gsub(",Äôs", "'s", text_v)
    text_v <- gsub("internal quotation marks omitted", " ", text_v)
    word_tokens <- tokenize_words(text_v)
    word_t <- table(word_tokens)
    word_df <- data.frame(the_dirs[i], word_t)
    
    # create a master file with metadata
    unique_df <- rbind(unique_df, word_df)
    
    # monitor progress. . . 
    cat(the_dirs[i], "---", x, the_files[x], "\n")
  }
  # add table?
  temp_name <- paste("Unique Words/RData/", the_dirs[i], ".RData", sep="")
  save(unique_df, file=temp_name)
}
long_unique <- NULL
rdata_files <- dir("Unique Words/RData/")
for(i in 1:length(rdata_files)){
  load(file.path("Unique Words/RData/", rdata_files[i]))
  long_unique <- rbind(long_unique, unique_df)
}

colnames(long_unique) <- c("Author", "Word", "Freq")

sentiment_groups <- group_by(sentiment_result, Author, Year) %>%
  summarise(year_sent = mean(`Mean Sentiment`))

words <- group_by(long_unique, Author) %>%
  summarise(unique_words = length(`Word`))


