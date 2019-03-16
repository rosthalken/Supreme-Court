library(tokenizers)
library(syuzhet)
library(dplyr)
library(tidyr)
library(readr)
library(stringi)
library(ngram)
user_local <- "/Users/matthew.jockers/Documents/Students/Thalken/thesis/Ngram"

load(file.path(user_local, "Data/long_form.RData"))

load("Ngram/Data/long_form.RData")
load("Ngram/Data/metadata.RData")
load("Ngram/RData/long_result")

# mutate to create a unique primary key "ID" for each document and to create a "Feature" column that prefixes each token with its token type based on the "type" column
long_form <- mutate(long_result, ID=paste(Author, Text_ID, sep="_"), Feature=paste(Type, Ngram, sep="_"))

myData <- myData[-c(2, 4, 6), ]

long_without <- long_form[long_form$Ngram[-rare]]

word_threshold <- 500
# gender working with long_docs
long_docs <- filter(metadata, NumPhrase >= word_threshold) %>%
  mutate(ID=paste(Author, Text_ID, sep="_")) %>%
  select(Author, Text_ID, ID, NumPhrase, gender)
long_doc_data <- long_form[which(long_form$ID %in% long_docs$ID),]




distinct <- long_doc_data %>% distinct(Ngram)

distinct_v <- distinct$Ngram
  


# df <- long_form
# rownames(df) = make.names(df$ID, unique=TRUE)

# Convert from long form to wide form sparse matrix.
# The following code is producing "Error: Duplicate identifiers for rows..."
wide_relative_df <- select(long_form, ID, Feature, Freq) %>%
    spread(Feature, Freq, fill = 0)

wide_relative_df <- select(long_doc_data, ID, Feature, Freq) %>%
  spread(Feature, Freq, fill = 0)


zero_value_test <- select(long_doc_data, ID, Ngram)
  
arranged <- arrange(zero_value_test, Ngram)

phrases_only <- select(arranged, Ngram)
phrases <- phrases_only$Ngram

one_use <- phrases_sorted$phrases[which(phrases_sorted$Freq == 1)]
zero_use <- phrases_sorted$phrases[which(phrases_sorted$Freq == 0)]
      
rare <- c(one_use, zero_use)


phrases_sorted <- data.frame(sort(table(phrases)))

# phrases_top <- sort(table(phrases),  TRUE)


rare <- NULL
for(i in 1:length(distinct_v[1:300])){
  #number <- select(long_doc_data, Ngram) +
    #filter(Ngram == "distinct[i]")
  phrase <- distinct_v[12]
  length(which(arranged$Ngram == phrase))
  #number <- filter(long_doc_data, Ngram == phrase)
  length <- length(number$Count)
  if(length(number) == 1) {
    rare <- rbind(rare, distinct[i])
  }
  cat(phrase, "---", i, "\n")
}

if(length(remove) > 0){
  classing_data_full <- temp_data[, -which(colnames(temp_data) %in% remove)]
} else {
  classing_data_full <- temp_data
}


#  I have tried multiple other avenues including reshape and xtabs.  All produce same memory issue.
#  how about dcast in data.table. . .
# library(data.table)
#
# df <- select(long_form, ID, Feature, Freq)
# setDT(df)
# test <- dcast(df, ID ~ Feature, value.var = "Freq", fill = 0)
# # let's try to process a subset of 1M of the full long data. . .
# test <- dcast(df[1:1000000], ID ~ Feature, value.var = "Freq", fill = 0)
# # The next line reveals a problem with the bigrams. . . .
# test[1:10, 1:5]

# stopping here. . . .

save(wide_relative_df, file="Ngram/Data/wide_relative_df.RData")
rm(wide_relative_df)

# Repeat for raw counts instead of the relative frequencies
wide_raw_df <- select(long_form, ID, Feature, Count) %>%
  spread(Feature, Count, fill = 0)

save(wide_raw_df, file="Ngram/Data/wide_raw_df.RData")
rm(wide_raw_df)
