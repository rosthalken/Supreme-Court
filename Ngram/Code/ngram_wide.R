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

# mutate to create a unique primary key "ID" for each document and to create a "Feature" column that prefixes each token with its token type based on the "type" column
long_form <- mutate(long_form, ID=paste(Author, Text_ID, sep="_"), Feature=paste(Type, Ngram, sep="_"))

# Convert from long form to wide form sparse matrix.
# The following code is producing "Error: Duplicate identifiers for rows..."
wide_relative_df <- select(long_form, ID, Feature, Freq) %>%
    spread(Feature, Freq, fill = 0)

#  I have tried multiple other avenues including reshape and xtabs.  All produce same memory issue.
#  how about dcast in data.table. . .
library(data.table)
df <- select(long_form, ID, Feature, Freq)
setDT(df)
test <- dcast(df, ID ~ Feature, value.var = "Freq", fill = 0)
# let's try to process a subset of 1M of the full long data. . .
test <- dcast(df[1:1000000], ID ~ Feature, value.var = "Freq", fill = 0)
# The next line reveals a problem with the bigrams. . . .
test[1:10, 1:5]

# stopping here. . . .

save(wide_relative_df, file="Ngram/Data/wide_relative_df.RData")
rm(wide_relative_df)

# Repeat for raw counts instead of the relative frequencies
wide_raw_df <- select(long_form, ID, Feature, Count) %>%
  spread(Feature, Count, fill = 0)

save(wide_raw_df, file="Ngram/Data/wide_raw_df.RData")
rm(wide_raw_df)
