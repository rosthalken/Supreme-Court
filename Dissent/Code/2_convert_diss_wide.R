library(dplyr)
library(readr)
library(tidyr)

# goal here is to convert from long form to wide form sparse matix with all data both word and punctuation tokens as columns

# Load the long for Data
load("Data/long_form.RData")

# mutate to create a unique primary key "ID" for each document and to create a "Feature" column that prefixes each token with its token type based on the "type" column
long_form <- mutate(long_form, ID=paste(Author, Text_ID, sep="_"), Feature=paste(Type, Token, sep="_"))

# Convert from long form to wide form sparse matrix
wide_relative_df <- select(long_form, ID, Feature, Freq) %>% 
  spread(Feature, Freq, fill = 0)

save(wide_relative_df, file="Data/wide_relative_df.RData")
rm(wide_relative_df)

# Repeat for raw counts instead of the relative frequencies
wide_raw_df <- select(long_form, ID, Feature, Count) %>% 
  spread(Feature, Count, fill = 0)

save(wide_raw_df, file="Data/wide_raw_df.RData")
rm(wide_raw_df)