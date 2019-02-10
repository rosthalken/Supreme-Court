library(tokenizers)
library(syuzhet)
library(dplyr)
library(tidyr)
library(readr)
library(stringi)
library(ngram)
corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Cleaned")
metadata <- NULL

for(i in 1:length(the_dirs)){
  long_result <- NULL
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)){
    text_v <- get_text_as_string(file.path(corpus, the_dirs[i], the_files[x]))

    # remove numbers
    text_v <-  concatenate(tolower(gsub('[[:punct:] ]+',' ', text_v)))

    # ngram creation
    ngram_n <- ngram(text_v, n = 2)
    ngram_count <- get.phrasetable(ngram_n)
    ngram_df <- data.frame(ngram_count)

    # raw token counts
    ngram_rel_t <- data.frame(the_dirs[i], x, ngram_df, "N", stringsAsFactors = FALSE)
    colnames(ngram_rel_t) <- c("Author", "Text_ID", "Ngram", "Count", "Freq", "Type")
    long_result <- rbind(long_result, ngram_rel_t)

    # create a master file with metadata
    metadata <- rbind(metadata, data.frame(the_dirs[i], x, sum(ngram_rel_t$Count), the_files[x]))

    # monitor progress. . .
    cat(the_dirs[i], "---", x, the_files[x], "\n")
  }
  temp_name <- paste("Ngram/RData/", the_dirs[i], ".RData", sep="")
  save(long_result, file=temp_name)
}


# add a column for gender and set all to male
metadata <- data.frame(metadata, gender="M", stringsAsFactors = F)
colnames(metadata) <- c("Author", "Text_ID", "NumPhrase","File_name", "gender")

# replace M with F for the two female justices
metadata[which(metadata$Author %in% c("GinsburgCleaned", "OConnorCleaned")), "gender"] <- "F"

# Check results
table(metadata$gender)

##########################################################
# Roz: This next bit does not work with the way that metadata is being created above.
#  there is no year or case metadata extracted from the files as they are being
#  processed.  You could add that in, however, at line 45.

# I just changed this quickly so maybe double check
# metadata <- select(metadata, Author, Text_ID, Year, Case, NumWords, File_name, gender)

temp_name <- paste("Ngram/Data/metadata.RData", sep="")
save(metadata, file=temp_name)

# Make wide dataframes
long_form <- NULL
rdata_files <- dir("Ngram/RData")
for(i in 1:length(rdata_files)){
  load(file.path("Ngram/RData", rdata_files[i]))
  long_form <- rbind(long_form, long_result)
}
save(long_form, file="Ngram/Data/long_form.RData")


# mutate to create a unique primary key "ID" for each document and to create a "Feature" column that prefixes each token with its token type based on the "type" column
long_form <- mutate(long_form, ID=paste(Author, Text_ID, sep="_"), Feature=paste(Type, Ngram, sep="_"))

rm(long_result)
rm(metadata)

# Convert from long form to wide form sparse matrix
wide_relative_df <- select(long_form, ID, Feature, Freq) %>%
  spread(Feature, Freq, fill = 0)

save(wide_relative_df, file="Ngram/Data/wide_relative_df.RData")
rm(wide_relative_df)

# Repeat for raw counts instead of the relative frequencies
wide_raw_df <- select(long_form, ID, Feature, Count) %>%
  spread(Feature, Count, fill = 0)

save(wide_raw_df, file="Ngram/Data/wide_raw_df.RData")
rm(wide_raw_df)
