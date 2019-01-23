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

# Practice

# text_con <- concatenate(text_v)
# string.summary(text_con)
# bigram_sc <- ngram(text_con, n = 2, sep = "_")
# print(bigram_sc, output = "full")
# get.phrasetable(bigram_sc)
# ####
# trigram_sc <- ngram(text_con, n = 3, sep = "_")
# print(trigram_sc, output = "full")
# get.phrasetable(trigram_sc)
#
the_files <- dir(file.path(corpus, the_dirs[1]))
text_v <- get_text_as_string(file.path(corpus, the_dirs[1], the_files[1]))
# remove numbers
text_v <- gsub("\\d+", " ", text_v)
#remove punctuation and lower
text_v <-  tolower(gsub('[[:punct:] ]+',' ',text_v))
text_v <- concatenate(text_v)
#ngram
ngram_n <- ngram(text_v, n = 2)
ngram_t <- get.phrasetable(ngram_n)
ngram_df <- data.frame(ngram_t)
colnames(ngram_df) <- c("Ngrams", "Freq", "Rel")
full_df <- data.frame(the_dirs[1], 1, ngram_df, "N", stringsAsFactors = FALSE)
colnames(full_df) <- c("Author", "File", "Ngram", "Freq", "Rel")
as.numeric(full_df$Freq)

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
    metadata <- rbind(metadata, data.frame(the_dirs[i], x, ngram_rel_t$Count, the_files[x]))
    
    # monitor progress. . . 
    cat(the_dirs[i], "---", x, the_files[x], "\n")
  }
  temp_name <- paste("Ngram/RData/", the_dirs[i], ".RData", sep="")
  save(long_result, file=temp_name)
}

load("Ngram/Data/metadata.RData")

colnames(metadata) <- c("Author", "Text_ID", "NumPhrase","File_name", "Case")



# Load and combine rdata files into one long result
long_form <- NULL
rdata_files <- dir("Ngram/RData")
for(i in 1:length(rdata_files)){
  load(file.path("Ngram/RData", rdata_files[i]))
  long_form <- rbind(long_form, long_result)
}
save(long_form, file="Ngram/Data/long_form.RData")


# Load the long for Data
load("Ngram/Data/long_form.RData")

# mutate to create a unique primary key "ID" for each document and to create a "Feature" column that prefixes each token with its token type based on the "type" column
long_form <- mutate(long_form, ID=paste(Author, Text_ID, sep="_"), Feature=paste(Type, Ngram, sep="_"))

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

# load the metadata which was copied from Political Judges into this directory.
load("Ngram/Data/metadata.RData")

# add a column for gender and set all to male
metadata <- data.frame(metadata, gender="M", stringsAsFactors = F)

# replace M with F for the two female justices
metadata[which(metadata$Author %in% c("GinsburgCleaned", "OConnorCleaned")), "gender"] <- "F"

# Check results
table(metadata$gender)

# I just changed this quickly so maybe double check
metadata <- select(metadata, Author, Text_ID, Year, Case, NumWords, File_name, gender)

# resave metadata
save(metadata, file = "Ngram/Data/metadata.RData")

