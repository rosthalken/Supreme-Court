#TEST
library(tokenizers)
library(syuzhet)
library(dplyr)
library(readr)
library(stringi)
corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Cleaned")
metadata <- NULL

for(i in 1:length(the_dirs)){
  long_result <- NULL
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)){
    text_v <- get_text_as_string(file.path(corpus, the_dirs[i], the_files[x]))
    # remove numbers
    text_v <- gsub("\\d+", " ", text_v)
    word_tokens <- tokenize_words(text_v)
    
    # raw token counts
    word_t_raw <- data.frame(the_dirs[i], x, table(word_tokens), "W", stringsAsFactors = FALSE)
    colnames(word_t_raw) <- c("Author", "Text_ID", "Token", "Count", "Type")
    
    # calculate relative frequencies
    rels <- word_t_raw$Count/sum(word_t_raw$Count)
    word_t <- data.frame(word_t_raw, Freq=rels, stringsAsFactors = FALSE)
    long_result <- rbind(long_result, word_t)
    
    # Now get data about punctuation usage
    chars <- tokenize_characters(text_v, lowercase = TRUE, strip_non_alphanum = FALSE, simplify = TRUE)
    chars <-  gsub("\\s+", "", chars) # remove spaces
    chars <-  gsub("[a-z]", "", chars) # remove letters
    chars <-  gsub("\\d+", "", chars) # remove numbers
    punc <-  chars[-which(chars == "")] # remove blanks
    
    # normalize apostrophes
    punc <- gsub("’", "'", punc)
    punc_t_raw <- data.frame(the_dirs[i], x, table(punc), "P", stringsAsFactors = FALSE)
    colnames(punc_t_raw) <- c("Author", "Text_ID", "Token", "Count", "Type")
    p_rels <- punc_t_raw$Count/sum(punc_t_raw$Count)
    punc_t <- data.frame(punc_t_raw, Freq=p_rels, stringsAsFactors = FALSE)
    long_result <- rbind(long_result, punc_t)
    
    # create a master file with metadata
    metadata <- rbind(metadata, data.frame(the_dirs[i], x, sum(word_t$Count), sum(punc_t$Count), the_files[x]))
    
    # monitor progress. . . 
    cat(the_dirs[i], "---", x, the_files[x], "\n")
  }
  temp_name <- paste("RData/", the_dirs[i], ".RData", sep="")
  save(long_result, file=temp_name)
}

colnames(metadata) <- c("Author", "Text_ID", "NumWords","NumPuncs", "File_name")
save(metadata, file="Data/metadata.RData")

# Load and combine rdata files into one long result
long_form <- NULL
rdata_files <- dir("RData")
for(i in 1:length(rdata_files)){
  load(file.path("RData", rdata_files[i]))
  long_form <- rbind(long_form, long_result)
}
save(long_form, file="Data/long_form.RData")