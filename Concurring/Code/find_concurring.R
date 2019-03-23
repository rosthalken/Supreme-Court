library(dplyr)
library(readr)
library(stringi)
library(tm)

corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Original")
the_clean <- dir(corpus, pattern = ".Cleaned")

# Reason for having the_dirs and the_clean is that the dissent marker only exists in the original files with front matter

concurring <- NULL

# Warnings are just EOL warnings

for(i in 1:length(the_dirs)){
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)){
    text_v <- read_lines(file.path(corpus, the_dirs[i], the_files[x]))
    result <- grep(".*concurring\\.", text_v)
    if(length(result) != 0){
      concurring <- c(concurring, paste(corpus, the_clean[i], the_files[x], sep = "//"))
    }
  }
}

# Writing the csv file that holds all dissent documents
write.csv(concurring, file = "Concurring/Data/concurring.csv")


# Moving the files into a new folder
for(i in 1:length(concurring)){
  without_sc <- removeWords(concurring[i], "SupremeCourtCorpusFinalEncoded//")
  justice <- stri_extract(without_sc, regex = ".+?(?=//)")
  main_folder <- "Concurring/ConcurringFiles"
  file_pathway <- file.path(main_folder, justice)
  file.copy(concurring[i], file_pathway)
}
