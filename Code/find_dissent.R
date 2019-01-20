library(dplyr)
library(readr)
library(stringi)
library(tm)
corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Original")
the_better <- dir(corpus, pattern = ".Cleaned")

dissenting <- NULL

# dont worry about the warnings that this loop throws.  They are just EOL warnings.

for(i in 1:length(the_dirs)){
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)){
    text_v <- read.table(file = file.path(corpus, the_dirs[i], the_files[x]), sep = "\r", row.names = NULL)
    result <- grep(".*dissenting\\.", text_v)
    if(length(result) != 0){
      dissenting <- c(dissenting, paste(corpus, the_better[i], the_files[x], sep = "//"))
    }
  }
}

write.csv(dissenting, file = "Data/dissenting.csv")

# justice <- the_dirs[i]
# main_folder <- "DissentFiles"
# file_path <- file.path(main_folder, justice)
# file.copy(dissenting[x], file_path)

# dissenting[1]
# [1] "SupremeCourtCorpusFinalEncoded BreyerCleaned Abrams v Johnson.txt"

for(i in 1:length(dissenting)){
  without_sc <- removeWords(dissenting[i], "SupremeCourtCorpusFinalEncoded//")
  justice <- stri_extract(without_sc, regex = ".+?(?=//)")
  main_folder <- "DissentFiles"
  file_pathway <- file.path(main_folder, justice)
  file.copy(dissenting[i], file_pathway)
}

without_sc <- removeWords(dissenting[1], "SupremeCourtCorpusFinalEncoded ")
justice <- stri_extract(without_sc, regex = ".+?(?= )")
main_folder <- "DissentFiles"
file_pathway <- file.path(main_folder, justice)
file.copy(dissenting[1], file_pathway)

# without_sc <- removeWords(dissenting[1], "SupremeCourtCorpusFinalEncoded ")
# stri_extract(without_sc, regex = ".+?(?= )")


# Save out to the Data directory
file.copy(dissenting[1], "DissentFiles")

write.csv(dissenting, file = "Data/dissenting.csv")

# now I need to use this csv file to find the file names and create a new dissent corpus

