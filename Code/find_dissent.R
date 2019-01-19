library(dplyr)
library(readr)
library(stringi)
corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Original")
the_better <- dir(corpus, pattern = ".Cleaned")
new_folder <- dir.create("DissentFiles")

dissenting <- NULL

# dont worry about the warnings that this loop throws.  They are just EOL warnings.

for(i in 1:length(the_dirs)){
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)){
    text_v <- read.table(file = file.path(corpus, the_dirs[i], the_files[x]), sep = "\r", row.names = NULL)
    result <- grep(".*dissenting\\.", text_v)
    if(length(result) != 0){
      dissenting <- c(dissenting, paste(corpus, the_better[i], the_files[x]))
    }
  }
}

# Save out to the Data directory
write.csv(dissenting, file = "Data/dissenting.csv")

# now I need to use this csv file to find the file names and create a new dissent corpus

