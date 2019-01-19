library(dplyr)
library(readr)
library(stringi)
corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Original")

dissenting <- NULL

# dont worry about the warnings that this loop throws.  They are just EOL warnings.

for(i in 1:length(the_dirs)){
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)){
    text_v <- read.table(file = file.path(corpus, the_dirs[i], the_files[x]), sep = "\r", row.names = NULL)
    result <- grep(".*dissenting\\.", text_v)
    if(length(result) != 0){ # Hi Roz: I figured this out 30 seconds after you left :-)
      dissenting <- c(dissenting, paste(corpus, the_dirs[i], the_files[x]))
    }
  }
}

# Save out to the Data directory
write.csv(dissenting, file = "Data/dissenting.csv")