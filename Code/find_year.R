library(dplyr)
library(readr)
library(stringi)
library(tm)
corpus <- "SupremeCourtCorpusFinalEncoded"
the_dirs <- dir(corpus, pattern = ".Original")
the_clean <- dir(corpus, pattern = ".Cleaned")

# Reason for having the_dirs and the_clean is that the dissent marker only exists in the uncleaned files

year_clean <- NULL
case_year <- NULL
for(i in 1:length(the_dirs)){
  the_files <- dir(file.path(corpus, the_dirs[i]))
  for(x in 1:length(the_files)){
    text_v <- read.table(file = file.path(corpus, the_dirs[i], the_files[x]), sep = "\r", row.names = NULL)
    #result <- grep("19[0-9]{2}|20[0-1][0-9]", text_v)
    result <- stri_extract_first(text_v, regex = "19[7-9][0-9]|20[0-1][0-9]")
    #result_frame <- data.frame(corpus, the_dirs[i], the_files[x], result)
    year_clean <- c(year_clean, paste(corpus, the_clean[i], result, the_files[x], sep = " "))
    case_year <- rbind(case_year, data.frame(corpus, the_clean[i], result, the_files[x]))
  }
}

colnames(case_year) <- c("Corpus", "Author", "Year", "Text_ID")
save(case_year, file="Data/case_year.RData")

# Writing the alternate csv file that holds all year documents
write.csv(case_year, file = "Data/case_year.csv")




# None of this is necessary but I'm not going to delete
framing_it_all <- NULL
bigger_df <- NULL
for (i in 1:length(year_clean)) {
  corpus <- "SupremeCourtCorpusFinalEncoded"
  without_sc <- removeWords(year_clean[i], "SupremeCourtCorpusFinalEncoded ")
  if (is.na(stri_extract(without_sc, regex = "NA")) == TRUE) {
   year <- stri_extract(without_sc, regex = "19[7-9][0-9]|20[0-1][0-9]")
  } else {
    year <- 0000
    }
  justice <- stri_extract(without_sc, regex = ".+?(?= )")
  without_sc <- removeWords(without_sc, justice)
  case <- removeWords(without_sc, year)
  # case still has a space before it--need to fix
  framing_it_all <- rbind(framing_it_all, data.frame(corpus, justice, year, case))
}

View(framing_it_all)
