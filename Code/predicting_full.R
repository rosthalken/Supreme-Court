library(caret)
library(dplyr)
library(readr)
library(pamr)
library(tidyr)
library(syuzhet)
library(wordcloud)

# Set some parameters
user_local <- "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code"
use_existing_data <- FALSE
remove_punc <- TRUE
use_punc_data <- FALSE

# max_col_vals <- c(200,100)
max_col_vals <- c(5000, 4500, 4000, 3500, 3000, 2500, 2000, 1500, 1000, 500, 400, 300, 200, 100)

get_accuracy <- function(list_item){
  list_item$overall[1]
}

get_full_accuracy <- function(list_item, table_data){
  table_data[[list_item]]$overall["Accuracy"]
}

# This function will identify columns that have NA values
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}
# Note: first we run tokenize.R, and convert_to_wide.R and add_meta.R in the code directory to produce the two files that are loaded here  -->
load(file.path(user_local, "Data/metadata.RData"))
load(file.path(user_local, "Data/wide_relative_df.RData"))

# Ignore very short documents: e.g. docs < 1000 wds
word_threshold <- 1000
long_docs <- filter(metadata, NumWords >= word_threshold) %>%
  mutate(ID=paste(Author, Text_ID, sep="_")) %>%
  select(Author, Text_ID, ID, NumWords, gender)
long_doc_data <- wide_relative_df[which(wide_relative_df$ID %in% long_docs$ID),]
if(remove_punc){
  punctuation <- grep("^P_", colnames(long_doc_data))
  meta_full <- merge(long_docs, long_doc_data[, -punctuation])
} else {
  # Merge in the meta data
  meta_full <- merge(long_docs, long_doc_data)
}

# Now winnow the data to high frequency features that are used by all authors.
# To save processing cycles, first calculate the columns means and remove most features that are above a certain mean.
# Calculate the means for winnowing
the_means <- colMeans(meta_full[, 6:ncol(meta_full)])

#  remove names of justices.
the_means <- the_means[-which(names(the_means) %in% c("W_rehnquist", "W_scalia", "W_thomas", "W_breyer", "W_ginsburg", "W_kennedy", "W_o'connor", "W_souter", "W_stevens"))]

# First set a maximum number of possible features to retain
# We'll use values from 5000 down to 500.  Ideally the feature set
# should be small and limited to high frequency "context insensative"
# features, so we will winnow further after this initial pass

if(use_existing_data == TRUE) {
  if(use_punc_data){
    load(file.path(user_local, "Gendered_Judges/Data/classifier_data_w_punc.RData"))
    load(file.path(user_local, "Gendered_Judges/Data/features_used_sum.RData"))
  } else {
    load(file.path(user_local, "Gendered_Judges/Data/classifier_data.RData"))
    load(file.path(user_local, "Gendered_Judges/Data/features_used_sum.RData"))
  }
} else {
  train_out <- list()
  test_out <- list()
  props_out <- NULL
  listgenes_out <- list()
  features_used <- NULL
  for(i in 1:length(max_col_vals)){
    max_cols <- max_col_vals[i]
    # This will ensure that max_cols is not more than the number of cols in the matrix
    if(length(the_means) < max_cols){
      max_cols <- length(the_means)
    }
    # We'll need to know the names of the meta columns
    metacols <- colnames(meta_full)[1:5]
    # we collect the most frequent features into a vector "keepers"
    keepers <- names(sort(the_means, decreasing = TRUE)[1:max_cols])
    
    # form a new dataframe with just the metadata and feature columns
    temp_data <- meta_full[,c(metacols, keepers)]
    
    # Now remove any features that do not appear at least once in every gender
    zero_value_test <- group_by(temp_data, gender) %>%
      select(-ID, -Text_ID, -NumWords, -Author) %>%
      summarise_all(funs(sum))
    
    # reset any 0 values to NA, so we can use a function to find
    # any columns containing an "NA" (i.e. zero value)
    zero_value_test[1,which(zero_value_test[1,] == 0)] <- NA
    zero_value_test[2,which(zero_value_test[2,] == 0)] <- NA
    
    # Sending zero_value_test to the function returns a set of features
    # that were not present in all three authors and also in the 
    # unknown file. we will remove these
    remove <- nacols(zero_value_test)
    
    # remove any features that are not common to all classes
    if(length(remove) > 0){
      classing_data_full <- temp_data[, -which(colnames(temp_data) %in% remove)]
    } else {
      classing_data_full <- temp_data
    }
    
    # save information for reporting
    features_used <- rbind(
      features_used, 
      c(length(keepers), 
        length(keepers) - length(remove), 
        mean(classing_data_full[, 6])*100000, 
        mean(classing_data_full[, ncol(classing_data_full)])*100000,
        gsub("W_", "", colnames(classing_data_full)[6]),
        gsub("W_", "", colnames(classing_data_full)[ncol(classing_data_full)])
      )
    )
    
    # Now we can begin the classification experiment.
    
    # Balance the classes by undersampling
    # Setting seed for repeatability during testing.
    set.seed(8675309) # Jenny!
    
    # figure out which rows are which and then sample from the
    # larger classes based on the size of the smaller class
    m_ids <- which(classing_data_full$gender == "M")
    f_ids <- which(classing_data_full$gender == "F")
    
    small_class_size <- min(c(length(m_ids), length(f_ids)))
    
    m_keep <- sample(m_ids, small_class_size)
    f_keep <- sample(f_ids, small_class_size)
    
    # a new data frame from the sampled data
    classing_data <- classing_data_full[c(m_keep, f_keep),]
    
    # Classify USING NSC  
    # TRAIN ON 3/4 OF DATA
    set.seed(8675309)
    trainIndex <- createDataPartition(factor(classing_data$gender), p = .75, list = FALSE, times = 1)
    training <- classing_data[trainIndex,6:ncol(classing_data)]
    testing  <- classing_data[-trainIndex,6:ncol(classing_data)]
    
    # 10 x 10-fold x-validation
    fitControl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = T)
    
    sink("temp")
    
    #Build the NSC model
    nscFit <- train(x=training,
                    y = factor(classing_data$gender[trainIndex]),
                    method = "pam",
                    trControl = fitControl,
                    preProcess = c("center","scale")
    )
    sink()
    
    
    # Examine the features that the model found most useful in distinguishing between the two classes.
    just_data <- classing_data[, -which(colnames(classing_data) %in% metacols)]
    mydata <- list(x=t(just_data), y=factor(classing_data$Class), geneid=colnames(just_data))
    sink("temp")
    listgenes_out[[i]] <- data.frame(pamr.listgenes(nscFit$finalModel, mydata, nscFit$finalModel$threshold, pamr.cv(nscFit$finalModel, mydata)), stringsAsFactors = F)
    sink()
    
    # Examine how the training data was classified in x-validation
    training_data_class_pred <- predict(nscFit, newdata = training, type = "raw")
    train_cm <- confusionMatrix(data = training_data_class_pred, reference = factor(classing_data$gender[trainIndex]))
    
    train_out[[i]] <- train_cm
    
    # Now make predictions about the unseen data and examine results
    class_pred <- predict(nscFit, newdata = testing, type = "raw")
    class_probs <- predict(nscFit, newdata = testing, type = "prob")
    test_cm <- confusionMatrix(data = class_pred, reference = factor(classing_data$gender[-trainIndex]))
    test_out[[i]] <- test_cm
    
    # Show final classification result and probabilities
    props_out <- bind_rows(props_out, cbind(feature_start=max_cols, features_shared=ncol(classing_data_full)-4, class_probs))
  }
  # save copies of data so we don't have to rerun every time.
  if(remove_punc){
    file_name <- "classifier_data.RData"
  } else {
    file_name <- "classifier_data_w_punc.RData"
  }
  save(train_out, test_out, props_out, listgenes_out, file = file.path(user_local, "Data", file_name))
  save(features_used, file = file.path(user_local, "Data/features_used_sum.RData"))
}
num_features <- ncol(wide_relative_df)-1
by_author <- table(meta_full$Author)
names(by_author) <- gsub("Cleaned", "", names(by_author))
first <- paste(names(by_author)[1:8], collapse = ", ")
authors <- paste(first, "and", names(by_author)[9], collapse = "")

## Corpus Composition


clean_author <- table(gsub("Cleaned", "", metadata$Author))
clean_df <- as.data.frame(clean_author)
knitr::kable(clean_author, col.names = c("Justice", "Documents"), caption = "Documents by Justice")

bias <- group_by(metadata, gender, Author) %>%
  summarize(Documents = n()) %>%
  mutate(Author = gsub("Cleaned", "", Author)) %>%
  arrange(Author) %>%
  select(Justice = Author, Bias = gender)
knitr::kable(bias, col.names = c("Justice", "Gender"), caption = "Gender Codings")

bias_docs <- group_by(metadata, gender) %>%
  summarize(Documents = n()) %>%
  select(Bias = gender, Documents)

mean_balanced_accuracy_train <- summary(unlist(lapply(train_out, get_accuracy)))
mean_balanced_accuracy_test <- summary(unlist(lapply(test_out, get_accuracy)))

round(mean_balanced_accuracy_train["Mean"],2)
round(mean_balanced_accuracy_test["Mean"],2)

# This is where the column needs to be added
test_accs <- lapply(seq_along(test_out), get_full_accuracy, table_data=test_out)
train_accs <- lapply(seq_along(train_out), get_full_accuracy, table_data=train_out)
a <- data.frame(1:length(max_col_vals),  features_used[, 1:3], unlist(train_accs), unlist(test_accs))
knitr::kable(a, col.names = c("Model ID","Features", "Shared", "Retained","Accuracy on Training Data", "Accuracy on Test Data"), caption = "Model Accuracy")
a <- subset(a, select = -c(X3))
write.csv(a, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Results/a.csv")

round(mean_balanced_accuracy_train["Mean"],2)
round(mean_balanced_accuracy_test["Mean"],2)


combined_df <- do.call(rbind, listgenes_out) %>%
  mutate(diff_columns = abs(as.numeric(F.score) - as.numeric(M.score)))

combined_df$id <- gsub("^W_","", combined_df$id)

strong_male_diffs <- group_by(combined_df, id) %>%
  summarise(mean_f = mean(as.numeric(F.score)), mean_m = mean(as.numeric(M.score)), mean_diff = mean(diff_columns)) %>%
  arrange(desc(mean_diff))  %>%
  filter( mean_m > 0) %>%
  select(-mean_f) %>%
  mutate(G = "M") %>%
  select(id, mean_diff, G)

strong_female_diffs <- group_by(combined_df, id) %>%
  summarise(mean_f = mean(as.numeric(F.score)), mean_m = mean(as.numeric(M.score)), mean_diff = mean(diff_columns)) %>%
  arrange(desc(mean_diff)) %>%
  filter( mean_f > 0) %>%
  select(-mean_m) %>%
  mutate(G = "F") %>%
  select(id, mean_diff, G)

View(strong_female_diffs)
View(strong_male_diffs)

x <- rbind(strong_female_diffs, strong_male_diffs) %>%
  arrange(desc(mean_diff))
# Creating separate tables according to gender
female_table <- filter(x, G=="F")
male_table <- filter(x, G=="M")


write_csv(x, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Results/2x.csv")

write_csv(strong_female_diffs, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Results/2strong_female_diffs.csv")

write_csv(strong_male_diffs, "/Users/rosamondthalken/Documents/Graduate School/Thesis/Thesis Code/Results/2strong_male_diffs.csv")

knitr::kable(x[1:50,], col.names = c("Word", "Mean Difference", "Gender"), caption = "Most Useful Features")