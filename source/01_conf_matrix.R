library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(ggpmisc)
library(plyr)
library(tibble)
library(purrr)
library(stringr)
library(pROC)
library(ggrepel)


# Get a list of all CSV files in the "data/" directory that contain "detect"
get_data <- list.files("data/", pattern = "\\.csv$", full.names = TRUE, 
                   include.dirs = FALSE)[grepl("detect", list.files("data/", pattern = "\\.csv$"))]

# Import the detected CSV files into a list
data_list <- lapply(get_data, read.csv)

# Import the detected CSV files into a list
raw_data <- do.call(rbind, data_list)

# Extract unique versions from the source column of each dataframe in data_list
versions_id <- lapply(data_list, function(df) unique(df$source[df$source != ""]))

# Combine the unique versions into a single vector
all_versions <- unique(unlist(versions_id))

# Create a dataframe with the filenames from data and their respective versions
data_identified <- raw_data %>%
  mutate(version = gsub("\\.csv", "", source))



  data_final <- data_identified %>%
      na.omit() %>% 
      select(version, pic_name, detect.class, truth.class, confidence) %>%
      arrange(version, desc(confidence)) %>%
      mutate(is_truth = (detect.class == truth.class))


  
  classes <- data_final %>%
    group_by(version) %>%
    distinct(detect.class) %>%
    arrange(version)


# Loop over each unique version
unique_versions <- unique(data_final$version)

data_final$version = as.factor(data_final$version)

# ==============================================================================================================================
# ==============================================================================================================================
# ==============================================================================================================================

# Initialize a list to store dataframes for each version
version_dataframes <- list()

# Initialize a list to store merged dataframes for each version
merged_version_dataframes <- list()

# Loop over each unique version
for (version_name in unique(data_final$version)) {
  
  # Filter data for the current version
  version_data <- data_final %>% filter(version == version_name)
    # Get unique classes for the current version
  version_classes <- classes %>% filter(version == version_name) %>% pull(detect.class)
    # Initialize a list to store dataframes for each class in the current version
  class_dataframes <- list()
  
  # Loop over each unique class in the current version
  for (class_name in version_classes) {
        # Filter data for the specific class and version
    class_data <- version_data %>% filter(truth.class == class_name)
    
    # Calculate metrics
    tp <- cumsum(class_data$is_truth)
    fp <- cumsum(!class_data$is_truth)
    sensitivity <- tp / sum(class_data$is_truth)
    fpr <- fp / sum(!class_data$is_truth)
    specificity <- 1 - fpr
    
    # Store results in a dataframe
    class_result <- data.frame(tp, fp, sensitivity, fpr, specificity, class = class_name, version = version_name)
        # Append dataframe to the list
    class_dataframes[[class_name]] <- class_result
  }
  
  # Store dataframes for each class in the current version
  version_dataframes[[version_name]] <- class_dataframes
    # Merge all dataframes for the current version into one dataframe
  merged_version_dataframes[[version_name]] <- do.call(rbind, version_dataframes[[version_name]])
    # Save the merged dataframe to a CSV file
  file_name <- paste("results/conf_matrix/","version_", version_name, ".csv", sep="")
  write.csv(merged_version_dataframes[[version_name]], file = file_name, row.names = FALSE)
}

