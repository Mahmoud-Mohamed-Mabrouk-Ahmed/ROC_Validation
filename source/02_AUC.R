
# Initialize empty lists to store AUC values
version_auc_values <- list()
merged_version_auc_values = list()


for (version_name in unique(data_final$version)) {
  cat("Processing version:", version_name, "\n")
  
  version_data <- data_final %>% filter(version == version_name)
  version_classes <- classes %>% filter(version == version_name) %>% pull(detect.class)
  
  class_auc_values <- list()
  
  for (class_name in version_classes) {
    class_data <- version_data %>% filter(truth.class == class_name)
    
    # Check if the response variable has exactly two unique levels
    unique_levels <- unique(class_data$is_truth)
    if (length(unique_levels) != 2) {
      cat("Error: Response variable has", length(unique_levels), "unique level(s) in class", class_name, "of version", version_name, "\n")
      next  # Skip to the next class
    }
    
    roc_data <- roc(class_data$is_truth, class_data$confidence)
    auc_value <- auc(roc_data)
    
    class_auc_values[[class_name]] <- auc_value
  }
  
  version_auc_values[[version_name]] <- class_auc_values
  merged_version_auc_values[[version_name]] <- do.call(rbind, version_auc_values[[version_name]])
  
  file_name <- paste("results/AUC/","AUC_", version_name, ".csv", sep="")
  write.csv(merged_version_auc_values[[version_name]], file = file_name, row.names = FALSE)
}

print(merged_version_auc_values)










