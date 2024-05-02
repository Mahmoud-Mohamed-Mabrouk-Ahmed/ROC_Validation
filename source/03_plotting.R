

custom_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text = element_text(size = 13),
      axis.title.y = element_text(size = 13.5, face = "bold", margin = margin(t = 0, r = 15, b = 0, l = 0)),
      axis.title.x = element_text(size = 13.5, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
      plot.title = element_text(size = 18),
      strip.text = element_blank()
    )
}


# Specify the directory where CSV files are saved
results_directory <- "results/conf_matrix/"

# Get a list of CSV files in the directory
csv_files <- list.files(path = results_directory, pattern = "\\.csv$", full.names = TRUE)



# Iterate over each CSV file
for (csv_file in csv_files) {
  # Read the CSV file
  data <- read.csv(csv_file)
  
  # Extract version character from the file name
  version <- unique(data$version)  # Assuming version character is a single capital letter
  
  
  # Check if there's corresponding AUC values for this version
  if (version %in% names(version_auc_values)) {
    # Extract AUC values for the current version
    
    auc_values <- version_auc_values[[version]]
    
    # classes <- unlist(lapply(auc_values, names))
    
    # Create a data frame for AUC values
    auc_data <- data.frame(
      category = names(auc_values),
      auc = unlist(auc_values)
    )
    
  
  # Generate the plot
  p <- ggplot(data, aes(x = fpr, y = sensitivity, color = class)) +
    geom_ribbon(aes(ymin = 0, ymax = sensitivity, fill = "ROC_curve"), alpha = 0.1, fill = "#001BFF") +
    geom_step(size = 0.8) +
    guides(fill = "none") +
    geom_abline(intercept = 0, size = 0.5, slope = 1, linetype = "dashed", color = "black") +
    labs(x = "False Positive Rate", y = "True Positive Rate", title = paste( version)) +
    custom_theme() 
  
  # Add AUC values as annotations
  p <- p +
    geom_text_repel(data = auc_data, aes(x = 0.5, y = 0.2,label = paste0("AUC: ",category, " (", round( auc, 3),") ")),
                    hjust = 0.6, vjust = 0.3, size = 5, color = "black")
  
  # Print and save the plot
  plot_file <- paste("results/plots/", "plot_", basename(csv_file), ".png", sep = "")
  ggsave(plot_file, p, width = 10, height = 8, dpi = 900, bg = "white")
  
  # Print the plot with annotations
  print(p)
  }
}
