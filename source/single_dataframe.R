# load libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(ggpmisc)
library(ggrepel)
library(pROC)


## IMPORTANT MESSAGE: THE PIPELINE ALWAYS DETECT GROUNDTRUTH BUT,
## IT WRONGLY CLASSIFY THE OBJECT

# upload data
data <- read_csv("data/T32L75_detect_V8.csv")

# filter truth only
truth = data %>% filter(!truth.class == "NA")  ## The only truth data
# assign the truth length
length_ground_truth_count= length(truth$pic_name)

sum(is.na(truth)) ## AGAIN: ALL TRUTH OBJECTS CAN BE DETECTED BUT WRONGLY CLASSIFIED

#####################################  #####################################   #####################################   #####################################
#####################################  #####################################   #####################################   #####################################

# check the right detected objects in the truth data
truth_only = truth %>% mutate(detection_status = ifelse(detect.class == truth.class, "TRUE", "FALSE"))
is.na(truth_only)
truth_length = length(truth_only$pic_name)
truth_length
# detected correctly
true_detected_truth_only = truth_only %>% filter(detection_status == "TRUE") 
correct_detection = length(true_detected_truth_only$pic_name)
correct_detection

#detected incorrectly
detected_but_false = truth_only %>% filter(detection_status == "FALSE") 
incorrect_detection = length(detected_but_false$pic_name)
incorrect_detection

Overview <- data.frame(
  Truth_summary = c("Truth Length", "Correct Detection", "Incorrect Detection"),
  Value = c(truth_length, correct_detection, incorrect_detection)
)

# Calculate percentages
Overview$`Percentage(%)` <- c("100", 
                         round((correct_detection / truth_length) * 100),
                         round((incorrect_detection / truth_length) * 100))


# Print the updated data frame
print(Overview)

write.csv(Overview, file = "results/single_dataframe/Overview.csv", row.names = F)


#####################################  #####################################   #####################################   #####################################
#####################################  #####################################   #####################################   #####################################

# rewrite false over NA values in detection_status
data_NA_false = data
data_NA_false$detection_status <- ifelse(data_NA_false$truth.class == data_NA_false$detect.class, TRUE, FALSE)

data_NA_false$detection_status = data_NA_false$detection_status[is.na(data_NA_false1$detection_status)] <- "FALSE"

data_NA_false1 = na.omit(data_NA_false) ## Truly detected with detection classes
length(data_NA_false1$pic_name)


#####################################  #####################################   #####################################   #####################################
#####################################  #####################################   #####################################   #####################################
# Create a dataframe with the filenames from data and their respective versions
data_identified <- data %>%
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

# Get unique classes for the current version
version_classes <- classes  %>% pull(detect.class)

# Initialize an empty list to store results for each class
class_dataframes <- list()
# Loop over each unique class in the current version
for (class_name in version_classes) {
  
  # Filter data for the specific class and version
  class_data <- data_final %>% filter(truth.class == class_name)
  
  # Calculate metrics based on filtered data
  tp <- cumsum(class_data$is_truth)  # Cumulative True Positives
  fp <- cumsum(!class_data$is_truth)  # Cumulative False Positives
  
  # Calculate false negatives (FN) based on cumulative true positives
  fn <- sum(class_data$is_truth) - tp  # Cumulative False Negatives
  
  # Calculate total observations
  total_obs <- nrow(class_data)
  
  # Calculate true negatives (TN)
  tn <- total_obs - (tp + fp + fn)
  
  # Calculate precision and recall (sensitivity) from cumulative sums
  precision <- tp / (tp + fp)  # Precision
  recall <- tp / (tp + fn)  # Recall (Sensitivity)
  
  # Calculate rates from cumulative sums
  sensitivity <- tp / sum(class_data$is_truth)  # True Positive Rate (Sensitivity)
  fpr <- fp / sum(!class_data$is_truth)  # False Positive Rate
  specificity <- 1 - fpr  # Specificity
  
  # Prepare a dataframe for the results
  class_result <- data.frame(tp = tp,
                             fp = fp,
                             fn = fn,
                             tn = tn,  # Add TN to the dataframe
                             precision = precision,
                             recall = recall,
                             sensitivity = sensitivity,
                             fpr = fpr,
                             specificity = specificity,
                             class = class_name)
  
  # Append dataframe to the list using class_name as the list index
  class_dataframes[[class_name]] <- class_result
}

# Combine all dataframes into a single dataframe (if needed)
conf_matrix <- do.call(rbind, class_dataframes)


#####################################  #####################################   #####################################   #####################################
#####################################  #####################################   #####################################   #####################################
truth_class_counts <- table(data$truth.class)

truth_class_counts <- as.data.frame(truth_class_counts)  # Ensure truth_class_counts is a data frame
colnames(truth_class_counts) <- c("truth_class", "count_truth")


freq_table <- table(data$detect.class, data$truth.class)



# Convert frequency table to data frame and reshape it
freq_df <- as.data.frame(freq_table)
freq_df <- freq_df %>%
  mutate(detect_class = rownames(freq_df)) %>%
  gather(truth_class, value, -detect_class) %>%
  mutate(truth_class = factor(truth_class, levels = unique(truth_class)))  # Ensures proper order in the plot


colnames(freq_df) <- c("detect_class", "truth_class", "value")


# Join truth_class_counts to freq_df based on 'truth_class'
freq_df <- freq_df %>%
  left_join(truth_class_counts, by = c("truth_class" = "truth_class")) %>%
  mutate(observation_truth = count)  # Create a new column 'observation_truth' with count values

data1 = left_join(freq_df, truth_class_counts, by = "truth_class")

data1$percent = round((data1$value/data1$count_truth) *100,0)



# Plotting with ggplot
p1 = ggplot(data1, aes(x = detect_class, y = truth_class, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("#BD3E3E", "#FEB500", "#A3FE00"), values = c(0, 0.33, 0.66, 1)) +
  geom_text(aes(label = percent), color = "black", size = 5) +
  labs(title = "Confusion Matrix_T32L75",
       y = "Truth Class",
       x = "Detect Class",
       fill = "Frequency (%)") +
  custom_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1 


ggsave("results/single_dataframe/plot.png", plot = p1, bg = "white", width = 8, height = 6, units = "in", dpi = 600)


#####################################  #####################################   #####################################   #####################################
#####################################  #####################################   #####################################   #####################################
class_auc_values <- list()

for (class_name in version_classes) {
  class_data <- data_final %>% filter(truth.class == class_name)
  
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

all_class_auc_values <- as.data.frame(do.call(rbind, class_auc_values))

colnames(all_class_auc_values)[1] <- "AUC"


all_class_auc_values$AUC <- round(all_class_auc_values$AUC, digits = 2)

all_class_auc_values$category <- rownames(all_class_auc_values)


all_class_auc_values


#####################################  #####################################   #####################################   #####################################
#####################################  #####################################   #####################################   #####################################

# Initialize empty vectors to store results
classes <- unique(conf_matrix$class)
recall <- numeric(length(classes))
precision <- numeric(length(classes))
f1_score <- numeric(length(classes))

# Loop over each unique class
for (i in seq_along(classes)) {
  class_name <- classes[i]
  
  # Filter rows corresponding to the current class
  class_data <- conf_matrix[conf_matrix$class == class_name, ]
  
  # Calculate recall, precision, and F1 score
  tp <- sum(class_data$tp)
  fp <- sum(class_data$fp)
  fn <- sum(class_data$fn)
  
  if (tp + fn == 0) {
    recall[i] <- 0
  } else {
    recall[i] <- tp / (tp + fn)
  }
  
  if (tp + fp == 0) {
    precision[i] <- 0
  } else {
    precision[i] <- tp / (tp + fp)
  }
  
  if (precision[i] + recall[i] == 0) {
    f1_score[i] <- 0
  } else {
    f1_score[i] <- 2 * precision[i] * recall[i] / (precision[i] + recall[i])
  }
}

# Combine results into a dataframe
Class_indices <- data.frame(class = classes,
                      recall = recall,
                      precision = precision,
                      f1_score = f1_score)

# Print or view the results
print(Class_indices)

Class_indices[, -1] <- round(Class_indices[, -1], 2)

write.csv(Class_indices, file = "results/single_dataframe/Class_indices.csv", row.names = F)

#####################################  #####################################   #####################################   #####################################
#####################################  #####################################   #####################################   #####################################




conf_matrix_long = conf_matrix %>% select(class, tp, fp, fn, tn)
conf_matrix_long$value <- as.numeric(conf_matrix_long$value)

conf_matrix_long <- melt(conf_matrix, id.vars = "class")


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

# Aggregate to get a representative value (e.g., mean)
summary_data <- conf_matrix_long %>%
  group_by(class, variable) %>%
  summarize(value = round(max(value), 0))

# Plotting with ggplot
ggplot(summary_data, aes(x = class, y = variable, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("#FE5100", "#00FECC", "#FEB500","#A3FE00"), values = c(0, 0.33, 0.66, 1)) +
  geom_text(aes(label =  value), color = "black", size = 5) +
  custom_theme() +
  labs(title = "Confusion Matrix_T32L75",
       x = "Class",
       y = "Metrics",
       fill = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(corrplot)



# Generate the plot
p <- ggplot(conf_matrix, aes(x = fpr, y = precision, color = class)) +
  geom_ribbon(aes(ymin = 0, ymax = sensitivity, fill = "ROC_curve"), alpha = 0.1, fill = "#001BFF") +
  geom_step(size = 0.8) +
  guides(fill = "none") +
  geom_abline(intercept = 0, size = 0.5, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "False Positive Rate", y = "True Positive Rate", title = paste(data_final$version)) +
  custom_theme() 

p

# Add AUC values as annotations
p <- p +
  geom_text_repel(data = all_class_auc_values, aes(x = 0.5, y = 0.2,
                                                    label = paste0("AUC: ",category, " (", round( AUC, 3),") ")),
                  hjust = 0.7, vjust = 0.4, size = 3, color = "black")


p

ggsave("results/single_dataframe/ROC_CUrve.png", bg= "white", plot = p, width = 8, height = 6, dpi = 300)



colnames(truth)

traits_columns = c("detect.width",
                   "truth.width", 
                   "detect.length",
                   "truth.length",
                   "detect.area",
                   "truth.area")

truth_long = truth %>% pivot_longer(cols =  c("detect.width",
                                              "truth.width", 
                                              "detect.length",
                                              "truth.length",
                                              "detect.area",
                                              "truth.area"), names_to = "trait", values_to = "value")

custom_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#581845", fill = NA, size = 0.6),
      plot.title = element_text(hjust= 0.5, colour = "#581845", face = "bold.italic", size = 25),
      axis.text = element_text(color = "#581845", size = 30, face = "bold"),
      axis.title = element_text(color = "#581845", size = 18, face = "bold"),
      #axis.title.y = element_text(size = 17.5, margin = margin(t = 0, r = 30, b = 0, l = 0), color = "#581845"),  # Change the color here
      #axis.title.x = element_text(size = 17.5, margin = margin(t = 0, r = 30, b = 0, l = 0), color = "#581845"),  # Change the color here
      axis.text.x =  element_text(size = 16, color = "#581845"),
      axis.text.y = element_text(size = 16, color = "#581845"), 
      legend.text = element_text(size = 18), 
      strip.text = element_text(size = 20))
}

truth$leaf_side <- ifelse(grepl("_[1-4]$", truth$pic_name), "abaxial", "adaxial")


# Rename columns
truth <- truth %>%
  rename(detect.length = `detect.width`,
         detect.width = `detect.length`)

# Print the modified column names
print(colnames(truth))



# Create the ggplot with specified limits
p1 = ggplot(truth, aes(x=detect.length, y=truth.length, color = leaf_side))+
  geom_point(shape=1, size = 4, alpha = 1)+# color = "#036100")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 100) +  # Set x-axis limits
  ylim(0, 100) +  # Set y-axis limits
  ggtitle("Stomata length (µm)") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()+
  guides(color = FALSE)

p1

p2 = ggplot(truth, aes(x=detect.width, y=truth.width, color = leaf_side))+
  geom_point(shape=1, size = 4, alpha = 1)+# color = "#036100")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 100) +  # Set x-axis limits
  ylim(0, 100) +  # Set y-axis limits
  ggtitle("Stomata width (µm)") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()+
  guides(color = FALSE)

p2

p3  = ggplot(truth, aes(x=detect.area, y=truth.area, color = leaf_side))+
  geom_point(shape=1, size = 4, alpha = 1)+# color = "#036100")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 4000) +  # Set x-axis limits
  ylim(0, 4000) +  # Set y-axis limits
  ggtitle("Stomata area (µm^2)") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()

p3

combined_plot <- plot_grid(
  p1, p2, p3, ncol = 3) 

combined_plot
 ggsave(combined_plot, file= "results/single_dataframe.pdf", height=8, width = 25, dpi = 600)

#  =================================================================================================================
truth_leaf = truth

truth_leaf$leaf <- gsub("_\\d+$", "", truth_leaf$pic_name)

truth_leaf = truth_leaf %>%
  group_by(leaf) %>%
  summarise(
    detect_width = mean(detect.width, na.rm = TRUE),
    truth_width = mean(truth.width, na.rm = TRUE),
    detect_length = mean(detect.length, na.rm = TRUE),
    truth_length = mean(truth.length, na.rm = TRUE),
    detect_area = mean(detect.area, na.rm = TRUE),
    truth_area = mean(truth.area, na.rm = TRUE)
  )

# ===================================================================================================================

# Create the ggplot with specified limits
p4 = ggplot(truth_leaf, aes(x=detect_length, y=truth_length))+#, color = leaf_side))+
  geom_point(shape=1, size = 7, alpha = 1, color = "#581845")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 100) +  # Set x-axis limits
  ylim(0, 100) +  # Set y-axis limits
  ggtitle("Stomata length (µm) - leaf") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()+
  guides(color = FALSE)

p4

p5 = ggplot(truth_leaf, aes(x=detect_width, y=truth_width))+#, color = leaf_side))+
  geom_point(shape=1, size = 7, alpha = 1, color = "#581845")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 100) +  # Set x-axis limits
  ylim(0, 100) +  # Set y-axis limits
  ggtitle("Stomata width (µm) - leaf") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()+
  guides(color = FALSE)

p5

p6  = ggplot(truth_leaf, aes(x=detect_area, y=truth_area))+#, color = leaf_side))+
  geom_point(shape=1, size = 7, alpha = 1, color = "#581845")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 4000) +  # Set x-axis limits
  ylim(0, 4000) +  # Set y-axis limits
  ggtitle("Stomata area (µm^2) - leaf") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()

p6

leaf_plot <- plot_grid(p1 ,p2 ,p3,
                       p4, p5, p6, ncol = 3) 

leaf_plot

ggsave(leaf_plot, file= "output/plot_all.jpg", height=16, width = 25, dpi = 600)