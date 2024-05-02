# load libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(ggpmisc)
library(plyr)
library(tibble)
library(purrr)

# Get a list of all CSV files in the "data/" directory that contain "detect"
detect_files <- list.files("data/", pattern = "\\.csv$", full.names = TRUE, 
                           include.dirs = FALSE)[grepl("detect", list.files("data/", pattern = "\\.csv$"))]

# Import the detected CSV files into a list
data_list <- lapply(detect_files, read.csv)


# pipeline 1 name
p1 <- unique(data_list[[1]]$source[!data_list[[1]]$source == ""])
# rename the first output of the p1 validation 
names(data_list)[1] <- paste0(p1)

# pipeline 2 name
p2 <- unique(data_list[[2]]$source[!data_list[[2]]$source == ""])
# rename the second output of the p2 validation 
names(data_list)[2] <- paste0(p2)

# check the levels of the detected classes
p1_levels <- as.data.frame(as.character(unique(data_list[[1]]$detect.class)))
colnames(p1_levels)[1] <- unique(data_list[[1]]$source[!data_list[[1]]$source == ""])


# check the levels of the detected classes
p2_levels = as.data.frame(as.character(unique(data_list[[2]]$detect.class)))
colnames(p2_levels)[1] <- unique(data_list[[2]]$source[!data_list[[2]]$source == ""])

# ============================================================================================================================
#                                    Check the detected classes in first pipeline version
# ============================================================================================================================

# check the difference between the levels of detected classes
diff <- nrow(p2_levels) - nrow(p1_levels)

# Add empty rows to the smaller data frame
if (nrow(p1_levels) < nrow(p2_levels)) {
  p1_levels <- rbind.fill(p1_levels, data.frame(matrix(NA, nrow = diff, ncol = ncol(p1_levels))))
} else {
  p2_levels <- rbind.fill(p2_levels, data.frame(matrix(NA, nrow = diff, ncol = ncol(p2_levels))))
}

# assign the true classes that should be found
True_classes = c("complete",     
                 "blurry.complete",
                 "hair",        
                 "blurry.incomplete",
                 "incomplete",       
                 "Undetected") # Undetected was kept to check the freq of FDI

# bind the detected classes levels
detected_classes = cbind(True_classes, p1_levels[1],p2_levels[1]) %>% 
  #select(True_classes, p1_levels_detected_classes,p2_levels_detected_classes) %>% 
  mutate_all(~ ifelse(is.na(.) | . == "", "Undetected", .))


# Getting unique levels in column B
levels_True_classes <- unique(detected_classes$True_classes)

# Empty list to store results
matching_levels <- list()

# Loop through levels in True_classes
for (level in levels_True_classes) {
  # Check if level in True_classes exists in any other detected_classes columns
  p1_matched <- level %in% detected_classes[,2]
  p2_matched <- level %in% detected_classes[,3]
  # Append result to the list
  matching_levels[[level]] <- c(p1_matched, p2_matched)
}

# Assuming you have p1 and p2 as variables storing the desired column names
available_classes <- data.frame(True_classes = names(matching_levels),
                                p1 = sapply(matching_levels, `[`, 1),
                                p2 = sapply(matching_levels, `[`, 2)) 

colnames(available_classes)[2] <- p1
colnames(available_classes)[3] <- p2

# Print the result data frame
print(available_classes)   #so, incomplete and blurry.incomplete can not be detected in D_16_22_32




file_name <- c("results/detected_classes/detected_classes.csv")
write.csv(file = file_name, available_classes, row.names = FALSE)
