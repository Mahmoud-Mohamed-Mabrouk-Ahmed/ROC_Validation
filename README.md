# ROC_Validation
The repository is established to assess the precision of various object detection pipelines by employing ROC and AUC metrics.

Here are the quick steps for easy implementation:
1. Please clone the repository to your own drive
2. Put your check and detect files in "ROC_validation/data/"
3. Open ROC.Rproj
4. 00_detected_classes_check in ROC_validation/source" and run the lines to get the first glimpse of the detected classes in each pipeline
5. Open 01_conf_matrix in "ROC_validation/source" and run the lines, this step will calculate the confusion matrices for each pipeline version and,
the output will be saved in "results/conf_matrix/"
6. Open 02_AUC in "ROC_validation/source/" and run the lines, so the AUC (area under curve) will be calculated and stored in "results/AUC/"
7. Finally, open 03_plotting, where the plots will be stored in "results/plots/"

# Decision making
The higher the AUC, the better the object detection "if they are detectable"
In case the AUC are equal for two classes, then the decision should be based on sensitivity vs specificity preferences which can be decided by checking the shape of the ROC curve.

I hope it works fine ^^
