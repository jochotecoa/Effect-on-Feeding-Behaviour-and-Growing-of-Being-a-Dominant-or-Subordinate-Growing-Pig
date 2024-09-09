library(tidyverse)
library(ggplot2)
library(readxl)
VI1_modelos_PLSDA <- read_excel("xlsx/VI1_modelos_PLSDA.xlsx") %>% 
  na.omit()
View(VI1_modelos_PLSDA)

VI1_modelos_PLSDA$month <- factor(VI1_modelos_PLSDA$month, levels = c("July", "September", "October"))

tp_tn = data.frame(true_positives = c(VI1_modelos_PLSDA$predicted.as.control[VI1_modelos_PLSDA$group=="control"],
                       VI1_modelos_PLSDA$predicted.as.stress[VI1_modelos_PLSDA$group=="stress"]),
                       false_positives = c(VI1_modelos_PLSDA$predicted.as.stress[VI1_modelos_PLSDA$group=="control"],
                                           VI1_modelos_PLSDA$predicted.as.control[VI1_modelos_PLSDA$group=="stress"]), 
                       group = c(rep(c("control", "stress"), each = 3)),
                       month = rep(c("July", "September", "October"), 2))

tp_tn$month <- factor(tp_tn$month, levels = c("July", "September", "October"))

# plot the data
ggplot(tp_tn, aes(x = month, y = true_positives, color = group, group = group)) +
  geom_line() +
  geom_point() +
  labs(title = "PLSDA True Positives by Group and Month",
       x = "Month", y = "True Positive (%)",
       color = "Group")

ggplot(tp_tn, aes(x = month, y = false_positives, color = group, group = group)) +
  geom_line() +
  geom_point() +
  labs(title = "PLSDA False Positives Values by Group and Month",
       x = "Month", y = "False Positives (%)",
       color = "Group")

precision_total = as.numeric()
recall_total = as.numeric()
F1_total = as.numeric()
accuracy_total = as.numeric()

for (mnth in c("July", "September", "October")) {
  month = VI1_modelos_PLSDA %>% 
    filter(month == !!mnth) %>% 
    tibble::column_to_rownames("group")
  
  TN = month[1, 1] %>% as.numeric()
  TP = month[2, 2] %>% as.numeric()
  FN = month[2, 1] %>% as.numeric()
  FP = month[1, 2] %>% as.numeric()
  
  precision = TP / (TP + FP)
  recall = TP / (TP + FN)
  F1 = 2 * (precision * recall) / (precision + recall)
  accuracy = (TN + TP) / (TN + FP + FN + TP)
  
  precision_total = c(precision_total, precision)
  recall_total = c(recall_total, recall)
  F1_total = c(F1_total, F1)
  accuracy_total = c(accuracy_total, accuracy)
  
}

results = data.frame(
  month = c("July", "September", "October"),
  precision = precision_total,
  recall = recall_total,
  F1 = F1_total,
  accuracy = accuracy_total
)

results_melt = reshape::melt(results, id = "month")
results_melt$month = factor(results_melt$month, 
                            levels = c("July", "September", "October"))

ggplot(results_melt, aes(x = month, y = value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  labs(title = "PLSDA Statistic Values by Month",
       x = "Month", y = "Value",
       color = "Statistic")

precision_total = as.numeric()
recall_total = as.numeric()
F1_total = as.numeric()
accuracy_total = as.numeric()

for (mnth in c("July", "September", "October")) {
  month = VI1_modelos_PLSDA %>% 
    filter(month == !!mnth) %>% 
    tibble::column_to_rownames("group")
  
  TP = month[1, 1] %>% as.numeric()
  TN = month[2, 2] %>% as.numeric()
  FP = month[2, 1] %>% as.numeric()
  FN = month[1, 2] %>% as.numeric()
  
  precision = TP / (TP + FP)
  recall = TP / (TP + FN)
  F1 = 2 * (precision * recall) / (precision + recall)
  accuracy = (TN + TP) / (TN + FP + FN + TP)
  
  precision_total = c(precision_total, precision)
  recall_total = c(recall_total, recall)
  F1_total = c(F1_total, F1)
  accuracy_total = c(accuracy_total, accuracy)
  
}

results = data.frame(
  month = c("July", "September", "October"),
  precision = precision_total,
  recall = recall_total,
  F1 = F1_total,
  accuracy = accuracy_total
)

results_melt = reshape::melt(results, id = "month")
results_melt$month = factor(results_melt$month, 
                            levels = c("July", "September", "October"))

ggplot(results_melt, aes(x = month, y = value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  labs(title = "PLSDA Statistic Values by Month",
       x = "Month", y = "Value",
       color = "Statistic")

