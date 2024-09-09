library(tidyverse)

# Phase 1 - 2 -------------------------------------------------------------

# hierarchy class versus binary hierarchy change

hierarchy_data_12 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_2.rds')

# observed <- matrix(c(3+0+2+1, 5+5, 
#                      3+5+2+3, 8+11,
#                      0+5+1+3, 3+4
# ), nrow = 3, byrow = TRUE)
# colnames(observed) <- c("Changed", "Not Changed")
# rownames(observed) <- c("Dominant", "Intermediate", "Submissive")
# observed
# # Perform the chi-square test
# result <- fisher.test(observed)
# 
# # Print the result
# print(result)
# 

# Treatment group versus Binary Hierarchical Changes

# Create variables for the observed frequencies
# observed <- matrix(c(16, 16, 12, 20), nrow = 2, byrow = TRUE)
# colnames(observed) <- c("Changed", "Not Changed")
# rownames(observed) <- c("Control", "Treatment")
# observed
# # Perform the chi-square test
# result <- fisher.test(observed)
# 
# # Print the result
# print(result)
# 
# # Combination of treatment and hierarchical class versus hierarchical change
# 
# table(hierarchy_data_12$treatment.x, hierarchy_data_12$hierarchy_change)
# 
# observed <- matrix(c(3+0, 5, 
#                      2+1, 5,
#                      3+5, 8,
#                      2+3, 11,
#                      0+5, 3,
#                      1+3, 4
# ), nrow = 6, byrow = TRUE)
# colnames(observed) <- c("Changed", "Not Changed")
# # Create vectors for the levels of each factor
# group_levels <- c("Control", "Treatment")
# status_levels <- c("Dominant", "Intermediate", "Submissive")
# 
# # Generate all combinations
# combinations <- expand.grid(Group = group_levels, Status = status_levels)
# 
# # Create a vector of combined strings
# combinations <- apply(combinations, 1, function(row) {
#   paste(row, collapse = "-")
# })
# 
# # View the result
# print(combinations)
# 
# rownames(observed) <- combinations
# observed
# 
# # Perform the chi-square test
# result <- fisher.test(observed)
# 
# # Print the result
# print(result)

# All hierarchical changes versus treatment
table(hierarchy_data_12$treatment.x, hierarchy_data_12$hierarchy_change) %>%
  t %>% as.matrix %>% fisher.test()


# # All hierarchical changes versus treatment
# all_possibs = table(hierarchy_data_12$treatment.x, hierarchy_data_12$hierarchy_change) %>%
#   t %>% as.matrix 
# total_per_group = table(hierarchy_data_12$hierarchy.x, hierarchy_data_12$treatment.x) %>% 
#   as.matrix()
# for (hierarchy in c("dominant", "intermediate", "submissive")) {
#   for (treatment in c("Control", "Stress")) {
#     all_possibs[grep(paste0("^", hierarchy), rownames(all_possibs)), treatment] = 
#       all_possibs[grep(paste0("^", hierarchy), rownames(all_possibs)), treatment] / total_per_group[hierarchy, treatment] * 100
#   }
# }
# 
# fisher.test(all_possibs[c(1,5,9),])
# 
# fisher.test(all_possibs)
# # Calculate standardized residuals
# res <- fisher.test(all_possibs)$stdres
# 
# res[,2] %>% barplot(las = 2)
# 
# pairwise.prop.test(all_possibs)

# Phase 2 - 3 -------------------------------------------------------------

# hierarchy class versus binary hierarchy change

hierarchy_data_23 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_2_to_3.rds')
observed <- matrix(c(4+0+2+1, 4+5,
                     4+5+3+6, 7+6,
                     5+7, 3+1
                     ), nrow = 3, byrow = TRUE)
colnames(observed) <- c("Changed", "Not Changed")
rownames(observed) <- c("Dominant", "Intermediate", "Submissive")
observed
# Perform the chi-square test
result <- fisher.test(observed)

# Print the result
print(result)

# Treatment group versus Binary Hierarchical Changes
animals_missing = which(!hierarchy_data_12$ID %in% hierarchy_data_23$ID)
hierarchy_data_12[animals_missing, ]

# Create variables for the observed frequencies
observed <- matrix(c(17, 15, 19, 12), nrow = 2, byrow = TRUE) # 32 Control, 31 Stressed
colnames(observed) <- c("Changed", "Not Changed")
rownames(observed) <- c("Control", "Treatment")
observed
# Perform the chi-square test
result <- fisher.test(observed)

# Print the result
print(result)


# Combination of treatment and hierarchical class versus hierarchical change
hierarchy_data_23_2 = hierarchy_data_23 %>% 
  merge.data.frame(hierarchy_data_12, ., "ID")

table(hierarchy_data_23_2$treatment.x.x, hierarchy_data_23_2$hierarchy_change.y)

observed <- matrix(c(4+0, 4, 
                     2+1, 5,
                     4+5, 7,
                     3+6, 6,
                     5, 3,
                     7, 1
), nrow = 6, byrow = TRUE)
colnames(observed) <- c("Changed", "Not Changed")

rownames(observed) <- combinations
observed

# Perform the chi-square test
result <- fisher.test(observed)

# Print the result
print(result)

# All hierarchical changes versus treatment

table(hierarchy_data_23_2$treatment.x.x, hierarchy_data_23_2$hierarchy_change.y) %>%
  t %>% as.matrix %>% fisher.test()

# all_possibs = table(hierarchy_data_23_2$treatment.x.x, hierarchy_data_23_2$hierarchy_change.y) %>%
#   t %>% as.matrix 
# total_per_group = table(hierarchy_data_23_2$hierarchy.y.x, hierarchy_data_23_2$treatment.x.x) %>% 
#   as.matrix()
# for (hierarchy in c("dominant", "intermediate", "submissive")) {
#   for (treatment in c("Control", "Stress")) {
#     all_possibs[grep(paste0("^", hierarchy), rownames(all_possibs)), treatment] = all_possibs[grep(paste0("^", hierarchy), rownames(all_possibs)), treatment] / total_per_group[hierarchy, treatment] * 100
#   }
# }
# 
# fisher.test(all_possibs[c(1, 5, 8), ])
# # Calculate standardized residuals
# res <- fisher.test(all_possibs[c(1, 5, 8), ])$stdres
# 
# res[,2] %>% barplot(las = 2)
# 
# pairwise.prop.test(all_possibs[c(1, 5, 8), ])
# 
# fisher.test(all_possibs)
# # Calculate standardized residuals
# res <- fisher.test(all_possibs)$stdres
# 
# res[,2] %>% barplot(las = 2)
# 
# pairwise.prop.test(all_possibs)

# Phase 1 - 3 -------------------------------------------------------------

# # hierarchy class versus binary hierarchy change
# hierarchy_data_13 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_3.rds')
# 
# observed <- matrix(c(4+0+3+1, 4+4, 
#                      4+3+3+4, 9+8,
#                      0+3+1+4, 5+3
#                      ), nrow = 3, byrow = TRUE)
# colnames(observed) <- c("Changed", "Not Changed")
# rownames(observed) <- c("Dominant", "Intermediate", "Submissive")
# observed
# # Perform the chi-square test
# result <- fisher.test(observed)
# 
# # Print the result
# print(result)
# 
# # Treatment group versus Binary Hierarchical Changes
# 
# animals_missing = which(!hierarchy_data_12$ID %in% hierarchy_data_13$ID)
# hierarchy_data_12[animals_missing, ]
# 
# # Create variables for the observed frequencies
# observed <- matrix(c(13, 19, 
#                      16, 16), nrow = 2, byrow = TRUE) # 32 Control, 31 Stressed
# colnames(observed) <- c("Changed", "Not Changed")
# rownames(observed) <- c("Control", 
#                         "Treatment")
# observed
# # Perform the chi-square test
# result <- fisher.test(observed)
# 
# # Print the result
# print(result)
# 
# 
# 
# 
# # Combination of treatment and hierarchical class versus hierarchical change
# 
# 
# hierarchy_data_13_2 = hierarchy_data_13 %>% 
#   merge.data.frame(hierarchy_data_12, ., "ID")
# 
# table(hierarchy_data_13_2$treatment.x.x, hierarchy_data_13_2$hierarchy_change.y)
# 
# observed <- matrix(c(4+0, 4, 
#                      3+1, 4,
#                      4+3, 9,
#                      3+4, 8,
#                      0+3, 5,
#                      1+4, 3
# ), nrow = 6, byrow = TRUE)
# colnames(observed) <- c("Changed", "Not Changed")
# rownames(observed) <- combinations
# observed
# 
# # Perform the chi-square test
# result <- fisher.test(observed)
# 
# # Print the result
# print(result)

# All hierarchical changes versus treatment
table(hierarchy_data_13_2$treatment.x.x, hierarchy_data_13_2$hierarchy_change.y) %>%
  t %>% as.matrix %>% fisher.test()



hierarchy_123 = merge.data.frame(hierarchy_data_12, hierarchy_data_23, 'ID')

table(hierarchy_123$treatment.x.x, paste(hierarchy_123$hierarchy_change.x, hierarchy_123$hierarchy_change.y)) %>%
  t %>% as.matrix %>% fisher.test()

table(hierarchy_123$treatment.x.x, paste(hierarchy_123$hierarchy_change.x, hierarchy_123$hierarchy_change.y)) %>%
  t %>% as.matrix %>% .[c(1, 10, 18), ] %>%  fisher.test()

all_possibs = table(paste(hierarchy_123$hierarchy_change.x, hierarchy_123$hierarchy_change.y)) %>%
  as.matrix %>% as.data.frame()

all_possibs$random_prob = NA
for (rwnm in rownames(all_possibs)) {
  original_n = ifelse(grepl('^intermediate', rwnm), 
                      32, 
                      16)
  to_dom = grep('_to_dominant', strsplit(rwnm, ' ') %>% unlist)
  to_int = grep('_to_intermediate', strsplit(rwnm, ' ') %>% unlist)
  to_sub = grep('_to_submissive', strsplit(rwnm, ' ') %>% unlist)
  
  prob_n = original_n
  
  for (variable in to_dom) {
    prob_n = prob_n * 1/4
  }
  for (variable in to_int) {
    prob_n = prob_n * 1/2
  }
  for (variable in to_sub) {
    prob_n = prob_n * 1/4
  }
  all_possibs[rwnm, "random_prob"] = prob_n
}

# Perform the chi-square test
fisher.test(all_possibs)
all_possibs[c(1, 10, 18),] %>% fisher.test()

# 
# table(hierarchy_123$hierarchy_change.x, hierarchy_123$hierarchy_change.y)
# # 6 (6), 8, 2
# # 3, 3, 2
# # 3. 5, 0
# 
# observed <- matrix(c(3, 8-3, 
#                      3, 16-3,
#                      2, 8-2,
#                      3, 8-3,
#                      5, 16-5,
#                      0, 8-0
# ), nrow = 6, byrow = TRUE)
# colnames(observed) <- c("Not Changed", "Changed")
# rownames(observed) <- combinations %>% sort
# observed

# Perform the chi-square test
result <- fisher.test(observed)

# Print the result
print(result)

all_possibs = data.frame(Observed = c(3, 3, 2, 3, 5, 0), Random = c(1/2, 4, 1/2, 1/2, 4, 1/2))

rownames(all_possibs) <- c("Dominant", "Intermediate", "Submissive")




fisher.test(all_possibs)

all_possibs_2 = data.frame()
all_possibs_2 = rbind(all_possibs_2, all_possibs[1, ] + all_possibs[4, ])
all_possibs_2 = rbind(all_possibs_2, all_possibs[2, ] + all_possibs[5, ])
all_possibs_2 = rbind(all_possibs_2, all_possibs[3, ] + all_possibs[6, ])

all_possibs_2$Observed = c(6, 8, 2)

all_possibs_2 %>% fisher.test()
