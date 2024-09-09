library(tidyverse)
library(dplyr)
library(lmerTest)

hierarchy_data_1 = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

# hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'


hierarchy_data_2 = readRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

hierarchy_data_12 = merge.data.frame(hierarchy_data_1, hierarchy_data_2, 
                                     by = "ID")

hierarchy_data_12$hierarchy_change = paste0(hierarchy_data_12$hierarchy.x, 
                                            '_to_', 
                                            hierarchy_data_12$hierarchy.y)

dir.create('output/changes_in_hierarchy')
saveRDS(
  hierarchy_data_12, 
  file = 'output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_2.rds')


# Change 2 - 3 ------------------------------------------------------------


hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 





hierarchy_data_23 = merge.data.frame(hierarchy_data_2, hierarchy_data_3, 
                                     by = "ID")

hierarchy_data_23$hierarchy_change = paste0(hierarchy_data_23$hierarchy.x, 
                                            '_to_', 
                                            hierarchy_data_23$hierarchy.y)

table(hierarchy_data_23$hierarchy.x, hierarchy_data_23$hierarchy.y)

saveRDS(
  hierarchy_data_23, 
  file = 'output/changes_in_hierarchy/hierarchy_data_changes_from_phase_2_to_3.rds')


# Change 1 - 3 ------------------------------------------------------------


hierarchy_data_13 = merge.data.frame(hierarchy_data_1, hierarchy_data_3, 
                                     by = "ID")

hierarchy_data_13$hierarchy_change = paste0(hierarchy_data_13$hierarchy.x, 
                                            '_to_', 
                                            hierarchy_data_13$hierarchy.y)

table(hierarchy_data_13$hierarchy.x, hierarchy_data_13$hierarchy.y)

saveRDS(
  hierarchy_data_13, 
  file = 'output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_3.rds')

