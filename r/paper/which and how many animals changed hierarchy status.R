library(tidyverse)
library(sjPlot)
library(dplyr)
library(stringr)

# Phases 1 - 2 ------------------------------------------------------------


hierarchy_data_12 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_2.rds') %>% 
  mutate(hierarchy_change_value = ifelse(hierarchy.x == hierarchy.y, 
                                         'equal', 
                                         ifelse(hierarchy.x == 'dominant', 
                                                'worse', 
                                                ifelse(hierarchy.x == 'submissive',
                                                       'better',
                                                       ifelse(hierarchy.y == 'dominant',
                                                              'better',
                                                              'worse'))))) %>% 
  filter(hierarchy_change_value != 'equal') %>% 
  mutate(hierarchy_change_value = factor(hierarchy_change_value)) %>% 
  filter(ID != "M144", 
         ID != "M394")


# Phases 2 - 3 ------------------------------------------------------------



hierarchy_data_12 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_2.rds')

hierarchy_data_23 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_2_to_3.rds') %>% 
  mutate(hierarchy_change_value = ifelse(hierarchy.x == hierarchy.y, 
                                         'equal', 
                                         ifelse(hierarchy.x == 'dominant', 
                                                'worse', 
                                                ifelse(hierarchy.x == 'submissive',
                                                       'better',
                                                       ifelse(hierarchy.y == 'dominant',
                                                              'better',
                                                              'worse'))))) %>% 
  filter(hierarchy_change_value != 'equal') %>% 
  mutate(hierarchy_change_value = factor(hierarchy_change_value)) %>% 
  filter(ID != "M144", 
         ID != "M394") %>% 
  mutate(hierarchy_change_value = str_to_title(hierarchy_change_value)) %>% 
  merge.data.frame(hierarchy_data_12, ., "ID")




# Phases 1 - 3 ------------------------------------------------------------


hierarchy_data_12 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_2.rds')

hierarchy_data_13 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_3.rds') %>% 
  mutate(hierarchy_change_value = ifelse(hierarchy.x == hierarchy.y, 
                                         'equal', 
                                         ifelse(hierarchy.x == 'dominant', 
                                                'worse', 
                                                ifelse(hierarchy.x == 'submissive',
                                                       'better',
                                                       ifelse(hierarchy.y == 'dominant',
                                                              'better',
                                                              'worse'))))) %>% 
  filter(hierarchy_change_value != 'equal') %>% 
  mutate(hierarchy_change_value = factor(hierarchy_change_value)) %>% 
  filter(ID != "M144", 
         ID != "M394") %>% 
  mutate(hierarchy_change_value = str_to_title(hierarchy_change_value)) %>% 
  merge.data.frame(hierarchy_data_12, ., "ID")

table(hierarchy_data_13$treatment.x.x, hierarchy_data_13$hierarchy_change.y)
table(hierarchy_data_13$hierarchy_change_value)
table(hierarchy_data_13$treatment.x.x, hierarchy_data_13$hierarchy_change_value)
