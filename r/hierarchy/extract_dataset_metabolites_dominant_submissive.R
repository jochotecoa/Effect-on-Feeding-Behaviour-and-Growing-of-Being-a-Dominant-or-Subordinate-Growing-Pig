library(tidyverse)

hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) %>% 
  filter(hierarchy != "intermediate")

Input_DAMetabolitesALR_scale <- read.delim2("~/gutbrain/data/metabolites/Input_DAMetabolitesALR_scale.txt")
GUTBRAIN_control_full <- read_excel("data/diversity_microbiota/GUTBRAIN_control_full.xlsx", 
                                    sheet = "longitudinal_complete") %>% 
  mutate(Sampling_day = as.Date(Sampling_day)) %>%
  filter(Sampling_day == as.Date("2022-10-05"))

table(Input_DAMetabolitesALR_scale$Id %in% GUTBRAIN_control_full$ID_Torre)
table(hierarchy_data_3$ID %in% GUTBRAIN_control_full$ID)
hierarchy_data_3$ID[!hierarchy_data_3$ID %in% GUTBRAIN_control_full$ID]

library(stringr)

# Remove leading and trailing spaces in the ID column
hierarchy_data_3$ID <- str_trim(hierarchy_data_3$ID)

# Check for values that are not in GUTBRAIN_control_full$ID
hierarchy_data_3$ID[!hierarchy_data_3$ID %in% GUTBRAIN_control_full$ID]

DAMetabolitesALR_hierarchy = merge.data.frame(Input_DAMetabolitesALR_scale, 
                                          GUTBRAIN_control_full, 
                                          by.x = "Id", 
                                          by.y = "ID_Torre") %>% 
  merge.data.frame(hierarchy_data_3, "ID")

dir.create("output/hierarchy/extract_dataset_metabolites_dominant_submissive", 
           recursive = T)

saveRDS(object = DAMetabolitesALR_hierarchy, 
        file = "output/hierarchy/extract_dataset_metabolites_dominant_submissive/Input_DAMetabolitesALR_scale_dominance_phase_3_by_pen.rds")
