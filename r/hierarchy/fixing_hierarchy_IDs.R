hierarchy_data_12 = readRDS(file = 'output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_2.rds') %>% 
  mutate(Control = as.factor(is.odd(as.numeric(Pen.x))))

table(hierarchy_data_12$hierarchy.x, hierarchy_data_12$hierarchy.y, hierarchy_data_12$Control)
table(hierarchy_data_12$hierarchy.x, hierarchy_data_12$Pen.x)

# Why there is only one submissive in Pen 5 in hierarchy 1?

hierarchy_data_1 = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

# Not the case for the original data
table(hierarchy_data_1$hierarchy, hierarchy_data_1$Pen)



hierarchy_data_2 = readRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress')) 


if (!all(hierarchy_data_1$ID %in% hierarchy_data_2$ID) & all(hierarchy_data_2$ID %in% hierarchy_data_1$ID)) {
  weird_phase_1 = which(!sort(hierarchy_data_1$ID) %in% hierarchy_data_2$ID)
  weird_phase_1 = sort(hierarchy_data_1$ID)[weird_phase_1]
  weird_phase_1 %in% hierarchy_data_2$ID
  
  weird_phase_2 = which(!sort(hierarchy_data_2$ID) %in% hierarchy_data_1$ID)
  weird_phase_2 = sort(hierarchy_data_2$ID)[weird_phase_2]
  weird_phase_2 %in% hierarchy_data_1$ID
  weird_phase_2 = "F376"
  
  pen5_1 = hierarchy_data_1 %>% filter(Pen == '5')
  pen5_2 = hierarchy_data_2 %>% filter(Pen == '5')
  pen5_3 = hierarchy_data_3 %>% filter(Pen == '5')
  
  pen5_1$ID %>% sort()
  pen5_2$ID %>% sort()
  pen5_3$ID %>% sort
  
  all(pen5_2$ID %in% pen5_1$ID)
  all(pen5_3$ID %in% pen5_1$ID)
  
  # # # #
  hierarchy_data_2$ID = gsub("F376", "F842", hierarchy_data_2$ID)
  # # # #
  
  pen5_2 = hierarchy_data_2 %>% filter(Pen == '5')
  
  all(pen5_2$ID %in% pen5_1$ID)
  
  all(hierarchy_data_1$ID %in% hierarchy_data_2$ID)
  
  saveRDS(file = 'output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds', 
          object = select(hierarchy_data_2, -treatment))
  
}



# fix hierarchy 3 ---------------------------------------------------------



hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(as.numeric(Pen) %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

weird_phase_3 = which(!hierarchy_data_3$ID %in% hierarchy_data_1$ID)

if (length(weird_phase_3) > 0) {
  weird_phase_3 = hierarchy_data_3[weird_phase_3, ]
  
  weird_phase_3$Pen
  
  pen3_1 = hierarchy_data_1 %>% filter(Pen == '3')
  pen3_3 = hierarchy_data_3 %>% filter(Pen == '3')
  
  all(pen3_3$ID %in% pen3_1$ID)
  pen3_3$ID[!pen3_3$ID %in% pen3_1$ID]
  
  pen3_3$ID <- str_trim(pen3_3$ID)
  all(pen3_3$ID %in% pen3_1$ID)
  
  # # # #
  hierarchy_data_3$ID <- str_trim(hierarchy_data_3$ID)
  # # # #
  
  weird_phase_3 = which(!hierarchy_data_3$ID %in% hierarchy_data_1$ID)
  
  weird_phase_3 = hierarchy_data_3[weird_phase_3, ]
  weird_phase_1 = which(!hierarchy_data_1$ID %in% hierarchy_data_3$ID)
  weird_phase_1 = hierarchy_data_1[weird_phase_1, ]
  
  weird_phase_3$ID
  weird_phase_1$ID
  
  # M394 is the missing animal in phase 3
  
  weird_phase_3$ID = "M841"
  
  all(weird_phase_3$ID %in% hierarchy_data_1$ID)
  
  
  # # # #
  hierarchy_data_3$ID[grepl(pattern = "M814", x = hierarchy_data_3$ID)] <- "M841"
  # # # #
  
  all(hierarchy_data_3$ID %in% hierarchy_data_1$ID)
  
  
  saveRDS(file = 'output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds', 
          object = select(hierarchy_data_3, -treatment))
  
}


