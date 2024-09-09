library(tidyverse)
library(dplyr)
library(lmerTest)

Genus_GG2_long_CCS <- read.delim2("~/gutbrain/data/Genus_GG2_long_CCS.txt") %>% 
  t() %>% 
  as.data.frame()

samples = Genus_GG2_long_CCS %>% rownames()
Genus_GG2_long_CCS$Genus = samples

colname = Genus_GG2_long_CCS[1, ]
colnames(Genus_GG2_long_CCS) = colname
Genus_GG2_long_CCS = Genus_GG2_long_CCS[-1, ]

Genus_GG2_long_CCS = Genus_GG2_long_CCS %>% 
  # select(Genus, g__Acetitomaculum, g__Anaerobutyricum, g__Coprococcus, 
  #        g__Treponema, g__Floccifex) %>% 
  mutate(across(contains(c("g__")), as.numeric)) %>% 
  mutate(Genus = gsub("X", "", Genus))

metadata_mod_long <- read.delim("~/gutbrain/data/metadata_mod_long.tsv")

Genus_meta = Genus_GG2_long_CCS %>% 
  merge.data.frame(y = metadata_mod_long, by.x = "Genus", by.y = "SampleID") 

Genus_meta$Samplingday <- gsub("07_2022", "2022-07-22", Genus_meta$Samplingday)
Genus_meta$Samplingday <- gsub("09_2022", "2022-09-05", Genus_meta$Samplingday)
Genus_meta$Samplingday <- gsub("10_2022", "2022-10-05", Genus_meta$Samplingday)
Genus_meta$Samplingday = Genus_meta$Samplingday %>% as.Date()

Genus_meta$Subject.ID_Samplingday = paste(Genus_meta$Subject.ID, 
                                          Genus_meta$Samplingday)

# Phase 1 -----------------------------------------------------------------


hierarchy_data_1 = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'


genus_hierarchy = merge.data.frame(x = Genus_meta, y = hierarchy_data_1, 
                              by.x = "Subject.ID", 
                              by.y = "ID") %>% 
  filter(Samplingday == "2022-07-22")
colnames(genus_hierarchy) = make.names(colnames(genus_hierarchy))

naToOne <- function(x) {
  x[is.na(x)] = 1
  return(x)
}

microbes = colnames(genus_hierarchy) %>% subset(., grepl("g__", .))
for (microb in microbes) {
  form = as.formula(paste(microb, "~ hierarchy"))
  a = lm(form, genus_hierarchy) %>% summary
  if (any(naToOne(a$coefficients[-1, 4]) < 0.05)) {
    print(microb)
  }
  
}


# Phase 3 -----------------------------------------------------------------


hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

# hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'


genus_hierarchy = merge.data.frame(x = Genus_meta, y = hierarchy_data_3, 
                                   by.x = "Subject.ID", 
                                   by.y = "ID") %>% 
  filter(Samplingday == "2022-10-05")
colnames(genus_hierarchy) = make.names(colnames(genus_hierarchy))

microbes = colnames(genus_hierarchy) %>% subset(., grepl("g__", .))
for (microb in microbes) {
  form = as.formula(paste(microb, "~ hierarchy"))
  a = lm(form, genus_hierarchy) %>% summary
  if (any(a$coefficients[-1, 4] < 0.05)) {
    print(microb)
  }
  
}



