library(h2o)
library(tidyverse)

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(Pen = as.factor(Pen))
hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'


Genus_GG2_long_CCS <- read.delim2("~/gutbrain/data/Genus_GG2_long_CCS.txt") %>% 
  t() %>% 
  as.data.frame()

samples = Genus_GG2_long_CCS %>% rownames()
Genus_GG2_long_CCS$Genus = samples

colname = Genus_GG2_long_CCS[1, ]
colnames(Genus_GG2_long_CCS) = colname
Genus_GG2_long_CCS = Genus_GG2_long_CCS[-1, ] %>% 
  mutate(across(contains(c("g__")), as.numeric)) %>% 
  mutate(Genus = gsub("X", "", Genus))

metadata_mod_long <- read.delim("~/gutbrain/data/metadata_mod_long.tsv")

Genus_meta = Genus_GG2_long_CCS %>% 
  merge.data.frame(y = metadata_mod_long, by.x = "Genus", by.y = "SampleID") 

Genus_meta$Samplingday <- gsub("07_2022", "2022-07-22", Genus_meta$Samplingday)
Genus_meta$Samplingday <- gsub("09_2022", "2022-09-05", Genus_meta$Samplingday)
Genus_meta$Samplingday <- gsub("10_2022", "2022-10-05", Genus_meta$Samplingday)
Genus_meta$Samplingday = Genus_meta$Samplingday %>% as.Date()
Genus_meta_1 = Genus_meta %>% 
  filter(Samplingday == "2022-10-05")

Genus_meta$Subject.ID_Samplingday = paste(Genus_meta$Subject.ID, 
                                          Genus_meta$Samplingday)

hierarchy_genus = merge.data.frame(hierarchy_data, Genus_meta, 
                                   by.x = "ID", by.y = "Subject.ID")

h2o.init()

train = hierarchy_genus %>% 
  mutate(hierarchy = as.factor(hierarchy)) %>% 
  filter(hierarchy != "intermediate") %>%
  as.h2o()
y <- "hierarchy"
x <- subset(colnames(train), grepl("g__", x = colnames(train)))

# Run AutoML for 20 base models
aml <- h2o.automl(x = x, y = y,
                  training_frame = train, max_runtime_secs = 300)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)
h2o.varimp_plot(aml@leader, num_of_features = 15)
aml@leader %>% summary()

aml@leader %>% h2o.saveModel(path = "output/autoML_hierarchy_v_microbiota/october/")
