library(h2o)
library(tidyverse)


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
Genus_meta_2 = Genus_meta %>% 
  filter(Samplingday == "2022-07-22")

Genus_meta_2$Subject.ID_Samplingday = paste(Genus_meta_2$Subject.ID, 
                                            Genus_meta_2$Samplingday)

h2o.init()

train = Genus_meta_2 %>% 
  mutate(Tto = as.factor(Tto)) %>% 
  as.h2o()

y <- "Tto"
x <- subset(colnames(train), grepl("g__", x = colnames(train)))

# Run AutoML for 20 base models
aml <- h2o.automl(x = x, y = y,
                  training_frame = train)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)
h2o.varimp(aml@leader)
aml@leader %>% summary()

h2o.saveModel(object = aml@leader, path = 'output/autoML_treatment_v_microbiota/tto_vs_microbiota_july_h2omodel')
