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
  mutate(across(contains(c("g__")), as.numeric)) %>%
  rename_with(~gsub("g__", "", .x)) %>%
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

Metaboloite197_ALR_scale_paired <- 
  read.delim("~/gutbrain/data/Metaboloite197_ALR_scale_paired.txt") %>% 
  select(Id, C10H12N2O)

genus_metab = Genus_meta %>% 
  merge.data.frame(y = Metaboloite197_ALR_scale_paired, by.x = "Genus", 
                   by.y = "Id")

lmer(C10H12N2O ~ Acetitomaculum + Anaerobutyricum + 
       Coprococcus + Treponema + Floccifex + Group  + 
       (1|sexo), data = genus_metab) %>% 
  sjPlot::plot_model(show.values = T)

a = genus_metab %>% select(is.numeric) %>% as.matrix %>% cor() 
a[82,] %>% sort %>% as.matrix() %>%  corrplot::corrplot()
