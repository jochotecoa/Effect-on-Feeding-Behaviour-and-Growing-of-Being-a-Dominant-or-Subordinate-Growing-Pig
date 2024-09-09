library(dplyr)

Genus_candidatos <- read.delim2("~/gutbrain/data/Genus_candidatos.txt")
View(Genus_candidatos)
metadata_mod_long <- read.delim("~/gutbrain/data/metadata_mod_long.tsv")
View(metadata_mod_long)

Genus_meta = Genus_candidatos %>% 
  merge.data.frame(y = metadata_mod_long, by.x = "Sample", by.y = "SampleID") 

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
  merge.data.frame(y = Metaboloite197_ALR_scale_paired, by.x = "Sample", 
                   by.y = "Id")

lmer(C10H12N2O ~ g__Acetitomaculum + g__Anaerobutyricum + 
       g__Coprococcus + g__Treponema + Group  + 
       (1|sexo), data = genus_metab) %>% 
  sjPlot::plot_model(show.values = T)

