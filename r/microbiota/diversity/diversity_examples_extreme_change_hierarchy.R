

library(readxl)
Diversity_indexs_juan <- read_excel("data/diversity_microbiota/Diversity_indexs_juan.xlsx")

GUTBRAIN_control_full <- 
  read_excel("data/diversity_microbiota/GUTBRAIN_control_full.xlsx", 
             sheet = "metadata_longuitudinal")

examples_id = GUTBRAIN_control_full[GUTBRAIN_control_full$ID %in% c("F840", "M917"), 
                                    c("ID_Torre", "Sampling_day", "ID")]

Diversity_indexs_examples = Diversity_indexs_juan %>% 
  merge.data.frame(examples_id, by.x = "Id", by.y = "ID_Torre")


ggplot(data = Diversity_indexs_examples, aes(x = Sampling_day, y = shannon, fill = ID)) +
  geom_line()

