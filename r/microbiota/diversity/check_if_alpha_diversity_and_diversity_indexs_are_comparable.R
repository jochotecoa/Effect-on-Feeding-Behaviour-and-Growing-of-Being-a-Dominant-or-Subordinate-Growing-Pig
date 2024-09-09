alpha_diversity <- read.csv("~/gutbrain/data/diversity_microbiota/alpha_diversity.csv")
library(readxl)
Diversity_indexs_juan <- read_excel("data/diversity_microbiota/Diversity_indexs_juan.xlsx")

alpha_diversity = alpha_diversity %>% 
  arrange(X)
Diversity_indexs_juan = Diversity_indexs_juan %>% 
  arrange(Id)

table(alpha_diversity$X %in% Diversity_indexs_juan$Id)
table(Diversity_indexs_juan$Id %in% alpha_diversity$X)
table(alpha_diversity$X == Diversity_indexs_juan$Id)

cor.test(alpha_diversity$Shannon, Diversity_indexs_juan$shannon)

