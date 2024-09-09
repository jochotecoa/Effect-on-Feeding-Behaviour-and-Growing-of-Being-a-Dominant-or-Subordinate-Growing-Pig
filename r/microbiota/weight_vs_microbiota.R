library(dplyr)
library(lmerTest)

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


Pesos <- readRDS("output/load_data/pesos_approx.rds") %>% 
  mutate(FP_apprx = as.Date(FP_apprx)) %>% 
  mutate(Subject.ID = gsub("@.*", "", Ta)) %>% 
  mutate(Subject.ID_FP_apprx = paste(Subject.ID, FP_apprx))

genus_peso = merge.data.frame(x = Genus_meta, y = Pesos, 
                              by.x = "Subject.ID_Samplingday", 
                              by.y = "Subject.ID_FP_apprx")


lm(Pe_apprx ~ g__Acetitomaculum + g__Anaerobutyricum + g__Coprococcus + 
     g__Treponema, data = genus_peso) %>% summary()


genus_peso =  genus_peso %>% 
  rename(Acetitomaculum = g__Acetitomaculum,
         Anaerobutyricum = g__Anaerobutyricum,
         Coprococcus = g__Coprococcus, 
         Treponema = g__Treponema,
         Treatment = Group,
         Age = edad
         ) 

lmer(Pe_apprx ~ Acetitomaculum + Anaerobutyricum + 
       Coprococcus + Treponema + Treatment + Age + (1|Subject.ID.x) + 
       (1|sexo), data = genus_peso) %>% summary()
  sjPlot::plot_model(show.values = T, title = "Weight (Pes)")

library(ggplot2)

# Scatter plot with color
  ggplot(genus_peso, aes(x = Anaerobutyricum, y = Pe_apprx, color = as.factor(FP_apprx))) +
    geom_point() +
    labs(x = "Anaerobutyricum", y = "Weight", color = "Date") +
    scale_color_manual(values = c("darkgreen", "orange", "blue")) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black"),
          panel.grid.minor = element_line(color = "gray"))
  


genus_peso_sept = genus_peso %>% 
  filter(FP_apprx == "2022-09-05" )

lmer(Pe_apprx ~ Acetitomaculum + Anaerobutyricum + 
       Coprococcus + Treponema + Treatment +  
       (1|sexo), data = genus_peso_sept) %>% 
  summary()
  sjPlot::plot_model(show.values = T, title = "Weight (Pes) in September")

