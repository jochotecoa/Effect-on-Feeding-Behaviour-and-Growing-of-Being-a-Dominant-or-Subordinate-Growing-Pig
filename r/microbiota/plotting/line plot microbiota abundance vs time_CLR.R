library(dplyr)
library(lmerTest)

Genus_GG2_long_CLR <- read.delim2("~/gutbrain/data/Genus_GG2_long_CLR.txt") %>% 
  as.data.frame()

metadata_mod_long <- read.delim("~/gutbrain/data/metadata_mod_long.tsv")

Genus_meta = Genus_GG2_long_CLR %>% 
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


# lm(Pe_apprx ~ g__g__Acetitomaculum + g__g__Anaerobutyricum + g__g__Coprococcus + 
#      g__Treponema, data = genus_peso) %>% summary()


genus_peso =  genus_peso %>% 
  rename_with( ~ gsub("g__", "", .x)) %>% 
  rename(Treatment = Group,
         Age = edad) 

genus_peso = genus_peso %>% 
  rename(Weight = Pe_apprx)

library(ggplot2)

means_genus = genus_peso %>% 
  reshape2::melt(id = c("FP_apprx", "Treatment")) %>%
  rename(Genus = variable) %>% 
  mutate(Treatment = as.character(Treatment)) %>% 
  mutate(Treatment = ifelse(Treatment == '0', "Control", "Stress")) %>% 
  filter(!(Genus %in% c("Subject.ID_Samplingday", "Genus", "day_of_life", "Subject.ID.x", "studyid", "Subject_ID_new", 
                        "library.Size", "Corral", "Tto", "Samplingday", "LevelChoco", "sexo", "input", "Ta", "Weight", 
                        "Fe_ncmnt", "Age", "Subject.ID.y")) ) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(Genus, FP_apprx, Treatment) %>% 
  summarise(Mean = mean(value, na.rm = T))

# my_colors <- c("#1dae46", "#FF6600", "#1f96d5")
dir.create("output/microbiota/plotting/line plot microbiota abundance vs time_CLR/", recursive = T)

png(filename = "output/microbiota/plotting/line plot microbiota abundance vs time_CLR/plot_control.png",
    width = 1080*8, height = 720*8, res = 300*4)
means_genus %>% 
  filter(Genus %in% c('Coprococcus',
                      'DSXL01',
                      'SFDB01',
                      'HUN007')) %>% 
  ggplot(., aes(x = FP_apprx, y = Mean, color = Genus, linetype = Treatment)) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "22")) +
  # scale_color_manual(values = my_colors) +
  labs(x = "Date", y = "Mean Abundance") +
  theme(panel.background = element_rect(fill = "white"))
dev.off()

png(filename = "output/microbiota/plotting/line plot microbiota abundance vs time_CLR/plot_stress_1.png",
    width = 1080*8, height = 720*8, res = 300*4)

means_genus %>% 
  filter(Genus %in% c('Collinsella',
                      'Holdemanella' ,      
                      'Marvinbryantia',
                      'ER4')) %>% 
  ggplot(., aes(x = FP_apprx, y = Mean, color = Genus, linetype = Treatment)) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "22")) +
  # scale_color_manual(values = my_colors) +
  labs(x = "Date", y = "Mean Abundance") +
  theme(panel.background = element_rect(fill = "white"))
dev.off()

png(filename = "output/microbiota/plotting/line plot microbiota abundance vs time_CLR/plot_stress_2.png",
    width = 1080*8, height = 720*8, res = 300*4)

means_genus %>% 
  filter(Genus %in% c('Treponema',
                      'UBA636',
                      'Anaerobutyricum',
                      'Enterenecus',
                      'Peptococcus')) %>% 
  ggplot(., aes(x = FP_apprx, y = Mean, color = Genus, linetype = Treatment)) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "22")) +
  # scale_color_manual(values = my_colors) +
  labs(x = "Date", y = "Mean Abundance") +
  theme(panel.background = element_rect(fill = "white"))
dev.off()
