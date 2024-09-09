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

all_time_model = lmer(Weight ~ Acetitomaculum*Age + Anaerobutyricum*Age + 
       Coprococcus*Age + Treponema*Age + Floccifex*Age + Treatment + sexo + (1|Subject.ID.x) , data = genus_peso) 

all_time_model = lmer(Weight ~ Acetitomaculum + Anaerobutyricum + 
                        Coprococcus + Treponema + Floccifex + Treatment + sexo + (1|Subject.ID.x) + (1|FP_apprx) , data = genus_peso) 

# Models with biased data based on treatment ------------------------------


genus_peso %>% 
  filter(Treatment == 0) %>% 
  lmer(Acetitomaculum ~ as.factor(FP_apprx) * sexo + (1|Subject.ID.x) , data = .) %>% 
  summary

# genus_peso %>% 
#   filter(Treatment == 0) %>% 
#   lmer(Anaerobutyricum ~ as.factor(FP_apprx) * sexo + (1|Subject.ID.x) , data = .) %>% 
#   summary

genus_peso %>% 
  filter(Treatment == 0) %>% 
  lmer(Coprococcus ~ as.factor(FP_apprx) * sexo + (1|Subject.ID.x) , data = .) %>% 
  summary

genus_peso %>% 
  filter(Treatment == 1) %>% 
  lmer(Treponema ~ as.factor(FP_apprx) * sexo + (1|Subject.ID.x) , data = .) %>% 
  summary

genus_peso %>% 
  filter(Treatment == 0) %>% 
  lmer(Acetitomaculum ~ Age * sexo + (1|Subject.ID.x) , data = .) %>% 
  summary

# Assume you have a separate dataframe called "other_data" with additional variables
# that you want to include as fixed effects in the model.

time_model <- genus_peso %>% 
  select(where(is.numeric), Subject.ID.x) %>% 
  lmer(Weight ~ . - Subject.ID.x - day_of_life - studyid - library.Size - Corral - input + 
         (1 | Subject.ID.x) + (1 | Corral), data = .)

# Add terms from the "other_data" dataframe to the model formula
updated_formula <- update(formula(time_model), . ~ . + Variable1 + Variable2 + ...)

# Fit the updated model with the added terms
updated_model <- lmer(updated_formula, data = genus_peso)

time_model = genus_peso %>% 
  select(where(is.numeric), Treatment, sexo, Subject.ID.x) %>% lmer(Weight ~ . + Treatment + Age + 
                        sexo + (1|Subject.ID.x) , data = genus_peso) 


# Plots All Time ----------------------------------------------------------


all_time_model %>% summary()
all_time_model %>% sjPlot::plot_model(show.values = T, title = "Weight (Pes)")
all_time_model %>% sjPlot::plot_model(type = "pred")

library(ggplot2)

# Scatter plot with color
  ggplot(genus_peso, aes(x = Anaerobutyricum, y = Weight, color = as.factor(FP_apprx))) +
    geom_point() +
    labs(x = "Anaerobutyricum", y = "Weight", color = "Date") +
    scale_color_manual(values = c("darkgreen", "orange", "blue")) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black"),
          panel.grid.minor = element_line(color = "gray"))
  
 
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
  means_genus %>% 
    filter(Genus %in% c("PeH17", "Acetitomaculum", "Anaerobutyricum", 
                        "Floccifex", "Treponema", "Coprococcus")) %>% 
  ggplot(., aes(x = FP_apprx, y = Mean, color = Genus, linetype = Treatment)) +
    geom_line() +
    scale_linetype_manual(values = c("solid", "22")) +
    # scale_color_manual(values = my_colors) +
    labs(x = "Date", y = "Mean Abundance") +
    theme(panel.background = element_rect(fill = "white"))
  

  a = genus_peso %>% select(FP_apprx, Treatment, Coprococcus)
boxplot(a$Coprococcus ~ a$FP_apprx*a$Treatment)

# September ---------------------------------------------------------------

    
genus_peso_sept = genus_peso %>% 
  filter(FP_apprx == "2022-09-05" )

lmer(Pe_apprx ~ g__Acetitomaculum + g__Anaerobutyricum + 
       g__Coprococcus + Treponema + Treatment +  
       (1|sexo), data = genus_peso_sept) %>% 
  summary()
  sjPlot::plot_model(show.values = T, title = "Weight (Pes) in September")

