source('r/hierarchy/descriptiva/weight_vs_hierarchy_v4.R')

hierarchy_pesos = rbind(cbind(hierarchy_pesos_1, phase = 1), 
                        cbind(hierarchy_pesos_2, phase = 2),
                        cbind(hierarchy_pesos_3, phase = 3))

boxplot(hierarchy_pesos$Pe ~ hierarchy_pesos$hierarchy * hierarchy_pesos$FP, las = 2)

# Load ggplot2 package
library(ggplot2)

# Assuming 'hierarchy_pesos' is your data frame
# Assuming 'Pe', 'hierarchy', and 'phase' are columns in your data frame

library(stringr)

# Create the plot
hierarchy_pesos %>% 
  mutate(`Weighing Date` = factor(FP),
         Phase = factor(phase),
         Hierarchy = str_to_title(hierarchy)) %>% 
  rename(`Weight (kg)` = Pe) %>% 
ggplot(aes(x = `Weighing Date`, y = `Weight (kg)`, fill = Hierarchy, color = Phase)) +
  geom_boxplot() +
  # Customizations (optional)
  theme_minimal()  # Change the theme if desired

hierarchy_pesos %>% 
  mutate(`Weighing Date` = factor(FP),
         Phase = factor(phase),
         Hierarchy = str_to_title(hierarchy)) %>% 
  rename(`Weight (kg)` = Pe) %>% 
  ggplot(aes(x = `Weighing Date`, y = `Weight (kg)`, fill = Phase, color = Hierarchy)) +
  geom_boxplot() 
# Customizations (optional)
theme_minimal()  # Change the theme if desired



library(ggplot2)
library(dplyr)
library(stringr)

# Assuming 'hierarchy_pesos' is your data frame

library(ggplot2)
library(dplyr)
library(stringr)

# Assuming 'hierarchy_pesos' is your data frame

hierarchy_pesos %>%
  mutate(`Weighing Date` = factor(FP),
         Phase = factor(phase),
         Hierarchy = str_to_title(hierarchy)) %>%
  rename(`Weight (kg)` = Pe) %>%
  group_by(`Weighing Date`, Hierarchy, Phase) %>%
  summarise(avg_weight = mean(`Weight (kg)`),
            se = sd(`Weight (kg)`)/sqrt(n())) %>%
  ggplot(aes(x = `Weighing Date`, y = avg_weight, fill = Hierarchy, color = Phase)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) + # Adjust width as needed
  geom_errorbar(aes(ymin = avg_weight - se, ymax = avg_weight + se),
                position = position_dodge(width = 0.8), width = 0.2) +  # Error bars
  # Customizations (optional)
  labs(x = "Weighing Date", y = "Average Weight (kg)", fill = "Hierarchy", title = "Side-by-Side Barplots for Each Date and Hierarchy Class") +
  theme_minimal()  # Change the theme if desired


lmerTest::lmer(Pe ~ hierarchy + treatment + edad + (1|ID), 
               data = hierarchy_pesos_3) %>% 
  sjPlot::plot_model(show.values = T ,type = "pred") 