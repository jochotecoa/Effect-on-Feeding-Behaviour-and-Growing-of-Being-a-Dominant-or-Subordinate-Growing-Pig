library(tidyverse)

limpiar_nombres <- function(x) {
  x[1, 2:ncol(x)] = 
    x[1, 1:ncol(x) - 1]
  
  x = x[, -1]
  colnames(x) = x[1, ]
  x = x[-1, ]
  return(x)
}

weight_data = readRDS('output/load_data/pesos_approx.rds') %>% 
  mutate(ID = gsub("@.*", "", Ta)) 

ages = weight_data %>% 
  ungroup() %>% 
  mutate(ID_FP = paste(ID, FP_apprx, '_')) %>% 
  select(ID_FP, edad) %>% 
  unique()

Pesos_Juan <- read.delim("Pesos_Juan.txt", header=FALSE) %>% 
  limpiar_nombres() %>% 
  mutate(FP = as.Date(FP, "%Y-%m-%d"),
         ID = gsub("@.*", "", `T`),
         Pe = as.numeric(Pe)) %>% 
  mutate(ID_FP = paste(ID, FP, '_')) %>% 
  merge.data.frame(ages, "ID_FP")


# pesos_juan_1 ------------------------------------------------------------

Pesos_Juan_1 = Pesos_Juan %>% 
  filter(FP < "2022-07-25") 

hierarchy_data_1 = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'

hierarchy_pesos_1 = merge.data.frame(x = hierarchy_data_1, y = Pesos_Juan_1, 
                                      by = "ID")

boxplot(hierarchy_pesos_1$Pe ~ hierarchy_pesos_1$hierarchy)


if (any(table(hierarchy_pesos_1$ID) > 1)) {
  lmerTest::lmer(Pe ~ hierarchy + treatment + edad + (1|ID), 
                 data = hierarchy_pesos_1) %>% 
    sjPlot::plot_model(show.values = T) %>% plot()
  print("lmer")
} else {
  lm(Pe ~ hierarchy + treatment + edad, 
     data = hierarchy_pesos_1) %>% 
    sjPlot::plot_model(show.values = T) %>% 
    plot()
  print("lm")
}



# pesos_juan_2 ------------------------------------------------------------

Pesos_Juan_2 = Pesos_Juan %>% 
  filter(FP >= "2022-07-25",
         FP < "2022-08-16")
hierarchy_data_2 = readRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

hierarchy_pesos_2 = merge.data.frame(x = hierarchy_data_2, y = Pesos_Juan_2, 
                                     by = "ID")

boxplot(hierarchy_pesos_2$Pe ~ hierarchy_pesos_2$hierarchy)

if (any(table(hierarchy_pesos_2$ID) > 1)) {
  lmerTest::lmer(Pe ~ hierarchy + treatment + edad + (1|ID), 
                 data = hierarchy_pesos_2) %>% 
    sjPlot::plot_model(show.values = T) %>% plot()
  print("lmer")
} else {
  lm(Pe ~ hierarchy + treatment + edad, 
     data = hierarchy_pesos_2) %>% 
    sjPlot::plot_model(show.values = T) %>% 
    plot()
  print("lm")
}


# pesos_juan_3 ------------------------------------------------------------

Pesos_Juan_3 = Pesos_Juan %>% 
  filter(FP >= "2022-08-16")
hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

hierarchy_pesos_3 = merge.data.frame(x = hierarchy_data_3, y = Pesos_Juan_3, 
                                     by = "ID")

boxplot(hierarchy_pesos_3$Pe ~ hierarchy_pesos_3$hierarchy)

if (any(table(hierarchy_pesos_3$ID) > 1)) {
  lmerTest::lmer(Pe ~ hierarchy + treatment + edad + (1|ID), 
                 data = hierarchy_pesos_3) %>% 
    sjPlot::plot_model(show.values = T) %>% plot()
  print("lmer")
} else {
  lm(Pe ~ hierarchy + treatment + edad, 
     data = hierarchy_pesos_3) %>% 
    sjPlot::plot_model(show.values = T) %>% 
    plot()
  print("lm")
}



# Bar plot ----------------------------------------------------------------



hierarchy_pesos_1_summary <- hierarchy_pesos_1 %>%
  group_by(hierarchy) %>%
  transmute(
    weight = mean(Pe),
    se = sd(Pe) / sqrt(n()),  # Calculate standard error
    phase = 1
  ) %>%
  unique()
hierarchy_pesos_2_summary <- hierarchy_pesos_2 %>%
  group_by(hierarchy) %>%
  transmute(
    weight = mean(Pe),
    se = sd(Pe) / sqrt(n()),  # Calculate standard error
    phase = 2
  ) %>%
  unique()
hierarchy_pesos_3_summary <- hierarchy_pesos_3 %>%
  group_by(hierarchy) %>%
  transmute(
    weight = mean(Pe),
    se = sd(Pe) / sqrt(n()),  # Calculate standard error
    phase = 3
  ) %>%
  unique()

# Combine the summaries
hierarchy_pesos_summary <- rbind.data.frame(
  hierarchy_pesos_1_summary,
  hierarchy_pesos_2_summary,
  hierarchy_pesos_3_summary
)


hierarchy_pesos_summary$hierarchy <- 
  str_to_title(hierarchy_pesos_summary$hierarchy)


hierarchy_pesos_summary$hierarchy = factor(hierarchy_pesos_summary$hierarchy, 
                                           levels = sort(unique(hierarchy_pesos_summary$hierarchy), 
                                                         decreasing = T))


dir.create("output/hierarchy/descriptiva/weight_vs_hierarchy_v3/", recursive = T)

png(filename = "output/hierarchy/descriptiva/weight_vs_hierarchy_v3/barplot_weight_phase_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

library(ggplot2)
# Default bar plot
p <- ggplot(hierarchy_pesos_summary, aes(x = phase, y = weight, fill = hierarchy)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = weight - se, ymax = weight + se), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    title = "Weight per phase and hierarchy",
    x = "Phase",
    y = "Weight (kg)",
    fill = "Hierarchy"  
  ) +
  theme_classic()

# Define the positions for the significance bars
bar_positions <- data.frame(
  x = c(3),  # X-axis positions for the bars
  y = c(145),  # Y-axis positions for the bars (adjust as needed)
  label = c("*")  # Significance labels for each bar
)

# Add manual significance bars and labels to the plot
p +
  annotate(
    "text",
    x = bar_positions$x,
    y = bar_positions$y,
    label = bar_positions$label,
    size = 5,  # Adjust text size as needed
    vjust = -0.5  # Adjust vertical position of labels
  ) +
  annotate(
    "segment",
    x = c(2.70),
    xend = c(3.3),
    y = c(147),  # Adjust Y positions for bars
    yend = c(147),
    size = 0.5,  # Adjust line width as needed
    color = "black"  # Line color
  ) +
  annotate(
    "segment",
    x = c(2.70),
    xend = c(2.7),
    y = c(147),  # Adjust Y positions for bars
    yend = c(145),
    size = 0.5,  # Adjust line width as needed
    color = "black"  # Line color
  ) +
  annotate(
    "segment",
    x = c(3.3),
    xend = c(3.3),
    y = c(147),  # Adjust Y positions for bars
    yend = c(145),
    size = 0.5,  # Adjust line width as needed
    color = "black"  # Line color
  )

dev.off()



