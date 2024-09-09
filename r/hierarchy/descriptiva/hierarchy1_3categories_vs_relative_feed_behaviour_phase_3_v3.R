library(tidyverse)
library(sjPlot)

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(Pen = as.factor(Pen))
# hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'

# phase 3, group == 'second', group = '2'

# boxplot(Dominant.Confirmed ~ hierarchy + Control, hierarchy_data)
# boxplot(Dominant ~ hierarchy + Control, hierarchy_data)

library(dplyr)


is.odd <- function(x) x %% 2 != 0


# Intake ------------------------------------------------------------------


feed_data_3 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'second') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup() %>%    
  mutate(Control = as.factor(is.odd(as.numeric(Pen))),
         hierarchy = str_to_title(hierarchy))

co_lm = lm(Co_median ~ hierarchy + Control + Pen, feed_data_3)

co_lm %>% plot_model(show.values = T)

dir.create("output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v3/")

png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v3/marginal_effects_plot_intake_vs_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

co_lm %>%
  plot_model(show.values = TRUE, type = "pred", 
             title = "Predicted values of Food Intake", 
             axis.title = c("Hierarchy", "Food Intake (g)"), 
             terms = "hierarchy[Submissive, Intermediate, Dominant]") +
  scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))

dev.off()

co_rel_lm = lm(Co_median_rel ~ hierarchy + Control, feed_data_3)
co_rel_lm %>% plot_model(show.values = T)

png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v3/marginal_effects_plot_relative_intake_vs_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

co_rel_lm %>%
  plot_model(show.values = TRUE, type = "pred", 
             title = "Predicted values of Relative Food Intake", 
             axis.title = c("Hierarchy", "Relative Food Intake (Intake/Median Intake)"), 
             terms = "hierarchy[Submissive, Intermediate, Dominant]") +
  scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))

dev.off()

# boxplot(Co_median_rel ~ hierarchy + Control, feed_data_3)
ggstatsplot::ggbetweenstats(data = feed_data_3, x = hierarchy, 
                            y = Co_median_rel)

is.odd <- function(x) x %% 2 != 0


# N visits ----------------------------------------------------------------


nvis_total_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/median(n_visitas_median)) %>% 
  ungroup() %>% 
    mutate(Control = as.factor(is.odd(as.numeric(Pen))),          
           hierarchy = str_to_title(hierarchy))

n_vis_model = lm(n_visitas_median ~ hierarchy + Control + Pen, nvis_total_data_3) 
n_vis_rel_model = lm(n_visitas_median_rel ~ hierarchy + Control, nvis_total_data_3)

png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v3/marginal_effects_plot_n_visits_vs_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

n_vis_model %>%
  plot_model(show.values = TRUE, type = "pred", 
             title = "Predicted values of Number of Visits", 
             axis.title = c("Hierarchy", "Number of Visits"), 
             terms = "hierarchy[Submissive, Intermediate, Dominant]") +
  scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))

dev.off()


png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v3/marginal_effects_plot_relative_n_visits_vs_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

n_vis_rel_model %>%
  plot_model(show.values = TRUE, type = "pred", 
             title = "Predicted values of Relative Number of Visits", 
             axis.title = c("Hierarchy", "Relative Number of Visits"), 
             terms = "hierarchy[Submissive, Intermediate, Dominant]") +
  scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))

dev.off()

# boxplot(n_visitas_median_rel ~ hierarchy + Control, nvis_total_data_3)
ggstatsplot::ggbetweenstats(data = nvis_total_data_3, x = hierarchy, 
                            y = n_visitas_median_rel, type = 'n')

# Total time --------------------------------------------------------------


ti_total_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Ti_median_rel = Ti_median/median(Ti_median)) %>% 
  ungroup() %>% 
    mutate(Control = as.factor(is.odd(as.numeric(Pen))),          
           hierarchy = str_to_title(hierarchy))

Ti_median_lm = lm(Ti_median ~ hierarchy + Control + Pen, ti_total_data_3)
Ti_median_rel_lm = lm(Ti_median_rel ~ hierarchy + Control, ti_total_data_3) 


png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v3/marginal_effects_plot_total_time_vs_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

Ti_median_lm %>%
  plot_model(show.values = TRUE, type = "pred", 
             title = "Predicted values of Total Time", 
             axis.title = c("Hierarchy", "Total Time (s)"), 
             terms = "hierarchy[Submissive, Intermediate, Dominant]") +
  scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))

dev.off()

# Feed rate ---------------------------------------------------------------


fr_total_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Fr_median_rel = Fr_median/median(Fr_median)) %>% 
  ungroup() %>% 
    mutate(Control = as.factor(is.odd(as.numeric(Pen))),         
           hierarchy = str_to_title(hierarchy))



# boxplot(Fr_median_rel ~ hierarchy + Control, filter(fr_total_data_3, hierarchy != 'intermediate'))
lm(Fr_median_rel ~ hierarchy + Control + Pen, fr_total_data_3) %>% plot_model(show.values = T)
lm(Fr_median_rel ~ hierarchy + Control, fr_total_data_3) %>% plot_model(show.values = T)


# Time per feed -----------------------------------------------------------


ti_median_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/median(median_Ti_median)) %>% 
  ungroup() %>% 
    mutate(Control = as.factor(is.odd(as.numeric(Pen))),          
           hierarchy = str_to_title(hierarchy))

# boxplot(median_Ti_median_rel ~ hierarchy + Control, filter(ti_median_data_3, hierarchy != 'intermediate'))
median_Ti_median_lm = lm(median_Ti_median ~ hierarchy + Control + Pen, ti_median_data_3)
median_Ti_median_rel_lm = lm(median_Ti_median_rel ~ hierarchy + Control, ti_median_data_3)




png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v3/marginal_effects_plot_relative_median_time_per_visit_vs_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

median_Ti_median_rel_lm %>%
  plot_model(show.values = TRUE, type = "pred", 
             title = "Predicted values of Relative Median Time per Visit", 
             axis.title = c("Hierarchy", "Relative Median Time per Visit (s)"), 
             terms = "hierarchy[Submissive, Intermediate, Dominant]") +
  scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))

dev.off()

# Time slots --------------------------------------------------------------


ti_range_data_3 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
    mutate(Control = as.factor(is.odd(as.numeric(Pen))),          
           hierarchy = str_to_title(hierarchy))

lm(H6_H9 ~ hierarchy + Control, ti_range_data_3) %>% sjPlot::plot_model(show.values = T)


for (variable in subset(colnames(ti_range_data_3), grepl('H', colnames(ti_range_data_3)))) {
  print(variable)
  shp_tst = shapiro.test(ti_range_data_3[, variable])
  ti_range_data_3_i = ti_range_data_3 %>% 
    group_by(Pen) %>% 
    mutate(time_rel = !!sym(variable)/median(!!sym(variable))) %>% 
    ungroup() 
  # %>% 
    # filter(hierarchy != 'intermediate')
  for (variable_i in c(variable, "time_rel")) {
    # Assuming 'character' contains the variable name as a character string
    variable_name <- as.name(variable_i)
    
    # Creating the formula dynamically
    if (grepl("_rel", variable_name)) {
      formula <- as.formula(paste0(deparse(variable_name), " ~ hierarchy + Control"))
    } else {
      formula <- as.formula(paste0(deparse(variable_name), " ~ hierarchy + Control + Pen"))
    }
    
    # Fitting the linear model
    lm_model <- lm(formula, data = ti_range_data_3_i)
    
    # Plotting the predictions
    # if (variable == "H10_H13_perc" & variable_i == "time_rel") {
    #   png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v3/marginal_effects_plot_relative_percentage_time_from_1000_to_1359_vs_hierarchy.png", 
    #       width = 1080*4, height = 720*4, res = 150*4)
    #   
    #   lm_model %>%
    #     plot_model(show.values = TRUE, type = "pred", 
    #                title = "Predicted values of Relative % of Time Feeding between 10H and 14H", 
    #                axis.title = c("Hierarchy", "Relative % of Time Feeding between 10H and 14H"), 
    #                terms = "hierarchy[Submissive, Intermediate, Dominant]") +
    #     scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))
    #   
    #   dev.off()
    #   
    #   readline(prompt="Press [enter] to continue")
    #   
    # }
    
    # Plotting the model using sjPlot
    model_summ = lm_model %>% summary()
    
    if (any(model_summ$coefficients[2:3,4] < 0.05)) {
      a = sjPlot::plot_model(lm_model, show.values = TRUE)
      
      plot(a)
      readline(prompt="Press [enter] to continue")
      
    }
    
  }
  
}
