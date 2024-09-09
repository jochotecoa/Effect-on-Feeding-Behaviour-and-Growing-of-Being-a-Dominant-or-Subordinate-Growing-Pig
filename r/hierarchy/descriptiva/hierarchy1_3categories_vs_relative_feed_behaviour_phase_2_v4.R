library(tidyverse)
library(sjPlot)

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds') %>% 
  mutate(Pen = as.factor(Pen))
# hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'

# phase 3, group == 'second', group = '2'

# # boxplot(Dominant.Confirmed ~ hierarchy + Control, hierarchy_data)
# # boxplot(Dominant ~ hierarchy + Control, hierarchy_data)

library(dplyr)

is.odd <- function(x) x %% 2 != 0


# intake ------------------------------------------------------------------

library(stringr)

# initial   first  second 
# 62      62      62 

feed_data_2 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'first') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup() %>%    
  mutate(Control = as.factor(is.odd(as.numeric(Pen))),
         hierarchy = str_to_title(hierarchy))
  

co_lm = lm(Co_median ~ hierarchy + Control + Pen, feed_data_2)
co_rel_lm = lm(Co_median_rel ~ hierarchy + Control, feed_data_2)

co_lm %>% plot_model(show.values = T)
co_rel_lm %>% plot_model(show.values = T)

dir.create("output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_2_v4/")

png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_2_v4/marginal_effects_plot_intake_vs_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

co_lm %>%
  plot_model(show.values = TRUE, type = "pred", 
             title = "Predicted values of Food Intake", 
             axis.title = c("Hierarchy", "Food Intake (g)"), 
             terms = "hierarchy[Submissive, Intermediate, Dominant]") +
  scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))

dev.off()

png(filename = "output/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_2_v4/marginal_effects_plot_relative_intake_vs_hierarchy.png", 
    width = 1080*4, height = 720*4, res = 150*4)

co_rel_lm %>%
  plot_model(show.values = TRUE, type = "pred", 
             title = "Predicted values of Relative Food Intake", 
             axis.title = c("Hierarchy", "Relative Food Intake (Intake/Median Intake)"), 
             terms = "hierarchy[Submissive, Intermediate, Dominant]") +
  scale_x_discrete(limits = c("Submissive", "Intermediate", "Dominant"))

dev.off()

# boxplot(Co_median_rel ~ hierarchy + Control, feed_data_2)
# ggstatsplot::ggbetweenstats(data = feed_data_2, x = hierarchy, 
                            # y = Co_median_rel)

# N visits ----------------------------------------------------------------


nvis_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/median(n_visitas_median)) %>% 
  ungroup() %>%    mutate(Control = is.odd(as.numeric(Pen)))

lm(n_visitas_median ~ hierarchy + Control + Pen, nvis_total_data_2) %>% plot_model(show.values = T)
lm(n_visitas_median_rel ~ hierarchy + Control, nvis_total_data_2) %>% plot_model(show.values = T)

# boxplot(n_visitas_median ~ hierarchy + Control, nvis_total_data_2)
# boxplot(n_visitas_median_rel ~ hierarchy + Control, nvis_total_data_2)
# ggstatsplot::ggbetweenstats(data = nvis_total_data_2, x = hierarchy, 
                            # y = n_visitas_median_rel, type = 'n')

# Time total --------------------------------------------------------------


ti_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Ti_median_rel = Ti_median/median(Ti_median)) %>% 
  ungroup() %>%    mutate(Control = is.odd(as.numeric(Pen)))

lm(Ti_median ~ hierarchy + Control + Pen, ti_total_data_2) %>% summary()
lm(Ti_median_rel ~ hierarchy + Control, ti_total_data_2) %>% summary()
# boxplot(Ti_median_rel ~ hierarchy + Control, ti_total_data_2)
# ti_total_data_2$Ti_median_rel %>% shapiro.test()
# ggstatsplot::ggbetweenstats(data = ti_total_data_2, x = hierarchy, 
                            # y = Ti_median_rel, type = "nonparametric")

# Feed rate ---------------------------------------------------------------


fr_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Fr_median_rel = Fr_median/median(Fr_median)) %>% 
  ungroup() %>%    mutate(Control = is.odd(as.numeric(Pen)))

# fr_total_data_2$Fr_median_rel %>% shapiro.test()


# boxplot(Fr_median_rel ~ hierarchy + Control, filter(fr_total_data_2, hierarchy != 'intermediate'))

lm(Fr_median ~ hierarchy + Control + Pen, fr_total_data_2) %>% summary()
lm(Fr_median_rel ~ hierarchy + Control, fr_total_data_2) %>% summary()

# ggstatsplot::ggbetweenstats(data = filter(fr_total_data_2, hierarchy != 'intermediate'), x = hierarchy, 
                            # y = Fr_median_rel,type = 'n')


# Time per feed -----------------------------------------------------------


ti_median_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/median(median_Ti_median)) %>% 
  ungroup() %>%    mutate(Control = is.odd(as.numeric(Pen)))

# boxplot(median_Ti_median_rel ~ hierarchy + Control, filter(ti_median_data_2, hierarchy != 'intermediate'))
# ti_median_data_2$median_Ti_median_rel %>% shapiro.test()

lm(median_Ti_median ~ hierarchy + Control + Pen, ti_median_data_2) %>% summary()
lm(median_Ti_median_rel ~ hierarchy + Control, ti_median_data_2) %>% summary()


# ggstatsplot::ggbetweenstats(data = filter(ti_median_data_2, hierarchy != 'intermediate'), x = hierarchy, 
                            # y = median_Ti_median_rel, type = "n")

# Time slots --------------------------------------------------------------



ti_range_data_2 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')

# lm(H6_H9 ~ hierarchy + Control, ti_range_data_2) %>% sjPlot::plot_model(show.values = T)

for (variable in subset(colnames(ti_range_data_2), grepl('H', colnames(ti_range_data_2)))) {
  print(variable)
  shp_tst = shapiro.test(ti_range_data_2[, variable])
  ti_range_data_2_i = ti_range_data_2 %>% 
    group_by(Pen) %>% 
    mutate(time_rel = !!sym(variable)/median(!!sym(variable))) %>% 
    ungroup() %>% mutate(Control = is.odd(as.numeric(Pen))) 
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
    lm_model <- lm(formula, data = ti_range_data_2_i)
    
    
    model_summ = lm_model %>% summary()
    
    if (any(model_summ$coefficients[2:3,4] < 0.05)) {
      # Plotting the model using sjPlot
      a = sjPlot::plot_model(lm_model, show.values = TRUE)
      plot(a)
      readline(prompt="Press [enter] to continue")
      
    }
    
  }
  
}
