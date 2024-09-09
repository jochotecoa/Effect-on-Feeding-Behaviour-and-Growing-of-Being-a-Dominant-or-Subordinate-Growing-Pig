library(tidyverse)
library(sjPlot)

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(Pen = as.factor(Pen))
# hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'

# phase 3, group == 'second', group = '2'

# boxplot(Dominant.Confirmed ~ hierarchy + Control, hierarchy_data)
# boxplot(Dominant ~ hierarchy + Control, hierarchy_data)

library(dplyr)

is.odd <- function(x) x %% 2 != 0

# intake ------------------------------------------------------------------

# initial   first  second 
# 62      62      62 


feed_data_1 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'initial') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup() %>% mutate(Control = is.odd(as.numeric(Pen)))

co_lm = lm(Co_median ~ hierarchy + Control + Pen, feed_data_1)
co_lm %>% plot_model(show.values = T)

co_lm = lm(Co_median_rel ~ hierarchy + Control, feed_data_1)

co_lm %>% plot_model(show.values = T)
co_lm %>% plot_model(show.values = T, type = "pred")


# boxplot(Co_median_rel ~ hierarchy + Control, feed_data_1)
# ggstatsplot::ggbetweenstats(data = feed_data_1, x = hierarchy, 
                            # y = Co_median_rel)

# N visits ----------------------------------------------------------------

# 0  1  2 
# 62 63 63 

nvis_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/median(n_visitas_median)) %>% 
  ungroup() %>% mutate(Control = is.odd(as.numeric(Pen)))

lm(n_visitas_median ~ hierarchy + Control + Pen, nvis_total_data_1) %>% plot_model(show.values = T)
lm(n_visitas_median_rel ~ hierarchy + Control, nvis_total_data_1) %>% plot_model(show.values = T)

# boxplot(n_visitas_median_rel ~ hierarchy + Control, nvis_total_data_1)
# ggstatsplot::ggbetweenstats(data = nvis_total_data_1, x = hierarchy, 
                            # y = n_visitas_median_rel, type = 'n')

# Time total --------------------------------------------------------------

# 0  1  2 
# 62 63 63 

ti_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Ti_median_rel = Ti_median/median(Ti_median)) %>% 
  ungroup() %>% mutate(Control = is.odd(as.numeric(Pen)))

lm(Ti_median ~ hierarchy + Control + Pen, ti_total_data_1) %>% summary()
lm(Ti_median_rel ~ hierarchy + Control, ti_total_data_1) %>% summary()

# boxplot(Ti_median_rel ~ hierarchy + Control, ti_total_data_1)
# ti_total_data_1$Ti_median_rel %>% shapiro.test()
# ggstatsplot::ggbetweenstats(data = ti_total_data_1, x = hierarchy, 
                            # y = Ti_median_rel, type = "nonparametric")

# Feed rate ---------------------------------------------------------------

# 0  1  2 
# 62 63 63 

fr_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Fr_median_rel = Fr_median/median(Fr_median)) %>% 
  ungroup() %>% mutate(Control = is.odd(as.numeric(Pen)))

# fr_total_data_1$Fr_median_rel %>% shapiro.test()


# boxplot(Fr_median_rel ~ hierarchy + Control, filter(fr_total_data_1, hierarchy != 'intermediate'))

lm(Fr_median ~ hierarchy + Control + Pen, fr_total_data_1) %>% summary()
lm(Fr_median_rel ~ hierarchy + Control, fr_total_data_1) %>% summary()

# ggstatsplot::ggbetweenstats(data = filter(fr_total_data_1, hierarchy != 'intermediate'), x = hierarchy, 
                            # y = Fr_median_rel,type = 'n')


# Time per feed -----------------------------------------------------------

# 0  1  2 
# 62 63 63 

ti_median_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/median(median_Ti_median)) %>% 
  ungroup() %>% mutate(Control = is.odd(as.numeric(Pen)))

lm(median_Ti_median ~ hierarchy + Control + Pen, ti_median_data_1) %>% summary()
lm(median_Ti_median_rel ~ hierarchy + Control, ti_median_data_1) %>% summary()


# boxplot(median_Ti_median_rel ~ hierarchy + Control, filter(ti_median_data_1, hierarchy != 'intermediate'))
ti_median_data_1$median_Ti_median_rel %>% shapiro.test()

# ggstatsplot::ggbetweenstats(data = filter(ti_median_data_1, hierarchy != 'intermediate'), x = hierarchy, 
                            # y = median_Ti_median_rel, type = "n")


# Time slots --------------------------------------------------------------


ti_range_data_1 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')


for (variable in subset(colnames(ti_range_data_1), grepl('H', colnames(ti_range_data_1)))) {
  print(variable)
  # shp_tst = shapiro.test(ti_range_data_1[, variable])
  ti_range_data_1_i = ti_range_data_1 %>% 
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
    lm_model <- lm(formula, data = ti_range_data_1_i)
    
    model_summ = lm_model %>% summary()
    
    if (any(model_summ$coefficients[2:3,4] < 0.05)) {
      # Plotting the model using sjPlot
      a = sjPlot::plot_model(lm_model, show.values = TRUE)
      plot(a)
      readline(prompt="Press [enter] to continue")
      
    }
    
  
    
  }
  
}
