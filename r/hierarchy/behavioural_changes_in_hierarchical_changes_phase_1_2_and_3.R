library(tidyverse)
library(sjPlot)
library(dplyr)

hierarchy_data_12 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_1_to_2.rds') %>% 
  mutate(hierarchy_change_value = ifelse(hierarchy.x == hierarchy.y, 
                                         'equal', 
                                         ifelse(hierarchy.x == 'dominant', 
                                                'worse', 
                                                ifelse(hierarchy.x == 'submissive',
                                                       'better',
                                                       ifelse(hierarchy.y == 'dominant',
                                                              'better',
                                                              'worse'))))) %>% 
  mutate(hierarchy_change_value = factor(hierarchy_change_value, 
                                         levels = c('equal', 'better', 'worse')))

hierarchy_data_23 = readRDS('output/changes_in_hierarchy/hierarchy_data_changes_from_phase_2_to_3.rds') %>% 
  mutate(hierarchy_change_value = ifelse(hierarchy.x == hierarchy.y, 
                                         'equal', 
                                         ifelse(hierarchy.x == 'dominant', 
                                                'worse', 
                                                ifelse(hierarchy.x == 'submissive',
                                                       'better',
                                                       ifelse(hierarchy.y == 'dominant',
                                                              'better',
                                                              'worse'))))) %>% 
  mutate(hierarchy_change_value = factor(hierarchy_change_value, 
                                         levels = c('equal', 'better', 'worse')))


hierarchy_data_123 = merge.data.frame(x = hierarchy_data_12, 
                                      y = hierarchy_data_23, 
                                      by = 'ID') %>% 
  mutate(hierarchy_change_value = paste(hierarchy_change_value.x, 
                                        hierarchy_change_value.y, 
                                        sep = '_'))

hierarchy_change_value = hierarchy_data_123$hierarchy_change_value %>% unique()

hierarchy_data_123$hierarchy_change_value = 
  factor(hierarchy_data_123$hierarchy_change_value, 
         levels = c("equal_equal", 
                    subset(hierarchy_change_value, 
                           !grepl("equal_equal", 
                                  hierarchy_change_value))))


# Consumption -------------------------------------------------------------


feed_data_1 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'initial') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>%
  group_by(Pen.x.x) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup() %>% 
  mutate(phase = '1')
feed_data_1  <- feed_data_1[, !duplicated(as.list(feed_data_1))]

feed_data_2 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'first') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>%
  group_by(Pen.x.y) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup() %>% 
  mutate(phase = '2')
feed_data_2  <- feed_data_2[, !duplicated(as.list(feed_data_2))]

feed_data_3 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'second') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>%
  group_by(Pen.y.y) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup() %>% 
  mutate(phase = '3')
feed_data_3  <- feed_data_3[, !duplicated(as.list(feed_data_3))]

feed_data_123 = rbind.data.frame(feed_data_1, feed_data_2, feed_data_3)


# Assuming you've installed and loaded the 'lme4' package
library(lmerTest)

# Assuming your data frame is named 'pig_data'
model <- lmer(Co_median ~ phase * hierarchy_change_value + (1 | ID), data = feed_data_123)
summary(model)
plot_model(model, type = "pred", terms = c("phase", "hierarchy_change_value"))

model <- lmer(Co_median_rel ~ phase * hierarchy_change_value + (1 | ID), data = feed_data_123)
summary(model)
plot_model(model, type = "pred", terms = c("phase", "hierarchy_change_value"))
# N visits ----------------------------------------------------------------


nvis_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.x.x) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/median(n_visitas_median)) %>% 
  ungroup() %>% 
  mutate(phase = '1')

nvis_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.x.y) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/median(n_visitas_median)) %>% 
  ungroup() %>% 
  mutate(phase = '2')

nvis_total_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.y.y) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/median(n_visitas_median)) %>% 
  ungroup() %>% 
  mutate(phase = '3')

nvis_total_data_123 = rbind.data.frame(nvis_total_data_1, nvis_total_data_2, nvis_total_data_3)


# Assuming you've installed and loaded the 'lme4' package
library(lmerTest)

# Assuming your data frame is named 'pig_data'
model <- lmer(n_visitas_median ~ phase * hierarchy_change_value + (1 | ID), data = nvis_total_data_123)
summary(model)
model <- lmer(n_visitas_median_rel ~ phase * hierarchy_change_value + (1 | ID), data = nvis_total_data_123)
summary(model)


# Total time consumption --------------------------------------------------


ti_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.x.x) %>% 
  mutate(Ti_median_rel = Ti_median/median(Ti_median)) %>% 
  ungroup() %>% 
  mutate(phase = '1')

ti_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.x.y) %>% 
  mutate(Ti_median_rel = Ti_median/median(Ti_median)) %>% 
  ungroup() %>% 
  mutate(phase = '2')

ti_total_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.y.y) %>% 
  mutate(Ti_median_rel = Ti_median/median(Ti_median)) %>% 
  ungroup() %>% 
  mutate(phase = '3')

ti_total_data_123 = rbind.data.frame(ti_total_data_1, ti_total_data_2, ti_total_data_3)


# Assuming you've installed and loaded the 'lme4' package
library(lmerTest)

# Assuming your data frame is named 'pig_data'
model <- lmer(Ti_median ~ phase * hierarchy_change_value + (1 | ID), data = ti_total_data_123)
summary(model)
model <- lmer(Ti_median_rel ~ phase * hierarchy_change_value + (1 | ID), data = ti_total_data_123)
summary(model)

# Feed Rate ---------------------------------------------------------------


fr_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.x.x) %>% 
  mutate(Fr_median_rel = Fr_median/median(Fr_median)) %>% 
  ungroup() %>% 
  mutate(phase = '1')

fr_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.x.y) %>% 
  mutate(Fr_median_rel = Fr_median/median(Fr_median)) %>% 
  ungroup() %>% 
  mutate(phase = '2')

fr_total_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.y.y) %>% 
  mutate(Fr_median_rel = Fr_median/median(Fr_median)) %>% 
  ungroup() %>% 
  mutate(phase = '3')

fr_total_data_123 = rbind.data.frame(fr_total_data_1, fr_total_data_2, fr_total_data_3)


# Assuming you've installed and loaded the 'lme4' package
library(lmerTest)

# Assuming your data frame is named 'pig_data'
model <- lmer(Fr_median ~ phase * hierarchy_change_value + (1 | ID), data = fr_total_data_123)
summary(model)
plot_model(model, type = "pred", terms = c("phase", "hierarchy_change_value"))

model <- lmer(Fr_median_rel ~ phase * hierarchy_change_value + (1 | ID), data = fr_total_data_123)
summary(model)
plot_model(model, type = "pred", terms = c("phase", "hierarchy_change_value"))


# Time per feed -----------------------------------------------------------


ti_median_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.x.x) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/median(median_Ti_median)) %>% 
  ungroup() %>% 
  mutate(phase = '1')

ti_median_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.x.y) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/median(median_Ti_median)) %>% 
  ungroup() %>% 
  mutate(phase = '2')

ti_median_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  group_by(Pen.y.y) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/median(median_Ti_median)) %>% 
  ungroup() %>% 
  mutate(phase = '3')

ti_median_data_123 = rbind.data.frame(ti_median_data_1, ti_median_data_2, ti_median_data_3)


# Assuming you've installed and loaded the 'lme4' package
library(lmerTest)

# Assuming your data frame is named 'pig_data'
model <- lmer(median_Ti_median ~ phase * hierarchy_change_value + (1 | ID), data = ti_median_data_123)
summary(model)
plot_model(model, type = "pred", terms = c("phase", "hierarchy_change_value"))

model <- lmer(median_Ti_median_rel ~ phase * hierarchy_change_value + (1 | ID), data = ti_median_data_123)
summary(model)
plot_model(model, type = "pred", terms = c("phase", "hierarchy_change_value"))


# Time slots --------------------------------------------------------------

ti_range_data_1 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  mutate(phase = '1')

ti_range_data_2 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  mutate(phase = '2')

ti_range_data_3 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data_123, by = 'ID') %>% 
  mutate(phase = '3')

ti_range_data_123 = rbind.data.frame(ti_range_data_1, ti_range_data_2)

time_ranges = subset(colnames(ti_range_data_123), grepl("H", colnames(ti_range_data_123)))

for (time_range in time_ranges) {
  formulae = formula(paste0(time_range, " ~ phase * hierarchy_change_value + (1 | ID)"))
  
  model <- lmer(formulae, data = ti_range_data_123)
  summary(model) %>% print()
  plot_model(model, type = "pred", terms = c("phase", "hierarchy_change_value")) %>% plot()
  readline(prompt="Press [enter] to continue")
  
}
