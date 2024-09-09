library(tidyverse)
library(sjPlot)

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds') %>% 
  mutate(Pen = as.factor(Pen))
# hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'

# phase 3, group == 'second', group = '2'

boxplot(Dominant.Confirmed ~ hierarchy, hierarchy_data)
boxplot(Dominant ~ hierarchy, hierarchy_data)

library(dplyr)




feed_data_2 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'first') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup()

co_lm = lm(Co_median ~ hierarchy + Pen, feed_data_2)

co_lm %>% plot_model(show.values = T)
co_lm %>% plot_model(show.values = T, type = "pred")


boxplot(Co_median_rel ~ hierarchy, feed_data_2)
ggstatsplot::ggbetweenstats(data = feed_data_2, x = hierarchy, 
                            y = Co_median_rel)

nvis_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/median(n_visitas_median)) %>% 
  ungroup()

lm(n_visitas_median_rel ~ hierarchy + Pen, nvis_total_data_2) %>% plot_model(show.values = T)
boxplot(n_visitas_median_rel ~ hierarchy, nvis_total_data_2)
ggstatsplot::ggbetweenstats(data = nvis_total_data_2, x = hierarchy, 
                            y = n_visitas_median_rel, type = 'n')

ti_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Ti_median_rel = Ti_median/median(Ti_median)) %>% 
  ungroup()

lm(Ti_median_rel ~ hierarchy + Pen, ti_total_data_2) %>% summary()
boxplot(Ti_median_rel ~ hierarchy, ti_total_data_2)
ti_total_data_2$Ti_median_rel %>% shapiro.test()
ggstatsplot::ggbetweenstats(data = ti_total_data_2, x = hierarchy, 
                            y = Ti_median_rel, type = "nonparametric")

fr_total_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Fr_median_rel = Fr_median/median(Fr_median)) %>% 
  ungroup()

fr_total_data_2$Fr_median_rel %>% shapiro.test()


boxplot(Fr_median_rel ~ hierarchy, filter(fr_total_data_2, hierarchy != 'intermediate'))
lm(Fr_median_rel ~ hierarchy + Pen, filter(fr_total_data_2, hierarchy != 'intermediate')) %>% summary()
ggstatsplot::ggbetweenstats(data = filter(fr_total_data_2, hierarchy != 'intermediate'), x = hierarchy, 
                            y = Fr_median_rel,type = 'n')


ti_median_data_2 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/median(median_Ti_median)) %>% 
  ungroup()

boxplot(median_Ti_median_rel ~ hierarchy, filter(ti_median_data_2, hierarchy != 'intermediate'))
ti_median_data_2$median_Ti_median_rel %>% shapiro.test()

ggstatsplot::ggbetweenstats(data = filter(ti_median_data_2, hierarchy != 'intermediate'), x = hierarchy, 
                            y = median_Ti_median_rel, type = "n")


ti_range_data_2 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 1) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')

lm(H6_H9 ~ hierarchy, ti_range_data_2) %>% sjPlot::plot_model(show.values = T)

# for (variable in subset(colnames(ti_range_data_2), grepl('H', colnames(ti_range_data_2)))) {
#   boxplot(ti_range_data_2[[variable]] ~ ti_range_data_2$hierarchy, 
#           xlab = 'hierarchy', ylab = variable)
# }
# 
# for (variable in subset(colnames(ti_range_data_2), grepl('H', colnames(ti_range_data_2)))) {
#   a = ggstatsplot::ggbetweenstats(data = ti_range_data_2, x = hierarchy, 
#                               y = variable)
#   
# }

for (variable in subset(colnames(ti_range_data_2), grepl('H', colnames(ti_range_data_2)))) {
  print(variable)
  shp_tst = shapiro.test(ti_range_data_2[, variable])
  ti_range_data_2_i = ti_range_data_2 %>% 
    group_by(Pen) %>% 
    mutate(time_rel = !!sym(variable)/median(!!sym(variable))) %>% 
    ungroup() %>% 
    filter(hierarchy != 'intermediate')
  
  if (shp_tst$p.value >= 0.05) {
    a = ggstatsplot::ggbetweenstats(data = ti_range_data_2_i, x = hierarchy, 
                                    title =  variable, y = time_rel)
  } else {
    a = ggstatsplot::ggbetweenstats(data = ti_range_data_2_i, x = hierarchy, 
                                    title =  variable, y = time_rel, 
                                    type = 'nonparametric')
  }
  plot(a)
  readline(prompt="Press [enter] to continue")
}
