library(tidyverse)

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(Pen = as.factor(Pen))
hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'



boxplot(Dominant.Confirmed ~ hierarchy, hierarchy_data)
boxplot(Dominant ~ hierarchy, hierarchy_data)

library(dplyr)




feed_data_1 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'initial') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(submissive_mean = mean(ifelse(hierarchy == "submissive", Co_median, NA), na.rm = TRUE)) %>% 
  mutate(Co_median_rel = Co_median/submissive_mean) %>% 
  ungroup()

lm(Co_median ~ hierarchy + Pen, feed_data_1) %>% summary()
boxplot(Co_median_rel ~ hierarchy, feed_data_1)
ggstatsplot::ggbetweenstats(data = feed_data_1, x = hierarchy, y = Co_median_rel)

nvis_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(submissive_mean = mean(ifelse(hierarchy == "submissive", n_visitas_median, NA), na.rm = TRUE)) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/submissive_mean) %>% 
  ungroup()

boxplot(n_visitas_median_rel ~ hierarchy, nvis_total_data_1)
ggstatsplot::ggbetweenstats(data = nvis_total_data_1, x = hierarchy, y = n_visitas_median_rel, type = 'n')

ti_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(submissive_mean = mean(ifelse(hierarchy == "submissive", Ti_median, NA), na.rm = TRUE)) %>% 
  mutate(Ti_median_rel = Ti_median/submissive_mean) %>% 
  ungroup()

boxplot(Ti_median_rel ~ hierarchy, ti_total_data_1)
ggstatsplot::ggbetweenstats(data = ti_total_data_1, x = hierarchy, 
                            y = Ti_median_rel, type = "nonparametric")

fr_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(submissive_mean = mean(ifelse(hierarchy == "submissive", Fr_median, NA), na.rm = TRUE)) %>% 
  mutate(Fr_median_rel = Fr_median/submissive_mean) %>% 
  ungroup()

boxplot(Fr_median_rel ~ hierarchy, fr_total_data_1)
ggstatsplot::ggbetweenstats(data = fr_total_data_1, x = hierarchy, 
                            y = Fr_median_rel, type = 'n')


ti_median_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(submissive_mean = mean(ifelse(hierarchy == "submissive", 
                                       median_Ti_median, NA), na.rm = TRUE)) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/submissive_mean) %>% 
  ungroup()

boxplot(median_Ti_median_rel ~ hierarchy, ti_median_data_1)

ggstatsplot::ggbetweenstats(data = ti_median_data_1, x = hierarchy, 
                            y = median_Ti_median_rel, type = "n")


ti_range_data_1 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')


for (variable in subset(colnames(ti_range_data_1), grepl('H', colnames(ti_range_data_1)))) {
  boxplot(ti_range_data_1[[variable]] ~ ti_range_data_1$hierarchy, 
          xlab = 'hierarchy', ylab = variable)
}

for (variable in subset(colnames(ti_range_data_1), grepl('H', colnames(ti_range_data_1)))) {
  a = ggstatsplot::ggbetweenstats(data = ti_range_data_1, x = hierarchy, 
                              y = variable)
  
}

for (variable in subset(colnames(ti_range_data_1), grepl('H', colnames(ti_range_data_1)))) {
  print(variable)
  shp_tst = shapiro.test(ti_range_data_1[, variable])
  ti_range_data_1_i = ti_range_data_1 %>% 
    group_by(Pen) %>% 
    mutate(submissive_mean = mean(ifelse(hierarchy == "submissive", 
                                         !!sym(variable), NA), na.rm = TRUE)) %>% 
    mutate(time_rel = !!sym(variable)/submissive_mean) %>% 
    ungroup()
  
  if (shp_tst$p.value >= 0.05) {
    a = ggstatsplot::ggbetweenstats(data = ti_range_data_1_i, x = hierarchy, 
                                    title =  variable, y = time_rel)
  } else {
    a = ggstatsplot::ggbetweenstats(data = ti_range_data_1_i, x = hierarchy, 
                                    title =  variable, y = time_rel, 
                                    type = 'nonparametric')
  }
  plot(a)
}
