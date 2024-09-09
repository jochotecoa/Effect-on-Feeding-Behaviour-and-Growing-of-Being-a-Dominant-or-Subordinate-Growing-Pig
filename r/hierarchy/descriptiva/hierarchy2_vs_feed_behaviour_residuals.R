


# fase 0 ------------------------------------------------------------------

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_2.rds')
hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'

boxplot(Dominant.Confirmed ~ hierarchy, hierarchy_data)
boxplot(Dominant ~ hierarchy, hierarchy_data)

feed_data_1 = readRDS('output/ranking_diff_consumo/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'initial') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')

boxplot(Co_median ~ hierarchy, feed_data_1)
ggstatsplot::ggbetweenstats(data = feed_data_1, x = hierarchy, y = Co_median, 
                            type = 'n')

nvis_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')

boxplot(residuals_nvis_median ~ hierarchy, nvis_total_data_1)
ggstatsplot::ggbetweenstats(data = nvis_total_data_1, x = hierarchy, 
                            y = residuals_nvis_median, type = 'n')

ti_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')

boxplot(residuals_ti_median ~ hierarchy, ti_total_data_1)
ggstatsplot::ggbetweenstats(data = ti_total_data_1, x = hierarchy, y = residuals_ti_median)

fr_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')

boxplot(residuals_Fr_median ~ hierarchy, fr_total_data_1)
ggstatsplot::ggbetweenstats(data = fr_total_data_1, x = hierarchy, y = residuals_Fr_median)


ti_median_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')

boxplot(median_Ti_median ~ hierarchy, ti_median_data_1)
lm(median_Ti_median ~ hierarchy + Pen, ti_median_data_1) %>% summary()

ggstatsplot::ggbetweenstats(data = ti_median_data_1, x = hierarchy, y = median_Ti_median)
