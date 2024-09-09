


# fase 0 ------------------------------------------------------------------

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_1.rds') %>% 
  merge(readRDS('output/hierarchy_ranking_groups/metadata_phase_1.rds'), by = 'ID') %>% 
  mutate(Pen = as.factor(Pen))
hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'

boxplot(Dominant.Confirmed ~ hierarchy, hierarchy_data)
boxplot(Dominant ~ hierarchy, hierarchy_data)

feed_data_1 = readRDS('output/ranking_diff_consumo/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'initial') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup()

boxplot(Co_median_rel ~ hierarchy, feed_data_1)
abline(h = 1)
ggstatsplot::ggbetweenstats(data = feed_data_1, x = hierarchy, y = Co_median_rel)

nvis_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(residuals_nvis_median_rel = residuals_nvis_median/median(residuals_nvis_median)) %>% 
  ungroup()

boxplot(residuals_nvis_median_rel ~ hierarchy, nvis_total_data_1)
ggstatsplot::ggbetweenstats(data = nvis_total_data_1, x = hierarchy, 
                            y = residuals_nvis_median_rel, type = 'n')

ti_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(residuals_ti_median_rel = residuals_ti_median/median(residuals_ti_median)) %>% 
  ungroup()

boxplot(residuals_ti_median_rel ~ hierarchy, ti_total_data_1) ;abline(h = 1)
ggstatsplot::ggbetweenstats(data = ti_total_data_1, x = hierarchy, 
                            y = residuals_ti_median_rel, type = 'n')

fr_total_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/velocidad_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(residuals_Fr_median_rel = residuals_Fr_median/median(residuals_Fr_median)) %>% 
  ungroup()


boxplot(residuals_Fr_median_rel ~ hierarchy, fr_total_data_1)
ggstatsplot::ggbetweenstats(data = fr_total_data_1, x = hierarchy, y = residuals_Fr_median)


ti_median_data_1 = readRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(median_Ti_median_rel = median_Ti_median/median(median_Ti_median)) %>% 
  ungroup()

boxplot(median_Ti_median_rel ~ hierarchy, ti_median_data_1)
lm(median_Ti_median ~ hierarchy + Pen, ti_median_data_1) %>% summary()

ggstatsplot::ggbetweenstats(data = ti_median_data_1, x = hierarchy, y = median_Ti_median)
