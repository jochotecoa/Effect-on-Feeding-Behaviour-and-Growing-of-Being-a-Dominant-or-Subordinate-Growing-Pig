source('r/load_data.R')

adg_df = consumo_pesos %>% 
  mutate(FC = `F`) %>% 
  group_by(Ta, group) %>% 
  mutate(adg = (max(Pe_app) - min(Pe_app))/ as.numeric(max(FC) - min(FC))) %>% 
  group_by(Ta) %>% 
  mutate(Pe_i = min(Pe_app)) %>% 
  select(adg, Control, group, Pe_i, Ta) %>% 
  unique()


model_adg <- lmer(adg ~ Control*group + Pe_i + (Control | Ta), data = adg_df)

model_adg %>% summary

adg_df$pred = predict(model_adg, adg_df)
adg_df$resdls = adg_df$adg -  adg_df$pred

adg_rsdls = adg_df %>% group_by(Ta) %>% 
  summarise(resdls_abs_median = median(abs(resdls)), 
            Ta, Control) 

a = adg_df %>% filter(Ta == !!adg_rsdls$Ta[which.max(adg_rsdls$resdls_abs_median)])

b = adg_df %>% group_by(Pe_i) %>% summarise(adg = mean(adg))
points(a$group, a$pred)
plot(a$group, a$adg, col = 2)

res_adg = adg_df[order(abs(adg_df$resdls), decreasing = T), 
                                c('Ta', 'group', 'resdls', 'Control')] %>% unique() %>% 
  head(10)

res_ind_adg = adg_rsdls[order(adg_rsdls$resdls_abs_median, decreasing = T), 
                            c('Ta', 'resdls_abs_median', 'Control')] %>% unique() %>% 
  head(10)

dir.create('output/modelo_mixto_adg/')

write.csv(x = res_adg, 
          'output/modelo_mixto_adg/top10_residual_adg_mezcla_jp.csv')
write.csv(res_ind_adg,
          'output/modelo_mixto_adg/top10_residual_adg_individual_jp.csv')
