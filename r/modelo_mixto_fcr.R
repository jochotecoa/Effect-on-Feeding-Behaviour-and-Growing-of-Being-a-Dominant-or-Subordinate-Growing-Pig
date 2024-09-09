fcr_df = consumo_pesos %>% 
  mutate(FC = `F`) %>% 
  group_by(Ta, group) %>% 
  mutate(Mean_Weight_Gain = ((max(Pe_app) - min(Pe_app))*1000)/ as.numeric(max(FC) - min(FC)),
         Mean_feed_consumed = mean(Co)) %>% 
  group_by(Ta, group) %>% 
  mutate(fcr = Mean_feed_consumed / Mean_Weight_Gain) %>% 
  group_by(Ta) %>% 
  mutate(Pe_i = min(Pe_app)) %>% 
  select(fcr, Control, group, Pe_i, Ta) %>% 
  unique()

model_fcr <- lmer(fcr ~ Control*group + Pe_i + (Control | Ta), data = fcr_df)

model_fcr %>% summary

fcr_df$pred = predict(model_fcr, fcr_df)
fcr_df$resdls = fcr_df$fcr -  fcr_df$pred

fcr_rsdls = fcr_df %>% group_by(Ta) %>% 
  summarise(resdls_abs_median = median(abs(resdls)), 
            Ta, Control) 

a = fcr_df %>% filter(Ta == !!fcr_rsdls$Ta[which.max(fcr_rsdls$resdls_abs_median)])

b = fcr_df %>% group_by(Pe_i) %>% summarise(fcr = mean(fcr))
plot(a$group, a$pred)
points(a$group, a$fcr, col = 2)

res_fcr = fcr_df[order(abs(fcr_df$resdls), decreasing = T), 
                 c('Ta', 'group', 'resdls', 'Control')] %>% unique() %>% 
  head(10)

res_ind_fcr = fcr_rsdls[order(fcr_rsdls$resdls_abs_median, decreasing = T), 
                        c('Ta', 'resdls_abs_median', 'Control')] %>% unique() %>% 
  head(10)

dir.create('output/modelo_mixto_fcr/')

write.csv(x = res_fcr, 
          'output/modelo_mixto_fcr/top10_residual_fcr_mezcla_jp.csv')
write.csv(res_ind_fcr,
          'output/modelo_mixto_fcr/top10_residual_fcr_individual_jp.csv')

