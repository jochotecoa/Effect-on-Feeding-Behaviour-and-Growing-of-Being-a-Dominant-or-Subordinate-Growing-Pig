source('r/load_data.R')


consumo_medio = consumo_pesos %>% 
  group_by(Ta, Control, group) %>% 
  summarise(Co_mean = mean(Co),
            Ta, Pe_app = min(Pe_app), Control, group) %>% 
  unique() %>% 
  group_by(Ta) %>% 
  mutate(Pe_i = min(Pe_app)) %>% 
  filter(Ta != "M144@2022-03-24") # Sólo tiene consumo a partir de finales de 
#                                   julio

categorias = consumo_medio %>% 
  select(-Co_mean, -Pe_app) %>% 
  unique() %>% 
  filter(group != 'initial')

diff_consumo = consumo_medio %>% 
  group_by(Ta) %>% 
  summarise(cons_mean_diff = diff(Co_mean)) %>% 
  cbind(categorias)

all(diff_consumo$Ta...1 == diff_consumo$Ta...3) %>% stopifnot()

diff_consumo = diff_consumo %>% 
  select(-`Ta...3`) %>%
  rename(Ta = `Ta...1`)

# Load the lme4 package
library(lme4)

diff_consumo$Estres = as.numeric(!diff_consumo$Control)
diff_consumo$Control = as.numeric(diff_consumo$Control)
diff_consumo$Ta = diff_consumo$Ta %>% as.factor()
diff_consumo$group = as.numeric(diff_consumo$group == 'first')

# Fit a mixed-effects model with random intercepts for each Subject
# model <- lmer(cons_mean_diff ~ Control*group + (Control | Ta), data = diff_consumo)

model <- lmer(Co_mean ~ Control*group + Pe_i + (Control | Ta), data = consumo_medio)
# Print the model summary
summary(model)

consumo_medio$pred = predict(model, consumo_medio)
consumo_medio$rsdl = consumo_medio$Co_mean - consumo_medio$pred
total_rsdl = consumo_medio %>% group_by(Ta) %>% 
  summarise(rsdl = mean(abs(rsdl)), Control)

consumo_medio$rsdl %>% density() %>% plot
total_rsdl$rsdl %>% density() %>% plot

res_cons_mod_jp = consumo_medio[order(abs(consumo_medio$rsdl), decreasing = T), 
                         c('Ta', 'group', 'rsdl', 'Control')] %>% unique() %>% 
  head(10)

res_ind_mod_jp = total_rsdl[order(total_rsdl$rsdl, decreasing = T), 
                            c('Ta', 'rsdl', 'Control')] %>% unique() %>% 
  head(10)

# dir.create('output/modelo_mixto_diff_consumo/')

write.csv(x = res_cons_mod_jp, 
          'output/modelo_mixto_diff_consumo/top10_residual_consumo_mezcla_jp.csv')
write.csv(res_ind_mod_jp,
  'output/modelo_mixto_diff_consumo/top10_residual_consumo_individual_jp.csv')

total_cor = consumo_medio %>% group_by(Ta) %>% 
  summarise(cor = cor(Co_mean, pred))

consumo_medio$Ta[order(abs(consumo_medio$rsdl), decreasing = T)] %>% head
total_rsdl$Ta[order(total_rsdl$rsdl, decreasing = T)] %>% head

