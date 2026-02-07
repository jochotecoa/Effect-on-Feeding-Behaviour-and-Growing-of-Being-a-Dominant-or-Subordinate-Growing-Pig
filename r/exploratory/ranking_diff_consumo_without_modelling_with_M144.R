source('r/load_data.R')

# Construir modelo --------------------------------------------------------


consumo_pesos_2 = consumo_pesos %>% 
  mutate(edad = as.numeric(EDAD)) %>% 
  select(Co, Ta, Control, group, edad, Pe_app) %>% 
  mutate(edad2 = edad^2, edad3 = edad^3, 
         edad1_2 = edad^(1/2), edad1_3 = edad^(1/3),
         sex = as.numeric(grepl('M', Ta))) %>% 
  unique() 

# [1] "Co"      "Ta"      "Control" "group"   "edad"    "Pe_i"   
# modelo_lm_control = lm(Co ~ Pe_app + edad*Pe_app, 
#                        filter(consumo_pesos_2, Control))
# modelo_lm_control %>% summary
# 
# Calcular residuales -----------------------------------------------------


consumo_pesos$edad = consumo_pesos$EDAD %>% as.numeric()
# consumo_pesos$pred = predict(modelo_lm_control, consumo_pesos)
# consumo_pesos$Co = consumo_pesos$Co - consumo_pesos$pred

consumo_mediana = consumo_pesos %>% 
  group_by(Ta) %>% 
  summarise(Co_median = median(Co, na.rm = T),
            Ta, Control) %>% 
  unique()


consumo_mediano_by_group = consumo_pesos %>% 
  group_by(Ta, Control, group) %>% 
  summarise(Co_median = median(Co),
            Ta, Pe_app = min(Pe_app), Control, group) %>% 
  unique() 
categorias = consumo_mediano_by_group %>% 
  select(-Co_median, -Pe_app) %>% 
  unique() %>% 
  filter(group != 'initial')


# Calcular ranquing -------------------------------------------------------



diff_consumo = consumo_mediano_by_group %>% 
  group_by(Ta) %>% 
  summarise(cons_median_diff = diff(Co_median)) %>% 
  cbind(categorias)

all(diff_consumo$Ta...1 == diff_consumo$Ta...3) %>% stopifnot()

diff_consumo = diff_consumo %>% 
  select(-`Ta...3`) %>%
  rename(Ta = `Ta...1`)

diff_consumo_total = diff_consumo %>% 
  group_by(Ta) %>% 
  summarise(diff_cons = sum(abs(cons_median_diff)), Control) %>% 
  ungroup() %>% 
  unique.data.frame() %>% 
  mutate(ranking = rank(-diff_cons)) %>% 
  arrange(ranking) 



diff_consumo_ordered_first = 
  diff_consumo[order(diff_consumo$cons_median_diff), ] %>% 
  filter(group == 'first')

diff_consumo_ordered_second = 
  diff_consumo[order(diff_consumo$cons_median_diff), ] %>% 
  filter(group == 'second')


diff_consumo_total_ordered = 
  diff_consumo_total[order(diff_consumo_total$diff_cons), ]


# Plots -------------------------------------------------------------------



barplot(diff_consumo_ordered_first$cons_median_diff, 
        names.arg = gsub('@.*', '', diff_consumo_ordered_first$Ta), las=2)
barplot(diff_consumo_ordered_second$cons_median_diff, 
        names.arg = gsub('@.*', '', diff_consumo_ordered_second$Ta), las=2)
barplot(diff_consumo_total_ordered$diff_cons, 
        names.arg = gsub('@.*', '', diff_consumo_total_ordered$Ta), las=2)

a = filter(consumo_pesos_2, Ta == 'M644@2022-03-24')
# a$pred = predict(modelo_lm_control, a)
plot(a$edad, a$Co)
points(a$edad, a$pred, col = 2)

a = filter(consumo_pesos_2, Ta == 'F844@2022-03-20')
# a$pred = predict(modelo_lm_control, a)
plot(a$edad, a$Co)
points(a$edad, a$pred, col = 2)

pesos_i = pesos_approx %>% 
  filter(FP_apprx == "2022-05-25") %>% 
  rename(Pe_i = Pe_apprx) %>% 
  select(-FP_apprx)


pesos_f = pesos_approx %>% 
  filter(FP_apprx == "2022-10-06") %>% 
  rename(Pe_f = Pe_apprx) %>% 
  select(-FP_apprx)
  

# Save results ------------------------------------------------------------

dir.create("output/ranking_diff_consumo_without_modelling_with_M144")

diff_consumo_total %>% 
  merge.data.frame(pesos_i, by.x = "Ta", by.y = "Ta") %>%
  merge.data.frame(pesos_f, by.x = "Ta", by.y = "Ta") %>% 
  mutate(diff_Pe = Pe_f - Pe_i) %>% 
  filter(!Control) %>% 
  saveRDS("output/ranking_diff_consumo_without_modelling_with_M144/diff_consumo_total.rds")
diff_consumo_ordered_first %>% 
  merge.data.frame(pesos_i, by.x = "Ta", by.y = "Ta") %>%
  merge.data.frame(pesos_f, by.x = "Ta", by.y = "Ta") %>% 
  mutate(diff_Pe = Pe_f - Pe_i) %>% 
  filter(!Control) %>% 
  saveRDS("output/ranking_diff_consumo_without_modelling_with_M144/diff_consumo_ordered_first_mix.rds")
diff_consumo_ordered_second %>% 
  merge.data.frame(pesos_i, by.x = "Ta", by.y = "Ta") %>%
  merge.data.frame(pesos_f, by.x = "Ta", by.y = "Ta") %>% 
  mutate(diff_Pe = Pe_f - Pe_i) %>% 
  filter(!Control) %>% 
  saveRDS("output/ranking_diff_consumo_without_modelling_with_M144/diff_consumo_ordered_second_mix.rds")

consumo_mediana %>% 
  saveRDS("output/ranking_diff_consumo_without_modelling_with_M144/consumo_median_residuals_by_tattoo.rds")

consumo_mediano_by_group %>% 
  saveRDS("output/ranking_diff_consumo_without_modelling_with_M144/consumo_median_residuals_by_mixgroup.rds")

diff_consumo %>% 
  merge.data.frame(pesos_i, by.x = "Ta", by.y = "Ta") %>%
  merge.data.frame(pesos_f, by.x = "Ta", by.y = "Ta") %>% 
  mutate(diff_Pe = Pe_f - Pe_i) %>% 
  saveRDS("output/ranking_diff_consumo_without_modelling_with_M144/consumo_median_diff_mix_residuals.rds")

