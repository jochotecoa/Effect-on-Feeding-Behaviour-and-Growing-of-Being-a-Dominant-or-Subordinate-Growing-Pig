# Load data and libraries -------------------------------------------------
# setwd("/home/yuliaxis/GUTBRAIN")
library(RMySQL)
library(lme4)
library(doBy)
m<-dbDriver("MySQL")
con<-dbConnect(m,user='juanpablo',password='123',host='172.17.30.50',dbname='DB_CAP')



dbListTables(con)

# [1] "CHIPSs_SALVAR"                "EN_CONTROL_ANIMALES"          "EN_CONTROL_CONSUMO"          
# [4] "EN_CONTROL_EXP_LOTE"          "EN_CONTROL_FEED_RATE"         "EN_CONTROL_FI"               
# [7] "EN_CONTROL_OCCUPATION_TIME"   "EN_CONTROL_PESO"              "EN_CONTROL_VISITS"           
# [10] "EN_CONTROL_raw_VISITS"        "EXPORT_consumo"               "Historico_CONTROL_ANIMALES"  
# [13] "Historico_CONTROL_CONSUMO"    "Historico_CONTROL_PESO"       "Historico_CONTROL_raw_VISITS_without_modelling"
# [16] "Historico_EXP_LOTE"           "Historico_FEED_RATE"          "Historico_FI"                
# [19] "Historico_OCCUPATION_TIME"    "Historico_TATUAJE_CHIPS"      "Historico_VISITS"            
# [22] "IDs_SALVAR"                   "Import_CONTROL_ANIMALES"      "Import_CONTROL_CONSUMO"      
# [25] "Import_CONTROL_PESO"          "Import_by_hour"               "Import_visitas"              
# [28] "TATUAJE_CHIPS"               
source('r/load_data.R')

animales<-dbReadTable(con, "Historico_CONTROL_raw_VISITS") %>% 
  filter(T %in% !!unique(consumo_pesos$Ta)) %>% 
  as.data.frame() %>% 
  rename(Ta = `T`,
         FC = `F`) %>% 
  filter(Fr > 0)

consumo_pesos_2 = consumo_pesos %>% 
  mutate(id = paste0(Ta, `F`)) %>% 
  select(Pe_app, Ta, Control, EDAD, id) %>% 
  mutate(edad = as.numeric(EDAD))

pesos_approx = pesos_approx %>% 
  mutate(id = paste0(Ta, FP_apprx))

# N visitas ---------------------------------------------------------------


animales_nvis = animales %>% 
  mutate(FC = as.Date(FC)) %>% 
  group_by(Ta, FC) %>% 
  summarise(n_visitas = n()) %>% 
  mutate(id = paste0(Ta, FC)) %>% 
  merge.data.frame(consumo_pesos_2, by = "id", all.x = T) %>% 
  unique.data.frame() %>% 
  mutate(group = case_when(
    FC < "2022-07-25" ~ "0",
    FC >= "2022-07-25" & FC < "2022-08-16" ~ "1",
    FC >= "2022-08-16" ~ "2"
  )) %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  select(-c(edad, Pe_app)) %>% 
  merge.data.frame(pesos_approx, "id", all.x = T) %>% 
  mutate(edad2 = edad^2, edad3 = edad^3) %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  filter(FC < "2022-10-07") %>% 
  group_by(Ta) %>% 
  mutate(Control = Control[!is.na(Control)][1]) %>%
  ungroup()
  
animales_nvis_summ <- animales_nvis %>%
  group_by(Ta, group) %>%
  summarize(
    n_visitas_median = median(n_visitas),
  )

dir.create('output/Historico_CONTROL_raw_VISITS_without_modelling')

animales_nvis_summ %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds')

resdls_nvis_mean_total = animales_nvis %>% group_by(Ta) %>% 
  summarise(residuals_mean=mean(n_visitas))
resdls_nvis_mean_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_mean_residuals.rds')

resdls_nvis_median_total = animales_nvis %>% group_by(Ta, Control) %>% 
  summarise(residuals_median=median(n_visitas))
resdls_nvis_median_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_tattoo.rds')


resdls_nvis_median_abs_total = animales_nvis %>% group_by(Ta) %>% 
  summarise(total= mean(abs(n_visitas))) 
resdls_nvis_median_abs_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/nvisitas_consumo_median_group_abs_sum_residuals.rds')


animales_nvis_diff = animales_nvis_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(n_visitas_diff = diff(n_visitas_median),
  ) %>% 
  mutate(group = c('first', 'second')) %>% 
  merge.data.frame(select(animales_nvis, Ta, Control), 'Ta') %>% 
  unique.data.frame() %>% 
  na.omit()


a = animales_nvis %>% filter(Ta == "F831@2022-03-24")
plot(as.Date(a$FC), a$n_visitas, ylim = c(0, 15), 
       main = unique(a$Ta))
points(as.Date(a$FC), predict(nvis_lm, a), col = "red")
# plot(animales_grouped$Fe, animales_grouped$n_visitas)
abline(v = as.Date("2022-07-25"))
abline(v = as.Date("2022-08-16"))


animales_nvis_diff %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_diff_mix_residuals.rds')

# Tiempo comida -----------------------------------------------------------

animales_ti = animales %>% 
  mutate(FC = as.Date(FC)) %>% 
  group_by(Ta, FC) %>% 
  summarise(Ti = sum(Ti)) %>% 
  mutate(id = paste0(Ta, FC)) %>% 
  merge.data.frame(consumo_pesos_2, by = "id", all.x = T) %>% 
  unique.data.frame() %>% 
  mutate(group = case_when(
    FC < "2022-07-25" ~ "0",
    FC >= "2022-07-25" & FC < "2022-08-16" ~ "1",
    FC >= "2022-08-16" ~ "2"
  )) %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  select(-c(edad, Pe_app)) %>% 
  merge.data.frame(pesos_approx, "id", all.x = T) %>% 
  mutate(edad2 = edad^2, edad3 = edad^3) %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  filter(FC < "2022-10-07") %>% 
  group_by(Ta) %>% 
  mutate(Control = Control[!is.na(Control)][1]) %>%
  ungroup()

resdls_ti_mean_total = animales_ti %>% group_by(Ta) %>% 
  summarise(total=mean(Ti))

animales_ti_summ_by_ta <- animales_ti %>%
  group_by(Ta, Control) %>%
  summarize(
    Ti_median = median(Ti),
  )
animales_ti_summ_by_ta %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_tattoo.rds')

animales_ti_summ <- animales_ti %>%
  group_by(Ta, group) %>%
  summarize(
    Ti_median = median(Ti),
  )

animales_ti_summ %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds')

resdls_ti_median_total = animales_ti_summ %>% group_by(Ta) %>% 
  summarise(total=sum(abs(Ti_median)))



animales_ti_diff = animales_ti_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(Ti_diff = diff(Ti_median),
  ) %>% 
  mutate(group = c('first', 'second')) %>% 
  merge.data.frame(select(animales_ti, Ta, Control), 'Ta') %>% 
  unique.data.frame() %>% 
  na.omit()

a = animales_ti %>% filter(Ta == "F837@2022-03-22")
plot(as.Date(a$FC), a$Ti, main = unique(a$Ta))
points(as.Date(a$FC), predict(ti_lm, a), col = "red")
# plot(animales_grouped$Fe, animales_grouped$n_visitas)
abline(v = as.Date("2022-07-25"))
abline(v = as.Date("2022-08-16"))


resdls_ti_mean_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_mean_residuals.rds')
resdls_ti_median_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_group_abs_sum_residuals.rds')
animales_ti_diff %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_diff_mix_residuals.rds')


# Velocidad consumo -------------------------------------------------------


animales_Fr = animales %>% 
  mutate(FC = as.Date(FC)) %>% 
  group_by(Ta, FC) %>% 
  summarise(Fr = mean(Fr)) %>% 
  mutate(id = paste0(Ta, FC)) %>% 
  merge.data.frame(consumo_pesos_2, by = "id", all.x = T) %>% 
  unique.data.frame() %>% 
  mutate(group = case_when(
    FC < "2022-07-25" ~ "0",
    FC >= "2022-07-25" & FC < "2022-08-16" ~ "1",
    FC >= "2022-08-16" ~ "2"
  )) %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  select(-c(edad, Pe_app)) %>% 
  merge.data.frame(pesos_approx, "id", all.x = T) %>% 
  mutate(edad2 = edad^2, edad3 = edad^3) %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  filter(FC < "2022-10-07") %>% 
  group_by(Ta) %>% 
  mutate(Control = Control[!is.na(Control)][1]) %>%
  ungroup()

resdls_Fr_mean_total = animales_Fr %>% group_by(Ta) %>% 
  summarise(total=mean(Fr))

resdls_Fr_mean_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_mean_residuals.rds')

animales_Fr_summ_by_ta <- animales_Fr %>%
  group_by(Ta, Control) %>%
  summarize(
    Fr_median = median(Fr),
  )
animales_Fr_summ_by_ta %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_tattoo.rds')

animales_Fr_summ <- animales_Fr %>%
  group_by(Ta, group) %>%
  summarize(
    Fr_median = median(Fr),
  )

animales_Fr_summ %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_residuals_by_mixgroup.rds')


resdls_Fr_median_total = animales_Fr_summ %>% group_by(Ta) %>% 
  summarise(total=sum(abs(Fr_median)))

resdls_Fr_median_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_group_abs_sum_residuals.rds')


animales_Fr_diff = animales_Fr_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(Fr_diff = diff(Fr_median),
  ) %>% 
  mutate(group = c('first', 'second')) %>% 
  merge.data.frame(select(animales_Fr, Ta, Control), 'Ta') %>% 
  unique.data.frame() %>% 
  na.omit()

a = animales_Fr %>% filter(Ta == "M644@2022-03-24")
plot(as.Date(a$FC), a$Fr, main = unique(a$Ta))
points(as.Date(a$FC), predict(Fr_lm, a), col = "red")
# plot(animales_grouped$Fe, animales_grouped$n_visitas)
abline(v = as.Date("2022-07-25"))
abline(v = as.Date("2022-08-16"))

animales_Fr_diff %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/velocidad_consumo_median_diff_mix_residuals.rds')



# Duració mediana de menjada ----------------------------------------------


animales_median_Ti = animales %>% 
  mutate(FC = as.Date(FC)) %>% 
  group_by(Ta, FC) %>% 
  summarise(median_Ti = median(Ti)) %>% 
  mutate(id = paste0(Ta, FC)) %>% 
  merge.data.frame(consumo_pesos_2, by = "id", all.x = T) %>% 
  unique.data.frame() %>% 
  mutate(group = case_when(
    FC < "2022-07-25" ~ "0",
    FC >= "2022-07-25" & FC < "2022-08-16" ~ "1",
    FC >= "2022-08-16" ~ "2"
  )) %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  select(-c(edad, Pe_app)) %>% 
  merge.data.frame(pesos_approx, "id", all.x = T) %>% 
  mutate(edad2 = edad^2, edad3 = edad^3) %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  filter(FC < "2022-10-07") %>% 
  group_by(Ta) %>% 
  mutate(Control = Control[!is.na(Control)][1]) %>%
  ungroup()

# median_Ti_lm = lm(median_Ti ~ edad*Pe_app + edad2*Pe_app + edad3*Pe_app, 
#            filter(animales_median_Ti, Control))
# 
# animales_median_Ti$median_Ti <- 
#   animales_median_Ti$median_Ti - predict(median_Ti_lm, newdata = animales_median_Ti)

median_Ti_mean_total = animales_median_Ti %>% group_by(Ta) %>% 
  summarise(total=mean(median_Ti))

animales_median_Ti_summ_by_ta <- animales_median_Ti %>%
  group_by(Ta, Control) %>%
  summarize(
    median_Ti_median = median(median_Ti),
  )
animales_median_Ti_summ_by_ta %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_mediana_diaria_consumo_median_by_tattoo.rds')

animales_median_Ti_summ <- animales_median_Ti %>%
  group_by(Ta, group) %>%
  summarize(
    median_Ti_median = median(median_Ti),
  )

animales_median_Ti_summ %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_mediana_diaria_consumo_median_by_mixgroup.rds')

median_Ti_median_total = animales_median_Ti_summ %>% group_by(Ta) %>% 
  summarise(total=sum(abs(median_Ti_median)))



animales_median_Ti_diff = animales_median_Ti_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(median_Ti_diff = diff(median_Ti_median),
  ) %>% 
  mutate(group = c('first', 'second')) %>% 
  merge.data.frame(select(animales_median_Ti, Ta, Control), 'Ta') %>% 
  unique.data.frame() %>% 
  na.omit()

a = animales_median_Ti %>% filter(Ta == "M415@2022-03-24")
plot(as.Date(a$FC), a$median_Ti, main = unique(a$Ta))
# plot(animales_grouped$Fe, animales_grouped$n_visitas)
abline(v = as.Date("2022-07-25"))
abline(v = as.Date("2022-08-16"))


median_Ti_mean_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_mediana_diaria_consumo_mean_residuals.rds')
median_Ti_median_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_mediana_diaria_consumo_median_group_abs_sum_residuals.rds')
animales_median_Ti_diff %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_mediana_diaria_consumo_median_diff_mix_residuals.rds')


