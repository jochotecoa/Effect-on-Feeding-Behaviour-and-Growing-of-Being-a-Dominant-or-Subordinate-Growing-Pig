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
# [13] "Historico_CONTROL_CONSUMO"    "Historico_CONTROL_PESO"       "Historico_CONTROL_raw_VISITS"
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
  


nvis_lm = lm(n_visitas ~ edad*Pe_app + edad2*Pe_app + edad3*Pe_app, 
             filter(animales_nvis, Control))

animales_nvis$residuals_nvis <- 
  animales_nvis$n_visitas - predict(nvis_lm, newdata = animales_nvis)

animales_nvis_summ <- animales_nvis %>%
  group_by(Ta, group) %>%
  summarize(
    residuals_nvis_median = median(residuals_nvis),
  )

animales_nvis_summ %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/n_visitas_median_residuals_by_mixgroup.rds')

resdls_nvis_mean_total = animales_nvis %>% group_by(Ta) %>% 
  summarise(residuals_mean=mean(residuals_nvis))
resdls_nvis_mean_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/n_visitas_mean_residuals.rds')

resdls_nvis_median_total = animales_nvis %>% group_by(Ta, Control) %>% 
  summarise(residuals_median=median(residuals_nvis))
resdls_nvis_median_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/n_visitas_median_residuals_by_tattoo.rds')


resdls_nvis_median_abs_total = animales_nvis %>% group_by(Ta) %>% 
  summarise(total= mean(abs(residuals_nvis))) 
resdls_nvis_median_abs_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/nvisitas_consumo_median_group_abs_sum_residuals.rds')


animales_nvis_diff = animales_nvis_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(residuals_nvis_diff = diff(residuals_nvis_median),
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

dir.create('output/Historico_CONTROL_raw_VISITS')

animales_nvis_diff %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/n_visitas_median_diff_mix_residuals.rds')

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

ti_lm = lm(Ti ~ edad*Pe_app + edad2*Pe_app + edad3*Pe_app, 
             filter(animales_ti, Control))

animales_ti$residuals_ti <- 
  animales_ti$Ti - predict(ti_lm, newdata = animales_ti)

resdls_ti_mean_total = animales_ti %>% group_by(Ta) %>% 
  summarise(total=mean(residuals_ti))

animales_ti_summ_by_ta <- animales_ti %>%
  group_by(Ta, Control) %>%
  summarize(
    residuals_ti_median = median(residuals_ti),
  )
animales_ti_summ_by_ta %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_total_diaria_consumo_median_residuals_by_tattoo.rds')

animales_ti_summ <- animales_ti %>%
  group_by(Ta, group) %>%
  summarize(
    residuals_ti_median = median(residuals_ti),
  )

animales_ti_summ %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds')

resdls_ti_median_total = animales_ti_summ %>% group_by(Ta) %>% 
  summarise(total=sum(abs(residuals_ti_median)))



animales_ti_diff = animales_ti_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(residuals_ti_diff = diff(residuals_ti_median),
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
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_total_diaria_consumo_mean_residuals.rds')
resdls_ti_median_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_total_diaria_consumo_median_group_abs_sum_residuals.rds')
animales_ti_diff %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_total_diaria_consumo_median_diff_mix_residuals.rds')


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

Fr_lm = lm(Fr ~ edad*Pe_app + edad2*Pe_app + edad3*Pe_app, 
           filter(animales_Fr, Control))

animales_Fr$residuals_Fr <- 
  animales_Fr$Fr - predict(Fr_lm, newdata = animales_Fr)

resdls_Fr_mean_total = animales_Fr %>% group_by(Ta) %>% 
  summarise(total=mean(residuals_Fr))

resdls_Fr_mean_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/velocidad_consumo_mean_residuals.rds')

animales_Fr_summ_by_ta <- animales_Fr %>%
  group_by(Ta, Control) %>%
  summarize(
    residuals_Fr_median = median(residuals_Fr),
  )
animales_Fr_summ_by_ta %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/velocidad_consumo_median_residuals_by_tattoo.rds')

animales_Fr_summ <- animales_Fr %>%
  group_by(Ta, group) %>%
  summarize(
    residuals_Fr_median = median(residuals_Fr),
  )

animales_Fr_summ %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/velocidad_consumo_median_residuals_by_mixgroup.rds')


resdls_Fr_median_total = animales_Fr_summ %>% group_by(Ta) %>% 
  summarise(total=sum(abs(residuals_Fr_median)))

resdls_Fr_median_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/velocidad_consumo_median_group_abs_sum_residuals.rds')


animales_Fr_diff = animales_Fr_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(residuals_Fr_diff = diff(residuals_Fr_median),
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
  saveRDS('output/Historico_CONTROL_raw_VISITS/velocidad_consumo_median_diff_mix_residuals.rds')



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
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_tattoo.rds')

animales_median_Ti_summ <- animales_median_Ti %>%
  group_by(Ta, group) %>%
  summarize(
    median_Ti_median = median(median_Ti),
  )

animales_median_Ti_summ %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_by_mixgroup.rds')

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
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_mean_residuals.rds')
median_Ti_median_total %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_group_abs_sum_residuals.rds')
animales_median_Ti_diff %>% 
  saveRDS('output/Historico_CONTROL_raw_VISITS/duracion_mediana_diaria_consumo_median_diff_mix_residuals.rds')


