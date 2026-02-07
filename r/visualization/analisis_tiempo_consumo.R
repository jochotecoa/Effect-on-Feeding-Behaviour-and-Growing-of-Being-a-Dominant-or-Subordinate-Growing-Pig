# setwd("/home/yuliaxis/GUTBRAIN")
#######################################################################
#######################################################################
library(RMySQL)
library(lme4)
library(doBy)
m<-dbDriver("MySQL")
con<-dbConnect(m,user='juanpablo',password='123',host='172.17.30.50',dbname='DB_CAP')

dbListTables(con)
source('r/load_data.R')

#####################
animales<-dbReadTable(con, "Historico_OCCUPATION_TIME") %>% 
  filter(T %in% !!unique(consumo_pesos$Ta)) 

animales$horas_tot = rowSums(animales[, grepl('H', colnames(animales))])
animales$n_visitas = rowSums(animales[, grepl('H', colnames(animales))] > 0)

consumo_pesos_3 = consumo_pesos %>% 
  select(Ta, Control, F, EDAD, Pe_app) %>% 
  rename(Fe = F) %>% 
  mutate(id = paste0(Ta, Fe))

# create a new data frame with the grouped sums
animales_grouped <- animales %>%
  mutate(H2_H5 = rowSums(select(., H2:H5)),
         H6_H9 = rowSums(select(., H6:H9)),
         H10_H13 = rowSums(select(., H10:H13)),
         H14_H17 = rowSums(select(., H14:H17)),
         H18_H21 = rowSums(select(., H18:H21)),
         H22_H1 = rowSums(select(., c(H22:H24, H1)))) %>% 
  mutate(H2_H5_perc = H2_H5*100/rowSums(select(., H1:H24)),
         H6_H9_perc = H6_H9*100/rowSums(select(., H1:H24)),
         H10_H13_perc = H10_H13*100/rowSums(select(., H1:H24)),
         H14_H17_perc = H14_H17*100/rowSums(select(., H1:H24)),
         H18_H21_perc = H18_H21*100/rowSums(select(., H1:H24)),
         H22_H1_perc = H22_H1*100/rowSums(select(., H1:H24))) %>% 
  rename(Ta = T,
         Fe = F) %>% 
  mutate(Fe = as.Date(Fe)) %>% 
  mutate(group = case_when(
    Fe < "2022-07-25" ~ "0",
    Fe >= "2022-07-25" & Fe < "2022-08-16" ~ "1",
    Fe >= "2022-08-16" ~ "2"
  )) %>% 
  mutate(id = paste0(Ta, Fe)) %>% 
  merge.data.frame(consumo_pesos_3, by = "id") %>% 
  select(!contains(".y")) %>% 
  rename_with(~ gsub(".x", "", .x)) %>% 
  mutate(EDAD = as.numeric(EDAD)) %>% 
  mutate(EDAD2 = EDAD^2, EDAD3 = EDAD^3)

nvis_lm = lm(n_visitas ~ EDAD*Pe_app + EDAD2*Pe_app + EDAD3*Pe_app, 
             filter(animales_grouped, Control))

H2_H5_perc_lm = lm(H2_H5_perc ~ EDAD*Pe_app + EDAD2*Pe_app + EDAD3*Pe_app, 
             filter(animales_grouped, Control))

# predict on new data
animales_grouped$residuals_nvis <- 
  animales_grouped$n_visitas - predict(nvis_lm, newdata = animales_grouped)

# summarize predicted values
animales_summ <- animales_grouped %>%
  group_by(Ta, group) %>%
  summarize(
    residuals_nvis_median = median(residuals_nvis),
    H2_H5 = mean(H2_H5),
    H6_H9 = mean(H6_H9),
    H10_H13 = mean(H10_H13),
    H14_H17 = mean(H14_H17),
    H18_H21 = mean(H18_H21),
    H22_H1 = mean(H22_H1),
    H2_H5_perc = mean(H2_H5_perc),
    H6_H9_perc = mean(H6_H9_perc),
    H10_H13_perc = mean(H10_H13_perc),
    H14_H17_perc = mean(H14_H17_perc),
    H18_H21_perc = mean(H18_H21_perc),
    H22_H1_perc = mean(H22_H1_perc),
    n_visitas = median(n_visitas)
    )

resdls_nvis_mean_total = animales_summ %>% group_by(Ta) %>% 
  summarise(total=sum(abs(residuals_nvis_median)))

animales_diff = animales_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(residuals_nvis_diff = diff(residuals_nvis_median),
            H2_H5_diff = diff(H2_H5),
            H6_H9_diff = diff(H6_H9),
            H10_H13_diff = diff(H10_H13),
            H14_H17_diff = diff(H14_H17),
            H18_H21_diff = diff(H18_H21),
            H22_H1_diff = diff(H22_H1),
            H2_H5_perc_diff = diff(H2_H5_perc),
            H6_H9_perc_diff = diff(H6_H9_perc),
            H10_H13_perc_diff = diff(H10_H13_perc),
            H14_H17_perc_diff = diff(H14_H17_perc),
            H18_H21_perc_diff = diff(H18_H21_perc),
            H22_H1_perc_diff = diff(H22_H1_perc),
            n_visitas_diff = diff(n_visitas)
  ) %>% 
  mutate(group = c('first', 'second')) %>% 
  merge.data.frame(select(animales_grouped, Ta, Control), 'Ta') %>% 
  unique.data.frame()

a = animales_grouped %>% filter(Ta == "M371@2022-03-24")
plot(a$Fe, a$H2_H5_perc, main = unique(a$Ta))
points(a$Fe, predict(nvis_lm, a), col = "red")
plot(animales_grouped$Fe, animales_grouped$n_visitas)
abline(v = as.Date("2022-07-25"))
abline(v = as.Date("2022-08-16"))

a = animales_grouped %>% filter(Ta == "F855@2022-03-24")
plot(a$Fe, a$n_visitas, main = unique(a$Ta))


a = animales_grouped %>% group_by(Fe) %>% summarise(n_visitas = mean(n_visitas))


# Pesos iniciales y finales -----------------------------------------------

pesos_i = pesos_approx %>% 
  filter(FP_apprx == "2022-05-25") %>% 
  rename(Pe_i = Pe_apprx) %>% 
  select(-FP_apprx)


pesos_f = pesos_approx %>% 
  filter(FP_apprx == "2022-10-06") %>% 
  rename(Pe_f = Pe_apprx) %>% 
  select(-FP_apprx)


# Ranking 2 a 6h ----------------------------------------------------------

dir.create("output/analisis_tiempo_consumo/")

animales_diff %>% 
  merge.data.frame(pesos_i, by.x = "Ta", by.y = "T") %>%
  merge.data.frame(pesos_f, by.x = "Ta", by.y = "T") %>% 
  mutate(diff_Pe = Pe_f - Pe_i) %>% 
  filter(group == "first", !Control) %>% 
  select(Ta, H2_H5_perc_diff, Pe_i, Pe_f, diff_Pe) %>% 
  arrange(desc(H2_H5_perc_diff)) %>% 
  saveRDS("output/analisis_tiempo_consumo/h2_h5_difference_in_percentage_feeding_time_first_mix.rds")

animales_diff %>% 
  merge.data.frame(pesos_i, by.x = "Ta", by.y = "T") %>%
  merge.data.frame(pesos_f, by.x = "Ta", by.y = "T") %>% 
  mutate(diff_Pe = Pe_f - Pe_i) %>% 
  filter(group == "second", !Control) %>% 
  select(Ta, H2_H5_perc_diff, Pe_i, Pe_f, diff_Pe) %>% 
  arrange(desc(H2_H5_perc_diff)) %>% 
  saveRDS("output/analisis_tiempo_consumo/h2_h5_difference_in_percentage_feeding_time_second_mix.rds")

animales_diff %>% 
  merge.data.frame(pesos_i, by.x = "Ta", by.y = "T") %>%
  merge.data.frame(pesos_f, by.x = "Ta", by.y = "T") %>% 
  mutate(diff_Pe = Pe_f - Pe_i) %>% 
  filter(!Control) %>% 
  group_by(Ta) %>% 
  summarise(total_h2_h5 = sum(abs(H2_H5_perc_diff)), Ta, Pe_i, Pe_f, diff_Pe) %>% 
  arrange(desc(total_h2_h5)) %>% 
  saveRDS("output/analisis_tiempo_consumo/h2_h5_difference_in_percentage_feeding_time_total_absolute.rds")

