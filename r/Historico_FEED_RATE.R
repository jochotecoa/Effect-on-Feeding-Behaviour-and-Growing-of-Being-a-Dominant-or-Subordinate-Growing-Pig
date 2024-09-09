# setwd("/home/yuliaxis/GUTBRAIN")
#######################################################################
#######################################################################
library(RMySQL)
library(lme4)
library(doBy)

rowMeans <- function(mat) {
  apply(mat, 1, median)
}


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

#####################
animales<-dbReadTable(con, "Historico_FI") %>% 
  filter(T %in% !!unique(consumo_pesos$Ta)) %>% 
  as.data.frame() %>% 
  rename(Ta = `T`,
         FC = `F`) 

animales[animales < 0] = 0
  
consumo_pesos_2 = consumo_pesos %>% 
  mutate(id = paste0(Ta, `F`)) %>% 
  select(Pe_app, Ta, Control, EDAD, id) %>% 
  mutate(edad = as.numeric(EDAD))

pesos_approx = pesos_approx %>% 
  mutate(id = paste0(Ta, FP_apprx))


consumo_pesos_3 = consumo_pesos %>% 
  select(Ta, Control, F, EDAD, Pe_app) %>% 
  rename(Fe = F) %>% 
  mutate(id = paste0(Ta, Fe))

# create a new data frame with the grouped sums
animales_grouped <- animales %>%
  mutate(H2_H5 = rowMeans(select(., H2:H5)[. > 0,]),
         H6_H9 = rowMeans(select(., H6:H9)),
         H10_H13 = rowMeans(select(., H10:H13)),
         H14_H17 = rowMeans(select(., H14:H17)),
         H18_H21 = rowMeans(select(., H18:H21)),
         H22_H1 = rowMeans(select(., c(H22:H24, H1))),
         Fr_mean = rowMeans(select(., H1:H24))) %>% 
  mutate(H2_H5_ratio = H2_H5/Fr_mean,
         H6_H9_ratio = H6_H9/Fr_mean,
         H10_H13_ratio = H10_H13/Fr_mean,
         H14_H17_ratio = H14_H17/Fr_mean,
         H18_H21_ratio = H18_H21/Fr_mean,
         H22_H1_ratio = H22_H1/Fr_mean) %>% 
  rename(Fe = FC) %>% 
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

H2_H5_lm = lm(H2_H5 ~ EDAD*Pe_app + EDAD2*Pe_app + EDAD3*Pe_app, 
              filter(animales_grouped, Control))

# predict on new data
animales_grouped$residuals_H2_H5 <- 
  animales_grouped$H2_H5 - predict(H2_H5_lm, newdata = animales_grouped)


# summarize predicted values
animales_summ <- animales_grouped %>%
  group_by(Ta, group) %>%
  summarize(
    residuals_H2_H5 = median(residuals_H2_H5),
    H2_H5 = mean(H2_H5),
    H6_H9 = mean(H6_H9),
    H10_H13 = mean(H10_H13),
    H14_H17 = mean(H14_H17),
    H18_H21 = mean(H18_H21),
    H22_H1 = mean(H22_H1)
    )

animales_summ_total <- animales_grouped %>%
  group_by(Ta) %>%
  summarize(
    residuals_H2_H5 = mean(residuals_H2_H5),
    H2_H5 = mean(H2_H5),
    H6_H9 = mean(H6_H9),
    H10_H13 = mean(H10_H13),
    H14_H17 = mean(H14_H17),
    H18_H21 = mean(H18_H21),
    H22_H1 = mean(H22_H1)  )


animales_diff = animales_summ %>% 
  filter(Ta != "M144@2022-03-24") %>% 
  group_by(Ta) %>% 
  summarise(residuals_H2_H5_diff = diff(residuals_H2_H5),
            H2_H5_diff = diff(H2_H5),
            H6_H9_diff = diff(H6_H9),
            H10_H13_diff = diff(H10_H13),
            H14_H17_diff = diff(H14_H17),
            H18_H21_diff = diff(H18_H21),
            H22_H1_diff = diff(H22_H1)
  ) %>% 
  mutate(group = c('first', 'second')) %>% 
  merge.data.frame(select(animales_grouped, Ta, Control), 'Ta') %>% 
  unique.data.frame()

a = animales_grouped %>% filter(Ta == "M329@2022-03-24")
plot(a$Fe, a$H2_H5, main = unique(a$Ta))
boxplot(H6_H9 ~ group, a, main = unique(a$Ta))
points(a$Fe, predict(H2_H5_lm, a), col = "red")
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
