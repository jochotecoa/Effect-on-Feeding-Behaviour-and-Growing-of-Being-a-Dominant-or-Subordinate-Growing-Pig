# Install and load the necessary library
install.packages("dplyr")
library(dplyr)

# pesos_Juan_script.R -----------------------------------------------------


# Assuming df is your dataframe
summary_df <- Pesos_Juan %>%
  group_by(Control) %>%
  summarize(
    Mean_X = mean(ADG, na.rm = TRUE),
    SD_X = sd(ADG, na.rm = TRUE)
  )

# View the summary dataframe
print(summary_df)

cons_perm_2 %>%
  filter(!is.na(FCR) & is.finite(FCR)) %>%
  t.test(FCR ~ Control, .) 

cons_perm_2 %>%
  filter(!is.na(FCR) & is.finite(FCR)) %>%
  group_by(Control) %>%
  summarize(
    Mean_X = mean(FCR, na.rm = TRUE),
    SD_X = sd(FCR, na.rm = TRUE)
  )


# ranking_diff_consumo_without_modelling ----------------------------------



# View the summary dataframe
print(summary_df)

consumo_pesos_2 %>%
  group_by(Control) %>%
  summarize(
    Mean_X = mean(Co, na.rm = TRUE),
    SD_X = sd(Co, na.rm = TRUE)
  )


# Historico_CONTROL_raw_VISITS_without_modelling.R ------------------------



animales_nvis %>%
  group_by(Control) %>%
  summarize(
    Mean_X = mean(n_visitas , na.rm = TRUE),
    SD_X = sd(n_visitas , na.rm = TRUE)
  )

animales_ti %>%
  group_by(Control) %>%
  summarize(
    Mean_X = mean(Ti , na.rm = TRUE),
    SD_X = sd(Ti , na.rm = TRUE)
  )

animales_Fr = animales %>% 
  mutate(FC = as.Date(FC)) %>% 
  group_by(Ta, FC) %>% 
  summarise(Fr = mean(Fr)) %>% 
  mutate(id = paste0(Ta, FC)) %>% 
  merge.data.frame(consumo_pesos_2, by = "id", all.x = T) %>% 
  unique.data.frame()

lm(animales_Fr$Fr ~ animales_Fr$Control) %>% summary()

animales_Fr %>%
  group_by(Control) %>%
  summarize(
    Mean_X = mean(Fr , na.rm = TRUE),
    SD_X = sd(Fr , na.rm = TRUE)
  )

animales_median_Ti = animales %>% 
  mutate(FC = as.Date(FC)) %>% 
  group_by(Ta, FC) %>% 
  summarise(median_Ti = median(Ti)) %>% 
  mutate(id = paste0(Ta, FC)) %>% 
  merge.data.frame(consumo_pesos_2, by = "id", all.x = T) %>% 
  unique.data.frame()

lm(animales_median_Ti$median_Ti ~ animales_median_Ti$Control) %>% summary()

animales_median_Ti %>%
  group_by(Control) %>%
  summarize(
    Mean_X = mean(median_Ti , na.rm = TRUE),
    SD_X = sd(median_Ti , na.rm = TRUE)
  )


# Historico_OCCUPATION_TIME -----------------------------------------------

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
animales_horas<-dbReadTable(con, "Historico_OCCUPATION_TIME") %>% 
  filter(T %in% !!unique(consumo_pesos$Ta)) 

consumo_pesos_3 = consumo_pesos %>% 
  select(Ta, Control, F, EDAD, Pe_app) %>% 
  rename(Fe = F) %>% 
  mutate(id = paste0(Ta, Fe))

# create a new data frame with the grouped sums
animales_horas_grouped <- animales_horas %>%
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
  mutate(id = paste0(Ta, Fe)) %>% 
  merge.data.frame(consumo_pesos_3, by = "id", all.x = T) %>% 
  unique.data.frame()
library(dplyr)

a = tibble()

for (variable in colnames(animales_horas_grouped) %>% subset(., grepl("H.*_", .))) {
  print(variable)
  lm(as.formula(paste0(variable, " ~ Control")), animales_horas_grouped) %>%
    summary() %>% print()
  
  colnames(animales_horas_grouped)[2:3] = 
    paste0(colnames(animales_horas_grouped)[2:3], "_", variable)
  animales_horas_grouped %>%
    group_by(Control) %>%
    summarize(
      Mean_X = mean(!!rlang::sym(variable), na.rm = TRUE),
      SD_X = sd(!!rlang::sym(variable), na.rm = TRUE)
    ) %>% print()
  
  
}

