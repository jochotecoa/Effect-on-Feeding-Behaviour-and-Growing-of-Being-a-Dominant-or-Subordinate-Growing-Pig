# install.packages('dplyr')
library(dplyr)
library(ggplot2)
library(progress)
library(ggstatsplot)
library(ggpubr)

# Load data ---------------------------------------------------------------


is.odd <- function(x) x %% 2 != 0

Registros_animales_Juan <- 
  read.delim("Registros_animales_Juan.txt", 
             header=FALSE)
limpiar_nombres <- function(x) {
  x[1, 2:ncol(x)] = 
    x[1, 1:ncol(x) - 1]
  
  x = x[, -1]
  colnames(x) = x[1, ]
  x = x[-1, ]
  return(x)
}


Pesos_Juan <- read.delim("Pesos_Juan.txt", header=FALSE)


Registros_animales_Juan = Registros_animales_Juan %>% 
  limpiar_nombres()

Pesos_Juan = Pesos_Juan %>% 
  limpiar_nombres()

Output_consumo_from_script_comparar_pesos <- 
  read.delim("Output_consumo_from_script_comparar_pesos.txt", 
             header=FALSE) %>% 
  limpiar_nombres()

Output_consumo_from_script_comparar_pesos$tatuaje = 
  Output_consumo_from_script_comparar_pesos$T

Output_consumo_from_script_comparar_pesos$fecha_consumo = 
  Output_consumo_from_script_comparar_pesos$F

cons_pes = Output_consumo_from_script_comparar_pesos

cons_pes = merge.data.frame(cons_pes, Pesos_Juan, by = 'T')


# Plot Pe vs FP -----------------------------------------------------------


Pesos_Juan = Pesos_Juan[order(Pesos_Juan$FP), ]
Pesos_Juan = Pesos_Juan %>% 
  mutate(Ta = T,
         C = as.numeric(C),
         Control = ((C %% 2)!= 0),
         Pe = as.numeric(Pe),
         FP = as.Date(FP),
         diff_dies = NA,
         diff_peso = NA)


ggplot(data = Pesos_Juan, aes(x = FP, y = as.numeric(Pe), group = Ta)) + 
  geom_line()

ggplot(data = Pesos_Juan, aes(x = FP, y = Pe, group = Ta, col = Control)) + 
  geom_line()


# Differencia Días Pesado -------------------------------------------------


fechas_peso = Pesos_Juan$FP %>% unique %>% as.Date()

llista_dies = numeric()

for (i in 1:(length(fechas_peso)-1)) {
  dies = fechas_peso[i+1] - fechas_peso[i]
  dies = dies %>% as.numeric()
  
  Pesos_Juan[Pesos_Juan$FP == fechas_peso[i+1], "diff_dies"] = dies
  
}


# Differencia peso --------------------------------------------------------


library(tibble)

tatus = Pesos_Juan$Ta %>% unique
for (ta in tatus) {
  for (i in 1:(length(fechas_peso)-1)) {
    fecha_final = fechas_peso[i+1]
    peso_final = Pesos_Juan %>% 
      rownames_to_column() %>% 
      filter(Ta == !!ta,
             FP == !!fecha_final) %>% 
      select(Pe, rowname)
    pos_f = peso_final %>% 
      select(rowname) %>% 
      as.character()
    
    peso_final = peso_final %>% 
      select(Pe) %>% 
      as.numeric()
    
    fecha_inicial = fechas_peso[i]
    peso_inicial = Pesos_Juan %>% 
      filter(Ta == !!ta,
             FP == !!fecha_inicial) %>% 
      select(Pe) %>% 
      as.numeric()
    
    diff_peso = peso_final - peso_inicial
    
    Pesos_Juan[pos_f, "diff_peso"] = diff_peso
    
  }
  
}


# Average Daily Gain ------------------------------------------------------

Pesos_Juan = Pesos_Juan %>% 
  mutate(ADG = diff_peso/diff_dies)


# Feed Conversion Ratio ---------------------------------------------------

cons_perm = data.frame()

for (pig in tatus) {
  for (i in 1:(length(fechas_peso) - 1)) {
    fecha_inicial = fechas_peso[i]
    fecha_final = fechas_peso[i + 1]
    adg = Pesos_Juan %>% 
      filter(Ta == pig,
             FP == fecha_final) %>% 
      select(ADG) %>% 
      as.numeric()

    cons_temp = Output_consumo_from_script_comparar_pesos %>% 
      mutate(fecha_consumo = as.Date(fecha_consumo)) %>% 
      filter(fecha_consumo >= fecha_inicial,
             fecha_consumo < fecha_final, 
             tatuaje == pig) %>% 
      mutate(ADG = adg)
    
    cons_perm = rbind.data.frame(cons_perm, cons_temp)
      }
}

# Now I need to get each week's intake per pig, sum it, and divide it by the AVG

cons_perm_2 = cons_perm %>% 
    filter(fecha_consumo >= min(!!fechas_peso),
           fecha_consumo < max(!!fechas_peso)) %>% 
  mutate(Co = as.numeric(Co)) %>% 
  mutate(FCR = (Co/1000)/ADG)
intake_days = cons_perm_2$fecha_consumo %>% unique()
first_day = max(min(fechas_peso), min(intake_days))
weeks_total = as.numeric(max(fechas_peso) - first_day)/7
tatuajes = cons_perm_2$tatuaje %>% unique()

cons_perm_2$Control = cons_perm_2$C %>% as.numeric() %>% is.odd()
ggplot(data = cons_perm_2, aes(x = fecha_consumo, y = FCR, group = tatuaje, col = Control)) + 
  geom_line()



fcr_semanas = data.frame()
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = length(tatuajes), clear = FALSE, width= 60)


for (tatuaj in tatuajes) {
  for (week in 1:weeks_total) {
    last_day = first_day + 7
    fcr_semana = cons_perm_2 %>% 
      filter(tatuaje == !! tatuaj,
             fecha_consumo >= !!first_day,
             fecha_consumo < !!last_day) %>% 
      select(FCR) %>% 
      unlist() 
    
    if (week > 2 & week < 21) {
      if (length(fcr_semana) < 7) {
        warning(tatuaj, "__", week, "__", length(fcr_semana))
      }
    }
    if (anyNA(fcr_semana)) {fcr_semana = NA}
    if (sum(fcr_semana) == 0) {fcr_semana = NA}
    
    fcr_semana = fcr_semana %>% sum()
    
    fcr_df = data.frame(tatuaje = tatuaj, week = week, FCR = fcr_semana)
    fcr_semanas = rbind.data.frame(fcr_semanas, fcr_df)
    first_day = last_day
    
  }
  pb$tick()
}



# Weekly ADG --------------------------------------------------------------

cons_perm_2$week = ceiling(((cons_perm_2$fecha_consumo - min(cons_perm_2$fecha_consumo)) + 1)/7) %>% 
  as.numeric()

# cons_perm_2 %>% 
#   group_by(tatuaje, week) %>% 
#   summarise(fcr_median_week = median(FCR)) %>% 
#   head()
# ggplot(data = ., aes(x = week, y = fcr_median_week, group = tatuaje, col = 
# Control)) + 
#   geom_boxplot()



cons_perm_2 %>% 
  mutate(week_control = paste0(week, "_", Control)) %>% 
  mutate(week_control = as.factor(week_control),
         Control = as.factor(Control)) %>% 
ggplot(data = ., aes(y = FCR, group = week_control, 
                     col = Control, x = week)) + 
  geom_boxplot() +
  geom_vline(xintercept = seq(1.5, max(cons_perm_2$week) - 0.5), color = "red", 
             linetype = "dashed") +
  ggpubr::geom_signif(
    annotations = c('*', '*', '**'),
    y_position = rep(10, 3), xmin = c(0.75, 3.25, 14.25), 
    xmax = c(1.75, 4.25, 15.25), color = "black"
  ) +
  ggpubr::geom_signif(
    annotations = c('**'),
    y_position = 6, xmin = c(3.75), xmax = c(4.25), color = "black"
  )


# The Shapiro-Wilk test returns a significant result but the q-q plot looks 
# normal, it means that the sample may not be perfectly normally distributed, 
# but it is close enough to be considered normal for most purposes. In such  
# cases, it may be appropriate to use statistical methods that assume normality, 
# such as t-tests or ANOVA. 


cons_perm_2$Co %>% sample(5000) %>% shapiro.test()
qqnorm(cons_perm_2$Co); qqline(cons_perm_2$Co, col = 2)

fcr_weekcontrol = cons_perm_2 %>% 
  mutate(week_control = paste0(week, "_", Control)) %>% 
  filter(!is.infinite(FCR)) %>% 
  ggbetweenstats(data = ., y = FCR, x = week_control) %>% 
  extract_stats()

fcr_weekcontrol_prws = fcr_weekcontrol$pairwise_comparisons_data %>% 
  as.data.frame()
fcr_weekcontrol_prws$group1_1 = fcr_weekcontrol_prws$group1 %>% 
  strsplit(split = '_') %>%  lapply(`[[`, 1) %>% as.numeric()
fcr_weekcontrol_prws$group1_2 = fcr_weekcontrol_prws$group1 %>% 
  strsplit(split = '_') %>%  lapply(`[[`, 2) %>% as.logical()
fcr_weekcontrol_prws$group2_1 = fcr_weekcontrol_prws$group2 %>% 
  strsplit(split = '_') %>%  lapply(`[[`, 1) %>% as.numeric()
fcr_weekcontrol_prws$group2_2 = fcr_weekcontrol_prws$group2 %>% 
  strsplit(split = '_') %>%  lapply(`[[`, 2) %>% as.logical()


# Only the 4th week shows a significant difference across conditions

fcr_weekcontrol_prws_week = fcr_weekcontrol_prws %>% 
  filter(group1_1 == group2_1,
         group1_2 != group2_2,
         p.value < 0.05)

fcr_weekcontrol_prws_week = fcr_weekcontrol_prws %>% 
  filter(group1_1 == group2_1 - 1,
         group1_2 == group2_2,
         p.value < 0.05)



# Linear regression -------------------------------------------------------

fcr_week_lm = cons_perm_2 %>% 
  filter(!is.infinite(FCR)) %>% 
  lm(FCR ~ week, .) 

fcr_week_lm_con = cons_perm_2 %>% 
  filter(!is.infinite(FCR), 
         Control) %>% 
  lm(FCR ~ week, .) 

fcr_week_lm_est = cons_perm_2 %>% 
  filter(!is.infinite(FCR),
         !Control) %>% 
  lm(FCR ~ week, .) 


plot(cons_perm_2$week, cons_perm_2$FCR, main = 'R^2 ~ 0.35')
# abline(fcr_week_lm, col = 2)
abline(fcr_week_lm_con, col = 4)
abline(fcr_week_lm_est, col = 2)




a = lm(Pe ~ ., Pesos_Juan[, sapply(Pesos_Juan, is.numeric)])
