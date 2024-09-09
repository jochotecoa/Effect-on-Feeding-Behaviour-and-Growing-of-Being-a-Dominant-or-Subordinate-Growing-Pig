library(tidyverse)

limpiar_nombres <- function(x) {
  x[1, 2:ncol(x)] = 
    x[1, 1:ncol(x) - 1]
  
  x = x[, -1]
  colnames(x) = x[1, ]
  x = x[-1, ]
  return(x)
}

is.odd <- function(x) x %% 2 != 0

Registros_animales_Juan <- 
  read.delim("Registros_animales_Juan.txt", 
             header=FALSE) %>% 
  limpiar_nombres()

Pesos_Juan <- read.delim("Pesos_Juan.txt", header=FALSE) %>% 
  limpiar_nombres() %>% 
  mutate(FP = as.Date(FP, "%Y-%m-%d"))

Output_consumo_from_script_comparar_pesos <- 
  read.delim("Output_consumo_from_script_comparar_pesos.txt", 
             header=FALSE) %>% 
  limpiar_nombres()

pesos_approx = Pesos_Juan %>% 
  group_by(T) %>%
  summarise(FP_apprx = approx(FP, Pe, seq(min(FP), max(FP), by = "day"))$x,
            Pe_apprx = approx(FP, Pe, seq(min(FP), max(FP), by = "day"))$y) %>% 
  mutate(FP_apprx = as.Date(FP_apprx, origin = "1970-01-01")) 


consumo_pesos = merge.data.frame(pesos_approx, 
                                 Output_consumo_from_script_comparar_pesos, 
                                 "T") %>% 
  mutate(F = as.Date(F)) %>% 
  filter(F == FP_apprx) %>% 
  select(!contains(".y")) %>% 
  rename_with( ~ gsub(".x", "", .x)) %>% 
  rename(Ta = `T`) %>% 
  mutate(Co = as.numeric(Co)) %>% 
  mutate(cons_esc = Co/(Pe_app*1000),
         Control = is.odd(as.numeric(C))) %>% 
  mutate(group = case_when(
    F < "2022-07-25" ~ "initial",
    F >= "2022-07-25" & F < "2022-08-16" ~ "first",
    F >= "2022-08-16" ~ "second"
  )) %>% 
  mutate(group = factor(group, levels = c("initial", "first", "second")))

pesos_approx$Fe_ncmnt = pesos_approx$T %>% strsplit("@") %>% sapply('[[', 2) %>% 
  as.Date()
pesos_approx = pesos_approx %>% 
  rename(Ta = `T`) %>% 
  mutate(FP_apprx = as.Date(FP_apprx)) %>% 
  mutate(edad = as.numeric(FP_apprx - Fe_ncmnt))
  
  