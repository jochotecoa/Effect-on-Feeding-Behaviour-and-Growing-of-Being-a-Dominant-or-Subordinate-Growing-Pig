library(tidyverse)

limpiar_nombres <- function(x) {
  x[1, 2:ncol(x)] = 
    x[1, 1:ncol(x) - 1]
  
  x = x[, -1]
  colnames(x) = x[1, ]
  x = x[-1, ]
  return(x)
}

weight_data = readRDS('output/load_data/pesos_approx.rds') %>% 
  mutate(ID = gsub("@.*", "", Ta)) 

ages = weight_data %>% 
  ungroup() %>% 
  mutate(ID_FP = paste(ID, FP_apprx, '_')) %>% 
  select(ID_FP, edad) %>% 
  unique()

Pesos_Juan <- read.delim("Pesos_Juan.txt", header=FALSE) %>% 
  limpiar_nombres() %>% 
  mutate(FP = as.Date(FP, "%Y-%m-%d"),
         ID = gsub("@.*", "", `T`),
         Pe = as.numeric(Pe)) %>% 
  mutate(ID_FP = paste(ID, FP, '_')) %>% 
  merge.data.frame(ages, "ID_FP") %>% 
  mutate(Sex = as.factor(substr(ID, 1, 1))) %>% 
  filter(FP == min(FP, na.rm = T))

summary_pe_by_hierarchy <- Pesos_Juan %>%
  group_by(Sex) %>%
  summarise(
    Mean = mean(Pe, na.rm = TRUE),
    Median = median(Pe, na.rm = TRUE),
    SD = sd(Pe, na.rm = TRUE),
    SE = sd(Pe, na.rm = TRUE) / sqrt(n()),
    Min = min(Pe, na.rm = TRUE),
    Max = max(Pe, na.rm = TRUE),
    Count = n()
  )

print(summary_pe_by_hierarchy)



