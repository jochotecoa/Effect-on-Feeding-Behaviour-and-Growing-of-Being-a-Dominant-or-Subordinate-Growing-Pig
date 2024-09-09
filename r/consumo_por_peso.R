# install.packages('dplyr')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(progress)
library(ggstatsplot)
library(ggpubr)


# Load data ---------------------------------------------------------------

source('r/load_data.R')

# Seleccionar los animales estres/tratamiento
consumo_pesos_est =
  consumo_pesos %>% 
  filter(!Control) 

# Crear un modelo linear de los controles  
lm_controls = consumo_pesos %>% 
  filter(Control) %>% 
  lm(cons_esc ~ F, data = .)

cons_stat = data.frame(row.names = unique(consumo_pesos_est$Ta))
cons_stat$p.value = NA
cons_stat$p.value_1v2 = NA
cons_stat$p.value_0v1 = NA
cons_stat$p.value_0v2 = NA

dir.create("output/consumo_por_peso/", recursive = T)

for (Ta_i in unique(consumo_pesos_est$Ta)) {
  
  consumo_pesos_est_i = consumo_pesos_est %>% 
    filter(Ta == !!Ta_i)
  
  predicted_vals <- predict(lm_controls, newdata = consumo_pesos_est_i)
  
  consumo_pesos_est_i$residuals_cons_esc = 
    consumo_pesos_est_i$cons_esc - predict(lm_controls, newdata = consumo_pesos_est_i)
  
  ggb = ggbetweenstats(data = consumo_pesos_est_i, x = group, y = residuals_cons_esc, 
                       type = "n")
  ggstat = extract_stats(ggb)
  
  
  
  if (ggstat$subtitle_data$p.value < 0.05) {
    png(filename = paste0("output/consumo_por_peso/", Ta_i, ".png"))
    ggb %>% plot()
    dev.off()
    # lm_before = consumo_pesos_est_i %>% 
    #   filter(F < "2022-07-25") %>% 
    #   lm(residuals_cons_esc ~ F, data = .)
    # 
    # plot(residuals_cons_esc ~ F, consumo_pesos_est_i)
    # abline(lm_before, col = 2)
    
  }
  
  
  # boxplot(residuals_cons_esc ~ group, consumo_pesos_est_i)
  
  consumo_pesos_est_i$residuals_cons_esc %>% shapiro.test()
  
  
  cons_stat[Ta_i, "p.value"] = ggstat$subtitle_data$p.value
  cons_stat[Ta_i, "p.value_1v2"] = ggstat$pairwise_comparisons_data %>% 
    filter(group1 == 'first',
           group2 == 'second') %>% 
    select(p.value)
  cons_stat[Ta_i, "p.value_0v1"] = ggstat$pairwise_comparisons_data %>% 
    filter(group1 == 'initial',
           group2 == 'first') %>% 
    select(p.value)
  cons_stat[Ta_i, "p.value_0v2"] = ggstat$pairwise_comparisons_data %>% 
    filter(group1 == 'initial',
           group2 == 'second') %>% 
    select(p.value)
  
}

cons_stat_logi = (cons_stat < 0.05) %>% as.data.frame()
# cons_stat_rown = cons_stat_logi
# for (col in colnames(cons_stat_logi)) {
#   cons_stat_rown[, col] = rownames(cons_stat_logi)[cons_stat_rown[, col]]
# }

library(VennDiagram)

venn.diagram(
  x = list(
           p.value_1v2 = rownames(cons_stat_logi)[cons_stat_rown$p.value_1v2],
           p.value_0v1 = rownames(cons_stat_logi)[cons_stat_rown$p.value_0v1],
           p.value_0v2 = rownames(cons_stat_logi)[cons_stat_rown$p.value_0v2]),
  filename = "venn_diagram.png",
  # col = c("red", "blue", "green"),
  # fill = c("red", "blue", "green"),
  # alpha = c(0.5, 0.5, 0.5),
  # label.col = c("white", "white", "white"),
  # cex = 2,
  # fontface = "bold",
  # cat.cex = 1.5,
  # cat.fontface = "bold",
  # margin = 0.05
)

