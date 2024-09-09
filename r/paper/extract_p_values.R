library(tidyverse)
library(sjPlot)
library(stringr)
hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds') %>%
mutate(Pen = as.factor(Pen)) %>%
filter(ID != "M144",
ID != "M394") %>%
filter(hierarchy != "intermediate") %>%
mutate(hierarchy = str_to_title(hierarchy))
# hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'
# phase 3, group == 'second', group = '2'
# # boxplot(Dominant.Confirmed ~ hierarchy + Control, hierarchy_data)
# # boxplot(Dominant ~ hierarchy + Control, hierarchy_data)
library(dplyr)
is.odd <- function(x) x %% 2 != 0
# intake ------------------------------------------------------------------
# initial   first  second
# 62      62      62
feed_data_2 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>%
filter(group == 'first') %>%
mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>%
merge.data.frame(hierarchy_data, by = 'ID') %>%
group_by(Pen) %>%
mutate(Co_median_rel = Co_median/median(Co_median)) %>%
ungroup() %>%
mutate(Control = as.factor(is.odd(as.numeric(Pen))))
co_lm = lm(Co_median ~ hierarchy + Control + Pen, feed_data_2)
co_rel_lm = lm(Co_median_rel ~ hierarchy + Control, feed_data_2)
co_lm %>% summary()

co_rel_lm %>% summary()




# Phase 3 -----------------------------------------------------------------

library(tidyverse)
library(sjPlot)

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(Pen = as.factor(Pen)) %>% 
  filter(ID != "M144", # No data until end of July
         ID != "M394") %>% # No feed data
  filter(hierarchy != "intermediate") # Only Dominant versus Submissive
# hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'

# phase 3, group == 'second', group = '2'

# boxplot(Dominant.Confirmed ~ hierarchy + Control, hierarchy_data)
# boxplot(Dominant ~ hierarchy + Control, hierarchy_data)

library(dplyr)


is.odd <- function(x) x %% 2 != 0


# Intake ------------------------------------------------------------------


feed_data_3 = readRDS('output/ranking_diff_consumo_without_modelling/consumo_median_residuals_by_mixgroup.rds') %>% 
  filter(group == 'second') %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Co_median_rel = Co_median/median(Co_median)) %>% 
  ungroup() %>%    
  mutate(Control = as.factor(is.odd(as.numeric(Pen))),
         hierarchy = str_to_title(hierarchy))

co_lm = lm(Co_median ~ hierarchy + Control + Pen, feed_data_3)
co_rel_lm = lm(Co_median_rel ~ hierarchy + Control, feed_data_3)

co_lm %>% summary()

co_rel_lm %>% summary()


nvis_total_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(n_visitas_median_rel = n_visitas_median/median(n_visitas_median)) %>% 
  ungroup() %>% 
  
  mutate(Control = as.factor(is.odd(as.numeric(Pen))),          
         hierarchy = str_to_title(hierarchy))

n_vis_rel_model = lm(n_visitas_median_rel ~ hierarchy + Control, nvis_total_data_3)

n_vis_rel_model %>% summary()



ti_total_data_3 = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/duracion_total_diaria_consumo_median_residuals_by_mixgroup.rds') %>%
  filter(group == 2) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>%  mutate(ID = gsub("M686", "M904", ID)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID') %>% 
  group_by(Pen) %>% 
  mutate(Ti_median_rel = Ti_median/median(Ti_median)) %>% 
  ungroup() %>% 
  
  mutate(Control = as.factor(is.odd(as.numeric(Pen))),          
         hierarchy = str_to_title(hierarchy))

Ti_median_rel_lm = lm(Ti_median_rel ~ hierarchy + Control, ti_total_data_3) 

Ti_median_rel_lm %>% summary()
