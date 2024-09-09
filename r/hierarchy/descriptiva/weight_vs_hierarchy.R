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
  merge.data.frame(ages, "ID_FP")


# Phase 1 -----------------------------------------------------------------


hierarchy_data_1 = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 
  
hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'

weight_data_1 = weight_data %>% 
  mutate(FP_apprx = as.Date(FP_apprx)) %>% 
  filter(FP_apprx < "2022-07-25")



hierarchy_weight_1 = merge.data.frame(x = hierarchy_data_1, y = weight_data_1, 
                                    by = "ID")

boxplot(hierarchy_weight_1$Pe_apprx ~ hierarchy_weight_1$hierarchy)

lm(Pe_apprx ~ hierarchy*treatment, 
               data = hierarchy_weight_1) %>% 
  sjPlot::plot_model(show.values = T)

lmerTest::lmer(Pe_apprx ~ hierarchy*edad + (1|ID), 
               data = hierarchy_weight_1) %>% 
  sjPlot::plot_model(show.values = T)


lmerTest::lmer(Pe_apprx ~ hierarchy*edad + (1|ID), 
               data = hierarchy_weight_1) %>% 
  sjPlot::plot_model(show.values = T, type = "pred", terms = c("edad", "hierarchy"))

# Phase 1 with all weights ------------------------------------------------

hierarchy_weight = merge.data.frame(x = hierarchy_data_1, y = weight_data, 
                                    by = "ID")

lmerTest::lmer(Pe_apprx ~ hierarchy * edad *  + (1|ID), 
               data = filter(hierarchy_weight, hierarchy != "intermediate")) %>% 
  sjPlot::plot_model(show.values = T)

# Phase 2 -----------------------------------------------------------------


hierarchy_data_2 = readRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

weight_data_2 = weight_data %>% 
  mutate(FP_apprx = as.Date(FP_apprx)) %>% 
  filter(FP_apprx >= "2022-07-25",
         FP_apprx < "2022-08-16")

hierarchy_weight_2 = merge.data.frame(x = hierarchy_data_2, y = weight_data_2, 
                                    by = "ID")

boxplot(hierarchy_weight_2$Pe_apprx ~ hierarchy_weight_2$hierarchy)

lm(Pe_apprx ~ hierarchy + edad + treatment, 
   data = hierarchy_weight_2) %>% 
  sjPlot::plot_model(show.values = T)

lm(Pe_apprx ~ hierarchy, 
   data = hierarchy_weight_2) %>% 
  sjPlot::plot_model(show.values = T, type = "pred")


lmerTest::lmer(Pe_apprx ~ hierarchy + edad + treatment + (1|ID), 
               data = hierarchy_weight_2) %>% 
  sjPlot::plot_model(show.values = T)#, type = "pred", terms = c("edad", "hierarchy"))


# Phase 3 -----------------------------------------------------------------

hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

weight_data_3 = weight_data %>% 
  mutate(FP_apprx = as.Date(FP_apprx)) %>% 
  filter(FP_apprx >= "2022-08-16")

hierarchy_weight_3 = merge.data.frame(x = hierarchy_data_3, y = weight_data_3, 
                                      by = "ID")
boxplot(hierarchy_weight_3$Pe_apprx ~ hierarchy_weight_3$hierarchy)

lm(Pe_apprx ~ hierarchy, 
   data = hierarchy_weight_3) %>% 
  sjPlot::plot_model(show.values = T)


lmerTest::lmer(Pe_apprx ~ hierarchy + edad + treatment + (1|ID), 
               data = hierarchy_weight_3) %>% 
  sjPlot::plot_model(show.values = T) 
type = "pred", terms = c("edad", "hierarchy"))



# Joined data -------------------------------------------------------------

hierarchy_weight_1$group = '0' 
hierarchy_weight_2$group = '1'
hierarchy_weight_3$group = '2'


hierarchy_weight_123 = rbind.data.frame(hierarchy_weight_1, 
                                        hierarchy_weight_2,
                                        hierarchy_weight_3)

lmerTest::lmer(Pe_apprx ~ hierarchy * edad + (1|ID), 
               data = hierarchy_weight_123) %>% 
  sjPlot::plot_model(show.values = T, type = "pred", terms = c("hierarchy", "edad"))


# pesos_juan_1 ------------------------------------------------------------

Pesos_Juan_1 = Pesos_Juan %>% 
  filter(FP < "2022-07-25") 

hierarchy_data_1 = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'

hierarchy_pesos_1 = merge.data.frame(x = hierarchy_data_1, y = Pesos_Juan_1, 
                                      by = "ID")

boxplot(hierarchy_pesos_1$Pe ~ hierarchy_pesos_1$hierarchy)
if (any(table(hierarchy_pesos_1$ID) > 1)) {
  lmerTest::lmer(Pe ~ hierarchy + treatment + edad + (1|ID), 
                 data = hierarchy_pesos_1) %>% 
    sjPlot::plot_model(show.values = T) %>% plot()
  print("lmer")
} else {
  lm(Pe ~ hierarchy + treatment + edad, 
     data = hierarchy_pesos_1) %>% 
    sjPlot::plot_model(show.values = T) %>% 
    plot()
  print("lm")
}



# pesos_juan_2 ------------------------------------------------------------

Pesos_Juan_2 = Pesos_Juan %>% 
  filter(FP >= "2022-07-25",
         FP < "2022-08-16")
hierarchy_data_2 = readRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

hierarchy_pesos_2 = merge.data.frame(x = hierarchy_data_2, y = Pesos_Juan_2, 
                                     by = "ID")

boxplot(hierarchy_pesos_2$Pe ~ hierarchy_pesos_2$hierarchy)

if (any(table(hierarchy_pesos_2$ID) > 1)) {
  lmerTest::lmer(Pe ~ hierarchy + treatment + edad + (1|ID), 
                 data = hierarchy_pesos_2) %>% 
    sjPlot::plot_model(show.values = T) %>% plot()
  print("lmer")
} else {
  lm(Pe ~ hierarchy + treatment + edad, 
     data = hierarchy_pesos_2) %>% 
    sjPlot::plot_model(show.values = T) %>% 
    plot()
  print("lm")
}


# pesos_juan_3 ------------------------------------------------------------

Pesos_Juan_3 = Pesos_Juan %>% 
  filter(FP >= "2022-08-16")
hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) 

hierarchy_pesos_3 = merge.data.frame(x = hierarchy_data_3, y = Pesos_Juan_3, 
                                     by = "ID")

boxplot(hierarchy_pesos_3$Pe ~ hierarchy_pesos_3$hierarchy)

if (any(table(hierarchy_pesos_3$ID) > 1)) {
  lmerTest::lmer(Pe ~ hierarchy + treatment + edad + (1|ID), 
                 data = hierarchy_pesos_3) %>% 
    sjPlot::plot_model(show.values = T) %>% plot()
  print("lmer")
} else {
  lm(Pe ~ hierarchy + treatment + edad, 
     data = hierarchy_pesos_3) %>% 
    sjPlot::plot_model(show.values = T) %>% 
    plot()
  print("lm")
}
