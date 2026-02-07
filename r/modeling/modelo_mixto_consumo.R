source('r/load_data.R')


consumo_pesos_2 = consumo_pesos %>% 
  mutate(edad = as.numeric(EDAD)) %>% 
  select(Co, Ta, Control, group, edad, Pe_app) %>% 
  mutate(edad2 = edad^2, edad3 = edad^3, 
         edad1_2 = edad^(1/2), edad1_3 = edad^(1/3),
         sex = as.numeric(grepl('M', Ta))) %>% 
  unique() %>% 
  group_by(Ta) %>% 
  mutate(Pe_i = min(Pe_app)) %>% 
  # select(-Pe_app) %>% 
  filter(Ta != "M144@2022-03-24") # Sólo tiene consumo a partir de finales de 
#                                   julio

# [1] "Co"      "Ta"      "Control" "group"   "edad"    "Pe_i"   
modelo_lm_control = lm(Co ~ Pe_app + edad*Pe_app, filter(consumo_pesos_2, Control))
modelo_lm_control %>% summary

p_vls_df = data.frame()

for (Ta in unique(consumo_pesos_2$Ta)) {
  animal_cnsm = filter(consumo_pesos_2, Ta == !!Ta)
  animal_cnsm$pred = predict(modelo_lm_control, animal_cnsm)
  mod_lm_anml = lm(Co ~ group + pred, animal_cnsm) 
  summ_mod_lm_anml = summary(mod_lm_anml)
  p_vls = summ_mod_lm_anml$coefficients[c('groupfirst', 'groupsecond'), 
                                        "Pr(>|t|)"]
  p_vls = data.frame(groupfirst = p_vls[1], groupsecond = p_vls[2], 
                     row.names = Ta) %>% 
    rownames_to_column("Ta") %>% 
    merge.data.frame(y = select(consumo_pesos_2, Ta, Control), "Ta") %>% 
    unique() %>% 
    remove_rownames() %>% 
    column_to_rownames("Ta")
  
  p_vls_df = rbind.data.frame(p_vls_df, p_vls)
}


a = filter(consumo_pesos_2, Ta == 'F844@2022-03-20', grepl('initial|second', group))
a$pred = predict(modelo_lm_control, a)
plot(a$edad, a$Co)
points(a$edad, a$pred, col = 2)
mod_a = lm(Co ~ pred + group, a) 
summary(mod_a)
car::Anova(mod_a, type = 3) %>% summary
summary(mod_a)

abline(mod_a$coefficients[1], mod_a$coefficients[4], col = 3)



