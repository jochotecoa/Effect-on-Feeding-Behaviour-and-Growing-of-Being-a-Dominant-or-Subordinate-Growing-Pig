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


p_vls_df = data.frame()

for (Ta in unique(consumo_pesos_2$Ta)) {
  animal_cnsm = filter(consumo_pesos_2, Ta == !!Ta, grepl('initial|second', group))
  animal_cnsm$pred = predict(
    lm(Co/Pe_app ~ edad + Pe_app, 
       filter(animal_cnsm, group == 'initial')),
    animal_cnsm)
  animal_cnsm$rsdls = animal_cnsm$Co/animal_cnsm$Pe_app - animal_cnsm$pred
  animal_cnsm$group = droplevels(animal_cnsm$group)
  mod_lm_anml = t.test(rsdls ~ group, animal_cnsm)
  summ_mod_lm_anml = summary(mod_lm_anml)
  p_vls_ttest = mod_lm_anml$p.value
  p_vls_ttest = data.frame(p.value = p_vls_ttest,  
                     row.names = Ta) %>% 
    rownames_to_column("Ta") %>% 
    merge.data.frame(y = select(consumo_pesos_2, Ta, Control), "Ta") %>% 
    unique() %>% 
    remove_rownames() %>% 
    column_to_rownames("Ta")
  
  p_vls_df = rbind.data.frame(p_vls_df, p_vls_ttest)
}

p_vls_df$Control[p_vls_df$p.value < 0.05] %>% table()


a = filter(consumo_pesos_2, Ta == 'F827@2022-03-23')
a$pred = predict(modelo_lm_control, a)
plot(a$edad, a$Co)
plot(a$edad, a$Co - a$pred)
points(a$edad, a$pred, col = 2)
mod_a = lm(Co ~ pred + group, a) 
summary(mod_a)
car::Anova(mod_a, type = 3) %>% summary
summary(mod_a)

abline(mod_a$coefficients[1], mod_a$coefficients[4], col = 3)



