library(tidyverse)
library(readxl)

Copia_de_GUTBRAIN_AllData_10574_1 <- 
  read_excel("xlsx/Copia de GUTBRAIN_AllData(10574).xlsx", 
             sheet = "Dominance 1") %>%
  mutate(Hour = as.character(Hour),
         phase = 1)
Copia_de_GUTBRAIN_AllData_10574_2 <- 
  read_excel("xlsx/Copia de GUTBRAIN_AllData(10574).xlsx", 
             sheet = "Dominance 2") %>%
  mutate(Hour = as.character(Hour),
         phase = 2)
Copia_de_GUTBRAIN_AllData_10574_3 <- 
  read_excel("xlsx/Copia de GUTBRAIN_AllData(10574).xlsx", 
             sheet = "Dominance 3") %>%
  mutate(Hour = as.character(Hour),
         phase = 3)


Copia_de_GUTBRAIN_AllData_10574_ = rbind(Copia_de_GUTBRAIN_AllData_10574_1, 
                                         Copia_de_GUTBRAIN_AllData_10574_2,
                                         Copia_de_GUTBRAIN_AllData_10574_3)

# Group by column X and apply transformation

df_transformed = Copia_de_GUTBRAIN_AllData_10574_ 
names(df_transformed) = names(df_transformed) %>% 
  make.names()
df_transformed = df_transformed %>% 
  mutate(First.to.eat = as.integer(First.to.eat), 
         Mover = as.integer(Mover), 
         Dominant = as.integer(Dominant), 
         Dominant.Confirmed = as.integer(Dominant.Confirmed))

df_transformed_1 = df_transformed[seq(1, nrow(df_transformed), 2),]
df_transformed_2 = df_transformed[seq(2, nrow(df_transformed), 2),]

df_transformed_comprsn = merge.data.frame(df_transformed_1, 
                                          df_transformed_2, 
                                          'Group')
df_transformed_comprsn = df_transformed_comprsn %>% 
  mutate(First.to.eat.x = ifelse(First.to.eat.y == 1, -0.5, First.to.eat.x),
         First.to.eat.y = ifelse(First.to.eat.x == 1, -0.5, First.to.eat.y),
         Mover.x = ifelse(Mover.y == 1, -0.5, Mover.x),
         Mover.y = ifelse(Mover.x == 1, -0.5, Mover.y),
         Dominant.x = ifelse(Dominant.y == 1, -0.5, Dominant.x),
         Dominant.y = ifelse(Dominant.x == 1, -0.5, Dominant.y),
         Dominant.Confirmed.x = ifelse(Dominant.Confirmed.y == 1, -0.5, 
                                       Dominant.Confirmed.x),
         Dominant.Confirmed.y = ifelse(Dominant.Confirmed.x == 1, -0.5, 
                                       Dominant.Confirmed.y))

df_transformed_1 = df_transformed_comprsn[1:12]
df_transformed_2 = df_transformed_comprsn[c(1,13:23)]

colnames(df_transformed_1) = colnames(df_transformed_1) %>% 
  gsub('.x', '',  x = ., fixed = T)
colnames(df_transformed_2) = colnames(df_transformed_2) %>% 
  gsub('.y', '',  x = ., fixed = T)

df_transformed_v2 = rbind.data.frame(df_transformed_1, df_transformed_2) %>% 
  arrange(Group)
df_transformed_v2 %>% 
  group_by(ID, phase) %>% 
  summarise(First.to.eat = sum(First.to.eat, na.rm = T),
            Mover = sum(Mover, na.rm = T),
            Dominant = sum(Dominant, na.rm = T),
            Dominant.Confirmed = sum(Dominant.Confirmed, na.rm = T)) %>% 
  select(-ID, -phase) %>% 
  as.matrix %>% 
  heatmap()

df_transformed_v2 %>% 
  group_by(ID, phase) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  na.omit() %>% 
  select(-ID) %>% 
  as.matrix %>% 
  heatmap()

df_transformed_v3 = df_transformed_v2 %>% 
  group_by(ID, phase) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  na.omit() 

corrs = df_transformed_v3 %>% select(-ID) %>%  cor()
estmt_Frst.t.t_Dmnnt.Cnfrmd = corrs['First.to.eat', 'Dominant.Confirmed']
estmt_Mvr_Dmnnt.Cnfrmd = corrs['Mover', 'Dominant.Confirmed']
estmt_Dmnnt_Dmnnt.Cnfrmd = corrs['Dominant', 'Dominant.Confirmed']
estmt_Dmnnt.Cnfrmd_Dmnnt.Cnfrmd = corrs['Dominant.Confirmed', 'Dominant.Confirmed']

df_transformed_v3 %>% lm(Dominant ~ Mover, .) %>% summary
model = df_transformed_v3 %>% lm(Dominant.Confirmed ~ Dominant, .) 
model$coefficients['Mover']
df_transformed_v4 = df_transformed_v3 %>% 
  mutate(ranking_score = 
           !!model$coefficients['Mover']*Mover +
           !!model$coefficients['Dominant']*Dominant +
           Dominant.Confirmed)

