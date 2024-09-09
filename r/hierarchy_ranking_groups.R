library(tidyverse)
library(readxl)

# Dominance 1 -------------------------------------------------------------


Copia_de_GUTBRAIN_AllData_10574_ <- 
  read_excel("xlsx/GUTBRAIN_AllData(10574).xlsx", 
             sheet = "Dominance 1")
Copia_de_GUTBRAIN_AllData_10574_$ID %>% table %>% all(. == 7) %>% stopifnot()

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
  group_by(ID) %>% 
  summarise(First.to.eat = sum(First.to.eat, na.rm = T),
            Mover = sum(Mover, na.rm = T),
            Dominant = sum(Dominant, na.rm = T),
            Dominant.Confirmed = sum(Dominant.Confirmed, na.rm = T)) %>% 
  select(-ID) %>% 
  as.matrix %>% 
  heatmap()

df_transformed_v2 %>% 
  group_by(ID) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  na.omit() %>% 
  select(-ID) %>% 
  as.matrix %>% 
  heatmap()

df_transformed_v3 = df_transformed_v2 %>% 
   group_by(ID) %>% 
   summarise(First.to.eat = mean(First.to.eat, na.rm = T),
             Mover = mean(Mover, na.rm = T),
             Dominant = mean(Dominant, na.rm = T),
             Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
   na.omit() 

# corrs = df_transformed_v3 %>% select(-ID) %>%  cor()
# estmt_Frst.t.t_Dmnnt.Cnfrmd = corrs['First.to.eat', 'Dominant.Confirmed']
# estmt_Mvr_Dmnnt.Cnfrmd = corrs['Mover', 'Dominant.Confirmed']
# estmt_Dmnnt_Dmnnt.Cnfrmd = corrs['Dominant', 'Dominant.Confirmed']
# estmt_Dmnnt.Cnfrmd_Dmnnt.Cnfrmd = corrs['Dominant.Confirmed', 'Dominant.Confirmed']

# df_transformed_v3 %>% lm(Dominant.Confirmed ~ Mover + Dominant, .) %>% summary
# model = df_transformed_v3 %>% lm(Dominant.Confirmed ~ Mover + Dominant, .) 

df_transformed_v4 = df_transformed_v3 %>% 
  mutate(ranking_score = Dominant*0.5 + Dominant.Confirmed) %>% 
  mutate(hierarchy = case_when(
    ranking_score < quantile(ranking_score, 0.25) ~ 'submissive',
    (ranking_score >= quantile(ranking_score, 0.25) & 
      ranking_score < quantile(ranking_score, 0.5)) ~ 'slightly_submissive',
    ranking_score >= quantile(ranking_score, 0.5) & 
      ranking_score < quantile(ranking_score, 0.75) ~ 'slightly_dominant',
    ranking_score >= quantile(ranking_score, 0.75) ~ 'dominant'
  ))

dir.create('output/hierarchy_ranking_groups/')
df_transformed_v4 %>% 
  saveRDS('output/hierarchy_ranking_groups/dominance_phase_1.rds')
Copia_de_GUTBRAIN_AllData_10574_ %>% select(ID, Pen, Sex) %>% unique() %>% 
  saveRDS('output/hierarchy_ranking_groups/metadata_phase_1.rds')

df_transformed_v5 = df_transformed_v3 %>% 
  mutate(ranking_score = Dominant*0.5 + Dominant.Confirmed) %>% 
  merge.data.frame(select(Copia_de_GUTBRAIN_AllData_10574_, ID, Pen), by = 'ID') %>% 
  unique() %>% 
  group_by(Pen) %>% 
  mutate(hierarchy = case_when(
    rank(ranking_score, ties.method = "first") <= 2 ~ "submissive",
    rank(ranking_score, ties.method = "last") >= n() - 1 ~ "dominant",
    TRUE ~ "intermediate"
  )) %>% 
  ungroup() 


df_transformed_v5 %>% 
  saveRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds')


# Dominance 2 -------------------------------------------------------------


Copia_de_GUTBRAIN_AllData_10574_ <- 
  read_excel("xlsx/GUTBRAIN_AllData(10574).xlsx", 
             sheet = "Dominance 2")
Copia_de_GUTBRAIN_AllData_10574_$ID %>% table %>% all(. == 7) %>% stopifnot()

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

# Select columns 1 to 23 and split into two data frames
df_transformed_1 = df_transformed_comprsn[1:12]
df_transformed_2 = df_transformed_comprsn[c(1,14:24)]

colnames(df_transformed_1) = colnames(df_transformed_1) %>% 
  gsub('.x', '',  x = ., fixed = T)
colnames(df_transformed_2) = colnames(df_transformed_2) %>% 
  gsub('.y', '',  x = ., fixed = T)

df_transformed_v2 = df_transformed_comprsn %>% 
  arrange(Group)
df_transformed_v2 = rbind.data.frame(df_transformed_1, df_transformed_2) %>% 
  arrange(Group)
df_transformed %>% 
  group_by(ID) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  select(-ID) %>% 
  as.matrix %>% 
  heatmap()

df_transformed_v2 %>% 
  group_by(ID) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  na.omit() %>% 
  select(-ID) %>% 
  as.matrix %>% 
  heatmap()

df_transformed_v3 = df_transformed_v2 %>% 
  group_by(ID) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  na.omit() 

# corrs = df_transformed_v3 %>% select(-ID) %>%  cor()
# estmt_Frst.t.t_Dmnnt.Cnfrmd = corrs['First.to.eat', 'Dominant.Confirmed']
# estmt_Mvr_Dmnnt.Cnfrmd = corrs['Mover', 'Dominant.Confirmed']
# estmt_Dmnnt_Dmnnt.Cnfrmd = corrs['Dominant', 'Dominant.Confirmed']
# estmt_Dmnnt.Cnfrmd_Dmnnt.Cnfrmd = corrs['Dominant.Confirmed', 'Dominant.Confirmed']

# df_transformed_v3 %>% lm(Dominant.Confirmed ~ Mover + Dominant, .) %>% summary
# model = df_transformed_v3 %>% lm(Dominant.Confirmed ~ Mover + Dominant, .) 

df_transformed_v4 = df_transformed_v3 %>% 
  mutate(ranking_score = Dominant*0.5 + Dominant.Confirmed) %>% 
  mutate(hierarchy = case_when(
    ranking_score < quantile(ranking_score, 0.25) ~ 'submissive',
    (ranking_score >= quantile(ranking_score, 0.25) & 
       ranking_score < quantile(ranking_score, 0.5)) ~ 'slightly_submissive',
    ranking_score >= quantile(ranking_score, 0.5) & 
      ranking_score < quantile(ranking_score, 0.75) ~ 'slightly_dominant',
    ranking_score >= quantile(ranking_score, 0.75) ~ 'dominant'
  ))

dir.create('output/hierarchy_ranking_groups/')
df_transformed_v4 %>% 
  saveRDS('output/hierarchy_ranking_groups/dominance_phase_2.rds')
Copia_de_GUTBRAIN_AllData_10574_ %>% select(ID, Pen, Sex) %>% unique() %>% 
  saveRDS('output/hierarchy_ranking_groups/metadata_phase_2.rds')

df_transformed_v5 = df_transformed_v3 %>% 
  mutate(ranking_score = Dominant*0.5 + Dominant.Confirmed) %>% 
  merge.data.frame(select(Copia_de_GUTBRAIN_AllData_10574_, ID, Pen), by = 'ID') %>% 
  unique() %>% 
  group_by(Pen) %>% 
  mutate(hierarchy = case_when(
    rank(ranking_score, ties.method = "first") <= 2 ~ "submissive",
    rank(ranking_score, ties.method = "last") >= n() - 1 ~ "dominant",
    TRUE ~ "intermediate"
  )) %>% 
  ungroup() 


df_transformed_v5 %>% 
  saveRDS('output/hierarchy_ranking_groups/dominance_phase_2_by_pen.rds')

# Dominance 3 -------------------------------------------------------------


Copia_de_GUTBRAIN_AllData_10574_ <- 
  read_excel("xlsx/GUTBRAIN_AllData(10574).xlsx", 
             sheet = "Dominance 3")
Copia_de_GUTBRAIN_AllData_10574_$ID %>% table %>% all(. == 7) %>% stopifnot()

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
df_transformed_2 = df_transformed_comprsn[c(1,14:24)]

colnames(df_transformed_1) = colnames(df_transformed_1) %>% 
  gsub('.x', '',  x = ., fixed = T)
colnames(df_transformed_2) = colnames(df_transformed_2) %>% 
  gsub('.y', '',  x = ., fixed = T)

df_transformed_v2 = rbind.data.frame(df_transformed_1, df_transformed_2) %>% 
  arrange(Group)
df_transformed %>% 
  group_by(ID) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  select(-ID) %>% 
  na.omit() %>% 
  as.matrix %>% 
  heatmap()

df_transformed_v2 %>% 
  group_by(ID) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  na.omit() %>% 
  select(-ID) %>% 
  as.matrix %>% 
  heatmap()

df_transformed_v3 = df_transformed_v2 %>% 
  group_by(ID) %>% 
  summarise(First.to.eat = mean(First.to.eat, na.rm = T),
            Mover = mean(Mover, na.rm = T),
            Dominant = mean(Dominant, na.rm = T),
            Dominant.Confirmed = mean(Dominant.Confirmed, na.rm = T)) %>% 
  na.omit() 

# corrs = df_transformed_v3 %>% select(-ID) %>%  cor()
# estmt_Frst.t.t_Dmnnt.Cnfrmd = corrs['First.to.eat', 'Dominant.Confirmed']
# estmt_Mvr_Dmnnt.Cnfrmd = corrs['Mover', 'Dominant.Confirmed']
# estmt_Dmnnt_Dmnnt.Cnfrmd = corrs['Dominant', 'Dominant.Confirmed']
# estmt_Dmnnt.Cnfrmd_Dmnnt.Cnfrmd = corrs['Dominant.Confirmed', 'Dominant.Confirmed']

# df_transformed_v3 %>% lm(Dominant.Confirmed ~ Mover + Dominant, .) %>% summary
# model = df_transformed_v3 %>% lm(Dominant.Confirmed ~ Mover + Dominant, .) 

df_transformed_v4 = df_transformed_v3 %>% 
  mutate(ranking_score = Dominant*0.5 + Dominant.Confirmed) %>% 
  mutate(hierarchy = case_when(
    ranking_score < quantile(ranking_score, 0.25) ~ 'submissive',
    (ranking_score >= quantile(ranking_score, 0.25) & 
       ranking_score < quantile(ranking_score, 0.5)) ~ 'slightly_submissive',
    ranking_score >= quantile(ranking_score, 0.5) & 
      ranking_score < quantile(ranking_score, 0.75) ~ 'slightly_dominant',
    ranking_score >= quantile(ranking_score, 0.75) ~ 'dominant'
  ))

dir.create('output/hierarchy_ranking_groups/')
df_transformed_v4 %>% 
  saveRDS('output/hierarchy_ranking_groups/dominance_phase_3.rds')
Copia_de_GUTBRAIN_AllData_10574_ %>% select(ID, Pen, Sex) %>% unique() %>% 
  saveRDS('output/hierarchy_ranking_groups/metadata_phase_3.rds')

df_transformed_v5 = df_transformed_v3 %>% 
  mutate(ranking_score = Dominant*0.5 + Dominant.Confirmed) %>% 
  merge.data.frame(select(Copia_de_GUTBRAIN_AllData_10574_, ID, Pen), by = 'ID') %>% 
  unique() %>% 
  group_by(Pen) %>% 
  mutate(hierarchy = case_when(
    rank(ranking_score, ties.method = "first") <= 2 ~ "submissive",
    rank(ranking_score, ties.method = "last") >= n() - 1 ~ "dominant",
    TRUE ~ "intermediate"
  )) %>% 
  ungroup() 


df_transformed_v5 %>% 
  saveRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds')
