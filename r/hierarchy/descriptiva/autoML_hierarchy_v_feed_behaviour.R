library(h2o)

hierarchy_data = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(Pen = as.factor(Pen))
hierarchy_data$ID[hierarchy_data$ID=='M904'] = 'M686'


ti_range_data_1 = readRDS('output/Historico_OCCUPATION_TIME/time_slot_mean_by_mixgroup.rds') %>%
  filter(group == 0) %>% 
  mutate(ID = gsub('@.*', '', Ta)) %>% 
  merge.data.frame(hierarchy_data, by = 'ID')


h2o.init()

train = ti_range_data_1 %>% filter(hierarchy != "intermediate") %>%  as.h2o()
y <- "hierarchy"
x <- setdiff(names(train), y)[4:15]

train[, y] <- as.factor(train[, y])

# Run AutoML for 20 base models
aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 20,
                  seed = 1)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)
h2o.varimp(aml@leader)
