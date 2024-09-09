library(h2o)
library(tidyverse)


Metaboloite197_ALR_scale <- read.delim2("~/gutbrain/data/metabolites/Input_DAMetabolitesALR_scale.txt")
metadata <- read.delim("~/gutbrain/data/metabolites/metadata.txt")

Metaboloite197_meta = merge.data.frame(x = Metaboloite197_ALR_scale, y = metadata, 
                                       by.x = "Id", by.y = "Class") %>% 
  mutate(Tto = as.factor(Tto))



h2o.init()

train = Metaboloite197_meta %>% 
  as.h2o()

y <- "Tto"
x <- colnames(Metaboloite197_meta)[2:24]

# Run AutoML for 20 base models
aml <- h2o.automl(x = x, y = y,
                  training_frame = train)

lb <- aml@leaderboard 
head(lb)  
h2o.varimp_plot(aml@leader, num_of_features = 25)
h2o.varimp_heatmap(aml)
aml@leader %>% summary()

h2o.saveModel(object = aml@leader, path = 'output/autoML_treatment_v_metabolites/input_23_variables')
