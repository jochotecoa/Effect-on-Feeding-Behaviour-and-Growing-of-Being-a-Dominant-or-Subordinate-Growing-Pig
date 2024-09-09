
if (!require(psych)) {
  install.packages("psych")
  library(psych)
}

source("r/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_2_v5.R")

feed_data_2 %>%
  group_by(hierarchy) %>%
  summarise(mean_mpg = mean(Co_median),
            se_mpg = sd(Co_median) / sqrt(n()))


source("r/hierarchy/descriptiva/hierarchy1_3categories_vs_relative_feed_behaviour_phase_3_v7.R")


feed_data_3 %>%
  group_by(hierarchy) %>%
  summarise(mean_mpg = mean(Co_median),
            se_mpg = sd(Co_median) / sqrt(n()))

nvis_total_data_3 %>%
  group_by(hierarchy) %>%
  summarise(mean_mpg = mean(n_visitas_median_rel),
            se_mpg = sd(n_visitas_median_rel) / sqrt(n()))
