

# Co ----------------------------------------------------------------------


feed_data_123 = rbind.data.frame(feed_data_1, feed_data_2) %>% 
  rbind.data.frame(feed_data_3)

boxplot(feed_data_123$Co_median ~ feed_data_123$hierarchy)

co_lm = lm(Co_median ~ hierarchy + group + Pen, feed_data_123)

co_lm %>% plot_model(show.values = T)

boxplot(feed_data_123$Co_median_rel ~ feed_data_123$hierarchy)

co_lm = lm(Co_median_rel ~ hierarchy + Pen, feed_data_123)

co_lm %>% plot_model(show.values = T)

# FR ----------------------------------------------------------------------


fr_total_data_123 = rbind.data.frame(fr_total_data_1, fr_total_data_2) %>% 
  rbind.data.frame(fr_total_data_3)

boxplot(fr_total_data_123$Fr_median ~ fr_total_data_123$hierarchy)

fr_lm = lm(Fr_median ~ hierarchy*group + Pen, fr_total_data_123)

fr_lm %>% plot_model(show.values = T)

boxplot(fr_total_data_123$Fr_median_rel ~ fr_total_data_123$hierarchy)

fr_lm = lm(Fr_median_rel ~ hierarchy*group + Pen, fr_total_data_123)

fr_lm %>% plot_model(show.values = T)

# n_visitas ---------------------------------------------------------------


nvis_total_data_123 = rbind.data.frame(nvis_total_data_1, nvis_total_data_2) %>% 
  rbind.data.frame(nvis_total_data_3)

boxplot(nvis_total_data_123$n_visitas_median ~ nvis_total_data_123$hierarchy)

nvis_lm = lm(n_visitas_median ~ hierarchy*group + Pen, nvis_total_data_123)

nvis_lm %>% plot_model(show.values = T)

boxplot(nvis_total_data_123$n_visitas_median_rel ~ nvis_total_data_123$hierarchy)

nvis_lm = lm(n_visitas_median_rel ~ hierarchy*group + Pen, nvis_total_data_123)

nvis_lm %>% plot_model(show.values = T)


# Ti median ---------------------------------------------------------------

ti_median_data_123 = rbind.data.frame(ti_median_data_1, ti_median_data_2) %>% 
  rbind.data.frame(ti_median_data_3)

boxplot(ti_median_data_123$median_Ti_median ~ ti_median_data_123$hierarchy)

median_Ti_median_lm = lm(median_Ti_median ~ hierarchy + Pen, ti_median_data_123)

median_Ti_median_lm %>% plot_model(show.values = T)

boxplot(ti_median_data_123$median_Ti_median_rel ~ ti_median_data_123$hierarchy)

median_Ti_median_lm = lm(median_Ti_median_rel ~ hierarchy + Pen, ti_median_data_123)

median_Ti_median_lm %>% plot_model(show.values = T)


# Ti range ----------------------------------------------------------------

ti_range_data_123 = rbind.data.frame(ti_range_data_1, ti_range_data_2) %>% 
  rbind.data.frame(ti_range_data_3)
for (variable in subset(colnames(ti_range_data_123), grepl('H', colnames(ti_range_data_123)))) {
  # boxplot(ti_range_data_123[, variable] ~ ti_range_data_123$hierarchy)
  
  # Assuming 'character' contains the variable name as a character string
  variable_name <- as.name(variable)
  
  # Creating the formula dynamically
  formula <- as.formula(paste0(deparse(variable_name), " ~ hierarchy * group"))
  
  # Fitting the linear model
  lm_model <- lm(formula, data = ti_range_data_123)
  
  # Plotting the model using sjPlot
  a = sjPlot::plot_model(lm_model, show.values = TRUE)
  plot(a)
  
  readline(prompt="Press [enter] to continue")
  
}

median_ti_range_lm %>% plot_model(show.values = T)

boxplot(ti_range_data_123$median_ti_range_rel ~ ti_range_data_123$hierarchy)

median_ti_range_lm = lm(median_ti_range_rel ~ hierarchy + Pen, ti_range_data_123)

median_ti_range_lm %>% plot_model(show.values = T)




