significant_variables = NULL

for (variable in colnames(Metaboloite197_meta)[2:197]) {
  Metaboloite197_meta$variable = Metaboloite197_meta[, variable] %>% as.numeric()
  a = t.test(Metaboloite197_meta$variable ~ Metaboloite197_meta$Tto)
  if (a$p.value < 0.05) {
    boxplot(Metaboloite197_meta$variable ~ Metaboloite197_meta$Tto, ylab = variable)
    print(variable)
    significant_variables = c(significant_variables, variable)
  }
}
