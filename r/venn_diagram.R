
res_cons = read.csv('output/modelo_mixto_diff_consumo/top10_residual_consumo_individual_jp.csv')
res_adg = read.csv('output/modelo_mixto_adg/top10_residual_adg_individual_jp.csv')
res_fcr = read.csv('output/modelo_mixto_fcr/top10_residual_fcr_individual_jp.csv')

library(VennDiagram)

venn.diagram(
  x = list(res_adg$Ta, res_cons$Ta, res_fcr$Ta),
  category.names = c("ADG", "Consumo", "FCR"),
  fill = c("red", "green", "blue"),
  alpha = 0.5,
  cex = 1.5,
  filename = 'venny.png'
)
