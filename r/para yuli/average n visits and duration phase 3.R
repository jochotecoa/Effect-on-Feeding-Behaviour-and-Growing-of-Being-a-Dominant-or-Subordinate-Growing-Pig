run_script_until_line <- function(file_path, stop_line, start_line = 1) {
  # Read all lines from the script
  lines <- readLines(file_path)
  
  # Extract the lines up to the specified line number
  lines_to_run <- lines[start_line:stop_line]
  
  # Combine the lines into a single string
  code_to_run <- paste(lines_to_run, collapse = "\n")
  
  # Evaluate the code in the global environment
  eval(parse(text = code_to_run), envir = .GlobalEnv)
}


run_script_until_line(file_path, stop_line)

run_script_until_line(file_path = "r/microbiota/weight_vs_microbiota_v4.R", 
                      stop_line = 33)

microbiota_animals = Genus_meta$Subject.ID %>% unique()
microbiota_animals_group = Genus_meta %>% 
  select(Subject.ID, Group) %>% 
  unique.data.frame()

n_visitas = readRDS('output/Historico_CONTROL_raw_VISITS_without_modelling/n_visitas_median_residuals_by_mixgroup.rds') %>% 
  mutate(tatu = gsub("@.*", "", Ta)) %>% 
  merge.data.frame(y = microbiota_animals_group, by.x = 'tatu', by.y = 'Subject.ID') %>% 
  filter(tatu %in% !!microbiota_animals,
         FC <= '2022-10-06')

  

# Create the boxplot
boxplot(n_visitas$n_visitas_median ~ n_visitas$Group, 
        ylab = "N visits", 
        xlab = "Treatment",
        names = c("Stress", "Control"))  # Replace Name1 and Name2 with your desired labels

# Calculate the means
means <- tapply(n_visitas$n_visitas_median, n_visitas$Group, mean)

# Add the means to the plot
points(1:length(means), means, col = "red", pch = 19)  # Add points for the means
text(1:length(means), means, labels = round(means, 2), pos = 3)  # Add text with the means



# Example usage:
file_path <- 'r/Historico_CONTROL_raw_VISITS_without_modelling.R'
stop_line <- 72

run_script_until_line(file_path, stop_line)

animales_nvis = animales_nvis %>% 
  mutate(tatu = gsub("@.*", "", Ta)) %>% 
  merge.data.frame(y = microbiota_animals_group, by.x = 'tatu', by.y = 'Subject.ID') %>% 
  filter(tatu %in% !!microbiota_animals,
         FC <= '2022-10-06' )


# Create the boxplot
boxplot(log(animales_nvis$n_visitas) ~ animales_nvis$Control, 
        ylab = "log(N visits)", 
        xlab = "Treatment",
        names = c("Stress", "Control"), 
        main = 'All data for all microbiota animals')  

# Calculate the means
means <- tapply(log(animales_nvis$n_visitas), animales_nvis$Control, mean)

# Add the means to the plot
points(1:length(means), means, col = "red", pch = 19)  # Add points for the means
text(1:length(means), means, labels = round(means, 2), pos = 3)  # Add text with the means

qqnorm(animales_nvis$n_visitas)
qqline(animales_nvis$n_visitas)
qqnorm(animales_nvis$n_visitas %>% log)
qqline(animales_nvis$n_visitas %>% log)
t.test(log(animales_nvis$n_visitas) ~ animales_nvis$Control)
# Duration ----------------------------------------------------------------

run_script_until_line(file_path = 'r/Historico_CONTROL_raw_VISITS_without_modelling.R',
                      stop_line = 143, start_line = 119)

animales_ti = animales_ti %>% 
  mutate(tatu = gsub("@.*", "", Ta)) %>% 
  merge.data.frame(y = microbiota_animals_group, by.x = 'tatu', by.y = 'Subject.ID') %>% 
  filter(tatu %in% !!microbiota_animals,
         FC <= '2022-10-06')


# Create the boxplot
boxplot(animales_ti$Ti ~ animales_ti$Control, 
        ylab = "Feed Duration per Day (s)", 
        xlab = "Treatment",
        names = c("Stress", "Control"), 
        main = 'All data for all microbiota animals')  

# Calculate the means
means <- tapply(animales_ti$Ti, animales_ti$Control, mean)

# Add the means to the plot
points(1:length(means), means, col = "red", pch = 19)  # Add points for the means
text(1:length(means), means, labels = round(means, 2), pos = 3)  # Add text with the means

qqnorm(animales_ti$Ti)
qqline(animales_ti$Ti)
t.test(animales_ti$Ti ~ animales_ti$Control)
# Duration per feed ------------------------------------------------------------

run_script_until_line(file_path = 'r/Historico_CONTROL_raw_VISITS_without_modelling.R',
                      stop_line = 301, start_line = 277)

animales_median_Ti = animales_median_Ti %>% 
  mutate(tatu = gsub("@.*", "", Ta)) %>% 
  merge.data.frame(y = microbiota_animals_group, by.x = 'tatu', by.y = 'Subject.ID') %>% 
  filter(tatu %in% !!microbiota_animals,
         FC <= '2022-10-06')


# Create the boxplot
boxplot(animales_median_Ti$median_Ti ~ animales_median_Ti$Control, 
        ylab = "Duration per feed (s)", 
        xlab = "Treatment",
        names = c("Stress", "Control"), 
        main = 'All data for all microbiota animals')  

# Calculate the means
means <- tapply(animales_median_Ti$median_Ti, animales_median_Ti$Control, mean)

# Add the means to the plot
points(1:length(means), means, col = "red", pch = 19)  # Add points for the means
text(1:length(means), means, labels = round(means, 2), pos = 3)  # Add text with the means

qqnorm(animales_median_Ti$median_Ti)
qqline(animales_median_Ti$median_Ti)
t.test(animales_median_Ti$median_Ti ~ animales_median_Ti$Control)
