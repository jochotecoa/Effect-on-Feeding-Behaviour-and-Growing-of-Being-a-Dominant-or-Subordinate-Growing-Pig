# Reproducibility Check Script
# Run this script to verify data presence and run the main analysis.

# 1. Check Working Directory
if (!file.exists("Effect-on-Feeding-Behaviour-and-Growing-of-Being-a-Dominant-or-Subordinate-Growing-Pig.Rproj")) {
  warning("NOTE: You should run this script from the project root directory.")
}

# 2. Check Data Files
required_files <- c(
  "Registros_animales_Juan.txt",
  "Pesos_Juan.txt",
  "Output_consumo_from_script_comparar_pesos.txt"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(paste("ERROR: Missing data files:\n", paste(missing_files, collapse = "\n"), 
             "\n\nPlease place these files in the project root to proceed."))
} else {
  message("SUCCESS: All data files found.")
}

# 3. Run Analysis
message("Starting Main Analysis...")
tryCatch({
  source("r/ranking_diff_consumo.R")
  message("SUCCESS: Analysis script completed without error.")
}, error = function(e) {
  message("FAILURE: Analysis script encountered an error:")
  message(e$message)
})

