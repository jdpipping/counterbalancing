#############
### SETUP ###
#############

library(tidyverse)
library(naniar)

#################
### LOAD DATA ###
#################

cat("loading paper-aligned processed data...\n")
data = read_csv("../data/processed/paper_vars.csv")
cat("data loaded.\n")

# sample size information:
# - total participants in dataset: 11,417 (after restricting to both waves)
# - participants with complete data: 9,074 (79.5%)
# - participants with missing data: 2,343 (20.5%)

###########################
### BASIC DATA OVERVIEW ###
###########################

cat("=== basic data overview ===\n")
cat("dataset dimensions:", nrow(data), "participants,", ncol(data), "variables\n")

# show variable names
cat("variables included:\n")
print(names(data))

###########################
### MISSINGNESS SUMMARY ###
###########################

cat("=== missingness summary ===\n")
miss_summary = miss_var_summary(data)
print(miss_summary)

# detailed missingness analysis
cat("\n=== detailed missingness analysis ===\n")
cat("Total observations:", nrow(data), "\n")
cat("Complete cases (no missing on any variable):", sum(complete.cases(data)), "\n")
cat("Complete cases percentage:", round(100 * sum(complete.cases(data)) / nrow(data), 1), "%\n")

cat("\nNon-missing counts by variable:\n")
for(var in names(data)) {
  non_missing = sum(!is.na(data[[var]]))
  pct = round(100 * non_missing / nrow(data), 1)
  cat(sprintf("%-25s: %5d (%5.1f%%)\n", var, non_missing, pct))
}

#######################################
### DISTRIBUTIONS FOR KEY VARIABLES ###
#######################################

cat("=== variable distributions ===\n")
# continuous variables
cont_vars = c("reading_test", "math_test", "teacher_ability", "externalising", "internalising", "bsag_adjustment", "unstructured_socialising", "quiet_hobbies", "school_clubs_sports")
cont_vars = cont_vars[cont_vars %in% names(data)]

for (v in cont_vars) {
  cat("\n", v, ":\n")
  print(summary(data[[v]]))
}

# binary/dummy variables
bin_vars = c("asp_get_job", "asp_study", "asp_uncertain", "father_manual", "parents_edgt15", "nonintact", "free_lunch", "treatment_bin")
bin_vars = bin_vars[bin_vars %in% names(data)]

for (v in bin_vars) {
  cat("\n", v, ":\n")
  print(table(data[[v]], useNA = "always"))
}

###################################
### ASSOCIATIONS WITH TREATMENT ###
###################################

cat("=== associations with treatment ===\n")
if ("treatment_bin" %in% names(data)) {
  for (v in setdiff(names(data), c("ncdsid", "treatment_bin", "treatment_cont", "n622"))) {
    if (is.numeric(data[[v]])) {
      m = tapply(data[[v]], data$treatment_bin, mean, na.rm = TRUE)
      cat("mean of", v, "by treatment_bin:", paste(names(m), round(m, 2), sep = ": ", collapse = ", "), "\n")
    } else {
      tab = table(data[[v]], data$treatment_bin, useNA = "always")
      cat("table of", v, "by treatment_bin:\n")
      print(tab)
    }
  }
}

cat("exploration complete.\n") 