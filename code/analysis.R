#############
### SETUP ###
#############

library(tidyverse)
library(MatchIt)
library(cobalt)

#################
### LOAD DATA ###
#################

cat("loading paper-aligned processed data...\n")
data = read_csv("../data/processed/paper_vars.csv")
cat("data loaded.\n")

#####################################
### DEFINE VARIABLES FOR ANALYSIS ###
#####################################

cat("defining variables for analysis...\n")

# covariates (all except id, sex, treatment, and outcomes)
covariates = c(
  "teacher_ability", "externalising", "internalising",
  "bsag_adjustment", "unstructured_socialising", "quiet_hobbies", "school_clubs_sports",
  "asp_get_job", "asp_study", "asp_uncertain", "father_manual", "parents_edgt15", "nonintact", "free_lunch"
)
covariates = covariates[covariates %in% names(data)]

# treatment variable
treatment_var = "treatment_bin"

# outcome variable(s) (use age 42 post-secondary degree attainment)
outcome_vars = c("postsec_deg42")
outcome_vars = outcome_vars[outcome_vars %in% names(data)]

# sample size before complete case restriction: 11,417 participants
# restrict to complete cases for covariates and treatment
analysis_data = data |> select(ncdsid, all_of(treatment_var), all_of(covariates), all_of(outcome_vars)) |> drop_na()
# sample size after complete case restriction: 9,074 participants (dropped 2,343 due to missing data)

cat("analysis dataset dimensions:", nrow(analysis_data), ncol(analysis_data), "\n")

##########################
### BALANCE ASSESSMENT ###
##########################

cat("=== balance assessment ===\n")

# Only use numeric covariates for SMD
num_covariates = covariates[sapply(analysis_data[, covariates], is.numeric)]

if (length(num_covariates) > 0) {
  # Standardize numeric covariates using scale()
  analysis_data_scaled = analysis_data |>
    mutate(across(all_of(num_covariates), ~scale(.)[,1]))
  
  # Calculate mean differences between treatment groups
  balance_summary = analysis_data_scaled |>
    group_by(!!sym(treatment_var)) |>
    summarise(across(all_of(num_covariates), ~mean(., na.rm = TRUE))) |>
    pivot_longer(-!!sym(treatment_var), names_to = "variable", values_to = "mean_scaled") |>
    pivot_wider(names_from = !!sym(treatment_var), values_from = mean_scaled, names_prefix = "group_") |>
    mutate(std_diff = group_1 - group_0) |>
    arrange(desc(abs(std_diff)))
  
  cat("standardized mean differences (treatment vs control):\n")
  print(head(balance_summary, 10))
} else {
  cat("No numeric covariates for SMD calculation.\n")
}

#########################
### MATCHING ANALYSIS ###
#########################

cat("=== matching analysis ===\n")

match_formula = as.formula(paste(treatment_var, "~", paste(covariates, collapse = " + ")))
match_result = matchit(match_formula, data = analysis_data, method = "nearest", ratio = 1, caliper = 0.25)

# assess balance after matching
balance_after = bal.tab(match_result, un = TRUE)
cat("balance after matching:\n")
print(balance_after$Balance)

matched_data = match.data(match_result)
cat("matched dataset dimensions:", nrow(matched_data), ncol(matched_data), "\n")

#######################
### SURPRISE SCORES ###
#######################

# f: number of top surprise scores to show
f = 25

# compute surprise score: abs(treatment - propensity)
matched_data = matched_data |>
  mutate(surprise_score = abs(!!sym(treatment_var) - distance))

# get top f and bottom f most surprising individuals
cat("\n=== top", f, "most surprising individuals ===\n")
top_individuals = matched_data |>
  arrange(desc(surprise_score)) |>
  head(f)
print(top_individuals |> select(ncdsid, !!treatment_var, distance, surprise_score))

cat("\n=== bottom", f, "least surprising individuals ===\n")
bottom_individuals = matched_data |>
  arrange(surprise_score) |>
  head(f)
print(bottom_individuals |> select(ncdsid, !!treatment_var, distance, surprise_score))

# save datasets for analysis
write_csv(top_individuals, "../data/processed/top_surprising_individuals.csv")
write_csv(bottom_individuals, "../data/processed/bottom_surprising_individuals.csv")
cat("\nsaved top and bottom", f, "surprising individuals to csv files\n")

# get top f most surprising pairs (sum of surprise scores)
cat("\n=== top", f, "most surprising pairs (sum of surprise scores) ===\n")
pair_surprise = matched_data |>
  group_by(weights) |>
  mutate(
    pair_surprise_sum = sum(surprise_score),
    pair_size = n()
  ) |>
  ungroup() |>
  filter(pair_size == 2) |>
  arrange(desc(pair_surprise_sum)) |>
  select(ncdsid, !!treatment_var, distance, surprise_score, weights, pair_surprise_sum) |>
  head(f * 2)  # show both members of top f pairs
print(pair_surprise)

########################
### OUTCOME ANALYSIS ###
########################

cat("=== outcome analysis ===\n")

for (outcome in outcome_vars) {
  cat("analyzing outcome:", outcome, "\n")
  outcome_test = t.test(as.formula(paste(outcome, "~", treatment_var)), data = matched_data)
  cat("treatment effect (t-test):\n")
  cat("t-statistic:", outcome_test$statistic, "\n")
  cat("p-value:", outcome_test$p.value, "\n")
  cat("mean difference:", diff(outcome_test$estimate), "\n")
}

######################################
### AVERAGE TREATMENT EFFECT (ATE) ###
######################################

cat("=== average treatment effect (ate) ===\n")

# calculate ATE using linear regression with covariates
for (outcome in outcome_vars) {
  cat("calculating ATE for:", outcome, "\n")
  
  # formula with all covariates
  formula_str = paste(outcome, "~", treatment_var, "+", paste(covariates, collapse = " + "))
  formula_obj = as.formula(formula_str)
  
  # fit regression model
  model = lm(formula_obj, data = analysis_data)
  
  # extract treatment coefficient (ATE)
  treatment_coef = coef(model)[treatment_var]
  treatment_se = summary(model)$coefficients[treatment_var, "Std. Error"]
  treatment_p = summary(model)$coefficients[treatment_var, "Pr(>|t|)"]
  
  cat("ATE estimate:", round(treatment_coef, 4), "\n")
  cat("Standard error:", round(treatment_se, 4), "\n")
  cat("P-value:", round(treatment_p, 4), "\n")
  cat("95% CI:", round(treatment_coef - 1.96*treatment_se, 4), "to", round(treatment_coef + 1.96*treatment_se, 4), "\n")
  cat("\n")
}

cat("analysis complete.\n")