#############
### SETUP ###
#############

library(tidyverse)
library(naniar)
library(striprtf)

########################
### HELPER FUNCTIONS ###
########################

# mean with NA handling
auto_mean = function(...) rowMeans(cbind(...), na.rm = TRUE)
# sum with NA handling
auto_sum = function(...) rowSums(cbind(...), na.rm = TRUE)

#################
### LOAD DATA ###
#################

cat("loading childhood data...\n")
childhood_df = read_tsv("../data/raw/ncds-childhood/tab/ncds0123.tab")
cat("childhood data loaded.\n")

cat("loading age 42 data...\n")
age42_df = read_tsv("../data/raw/ncds-42/tab/ncds6_v2.tab")
cat("age 42 data loaded.\n")

# merge datasets and keep only people who appear in both waves
cat("merging datasets...\n")
df = childhood_df |> 
  inner_join(age42_df |> select(ncdsid, nvqlev, nvqlev2, nvqlev3, nvqlev4, nvqlev5, nvqlev6), by = "ncdsid")
# sample size reduction: 18,554 → 11,417 participants (dropped 7,137 who don't appear in both waves)
cat("merged dataset has", nrow(df), "participants who appear in both waves.\n")

#########################
### EXTRACT VARIABLES ###
#########################

# 1. reading comprehension test (n923)
# 2. mathematics test (n926)
# 3. teacher ratings (n65, n67, n68, n70)
# 4. externalising (n2310, n2311, n2314, n2315, n2309, n2312)
# 5. internalising (n2313, n2312, n2311)
# 6. bsag syndromes (n436, n438, n440, n442, n444, n446, n448, n450, n452, n454, n455)
# 7. unstructured socialising (n933, n934)
# 8. quiet hobbies (n935, n936, n937, n938, n939, n940)
# 9. school clubs & sport (n941)
# 10. future aspirations (n932)
# 11. father manual (derived)
# 12. parents left school ≤15 (derived)
# 13. non-intact family (derived)
# 14. free-lunch proxy (derived)

# select all needed variables
needed_vars = c(
  "ncdsid", "n622", # id and sex
  "n923", "n926", # reading, math
  "n65", "n67", "n68", "n70", # teacher ratings
  "n2310", "n2311", "n2314", "n2315", "n2309", "n2312", # externalising
  "n2313", # internalising
  "n436", "n438", "n440", "n442", "n444", "n446", "n448", "n450", "n452", "n454", "n455", # bsag
  "n933", "n934", # unstructured socialising
  "n935", "n936", "n937", "n938", "n939", "n940", # quiet hobbies
  "n941", # school clubs/sport
  "n932", # aspirations
  # variables for derived covariates:
  "n190", "n1685", "n2384", # father occupation
  "n266", "n267", # parent education
  "n2354", # family structure
  "n858", # free lunch
  # age 42 outcome variables:
  "nvqlev", "nvqlev2", "nvqlev3", "nvqlev4", "nvqlev5", "nvqlev6" # NVQ levels
)

# subset to needed variables
merged_data = df |> select(any_of(needed_vars))

# teacher ratings: mean of 4 items
merged_data = merged_data |> mutate(
  teacher_ability = auto_mean(n65, n67, n68, n70)
)

# externalising: mean of 6 items
merged_data = merged_data |> mutate(
  externalising = auto_mean(n2310, n2311, n2314, n2315, n2309, n2312)
)

# internalising: mean of 3 items
merged_data = merged_data |> mutate(
  internalising = auto_mean(n2313, n2312, n2311)
)

# bsag adjustment: total score (n455)
merged_data = merged_data |> mutate(
  bsag_adjustment = n455
)

# unstructured socialising: mean of 2 items
merged_data = merged_data |> mutate(
  unstructured_socialising = auto_mean(n933, n934)
)

# quiet hobbies: mean of 6 items
merged_data = merged_data |> mutate(
  quiet_hobbies = auto_mean(n935, n936, n937, n938, n939, n940)
)

# school clubs & sport: n941
merged_data = merged_data |> mutate(
  school_clubs_sports = n941
)

# future aspirations: recode n932
merged_data = merged_data |> mutate(
  asp_get_job = as.integer(n932 == 1),
  asp_study = as.integer(n932 == 2),
  asp_uncertain = as.integer(n932 == 3)
)

# father manual: derived from n190, n1685, n2384
# (manual if any of these == 1, as in paper)
merged_data = merged_data |> mutate(
  father_manual = as.integer(if_any(c(n190, n1685, n2384), ~ . == 1))
)

# parents left school ≤15: derived from n266, n267
merged_data = merged_data |> mutate(
  parents_edgt15 = as.integer(if_any(c(n266, n267), ~ . <= 15 & . > 0))
)

# non-intact family: n2354 == 1
merged_data = merged_data |> mutate(
  nonintact = as.integer(n2354 == 1)
)

# free-lunch proxy: n858 == 1
merged_data = merged_data |> mutate(
  free_lunch = as.integer(n858 == 1)
)

# reading/math test scores
merged_data = merged_data |> rename(
  reading_test = n923,
  math_test = n926
)

######################
### TREATMENT VARS ###
######################

# add alcohol consumption variables if present
alc_vars = c("n2889", "n2890", "n2891")
if (all(alc_vars %in% names(df))) {
  merged_data = merged_data |> 
    mutate(
      alc_slot1 = case_when(
        df$n2889 == -1           ~  0,
        df$n2889 ==  1           ~  2,
        df$n2889 ==  2           ~  1,
        df$n2889 ==  4           ~  1,
        df$n2889 ==  5           ~  2,
        df$n2889 ==  6           ~  3,
        df$n2889 ==  7           ~  4,
        df$n2889 ==  8           ~  1,
        df$n2889 ==  9           ~  2,
        df$n2889 == 10           ~  4,
        df$n2889 == 11           ~  6,
        df$n2889 == 12           ~  8,
        TRUE                     ~ NA_real_
      ),
      alc_slot2 = case_when(
        df$n2890 == -1           ~ NA_real_,
        df$n2890 ==  0           ~  0,
        df$n2890 ==  1           ~  2,
        df$n2890 ==  2           ~  1,
        df$n2890 ==  4           ~  1,
        df$n2890 ==  5           ~  2,
        df$n2890 ==  6           ~  3,
        df$n2890 ==  7           ~  4,
        df$n2890 ==  8           ~  1,
        df$n2890 ==  9           ~  2,
        df$n2890 == 10           ~  4,
        df$n2890 == 11           ~  6,
        df$n2890 == 12           ~  8,
        TRUE                     ~ NA_real_
      ),
      alc_slot3 = case_when(
        df$n2891 == -1           ~ NA_real_,
        df$n2891 ==  0           ~  0,
        df$n2891 ==  1           ~  2,
        df$n2891 ==  2           ~  1,
        df$n2891 ==  4           ~  1,
        df$n2891 ==  5           ~  2,
        df$n2891 ==  6           ~  3,
        df$n2891 ==  7           ~  4,
        df$n2891 ==  8           ~  1,
        df$n2891 ==  9           ~  2,
        df$n2891 == 10           ~  4,
        df$n2891 == 11           ~  6,
        df$n2891 == 12           ~  8,
        TRUE                     ~ NA_real_
      )
    ) |>
    mutate(
      treatment_cont = case_when(
        is.na(alc_slot1) & is.na(alc_slot2) & is.na(alc_slot3) ~ NA_real_,
        TRUE ~ rowSums(cbind(alc_slot1, alc_slot2, alc_slot3), na.rm = TRUE)
      ),
      treatment_bin = case_when(
        is.na(treatment_cont) ~ NA_real_,
        n622 == 1 & treatment_cont >= 5 ~ 1,
        n622 == 2 & treatment_cont >= 4 ~ 1,
        TRUE ~ 0
      )
    )
}

####################
### OUTCOME VARS ###
####################

# create post-secondary degree outcome at age 42
merged_data = merged_data |>
  mutate(
    across(c(nvqlev, nvqlev2, nvqlev3, nvqlev4, nvqlev5, nvqlev6), 
           ~na_if(na_if(na_if(na_if(., 7), 8), 98), 99)),
    max_nvq = pmax(nvqlev, nvqlev2, nvqlev3, nvqlev4, nvqlev5, nvqlev6, na.rm = TRUE),
    postsec_deg42 = case_when(
      is.infinite(max_nvq) ~ NA_integer_,
      max_nvq >= 4 ~ 1L,
      TRUE ~ 0L
    )
  )

############
### SAVE ###
############

final_vars = c(
  "ncdsid", "n622", "reading_test", "math_test", "teacher_ability", "externalising", "internalising",
  "bsag_adjustment", "unstructured_socialising", "quiet_hobbies", "school_clubs_sports",
  "asp_get_job", "asp_study", "asp_uncertain", "father_manual", "parents_edgt15", "nonintact", "free_lunch",
  # treatment variables
  "treatment_bin", "treatment_cont",
  # outcome variable
  "postsec_deg42"
)

final_data = merged_data |> select(any_of(final_vars))

cat("saving processed data...\n")
write_csv(final_data, "../data/processed/paper_vars.csv")
cat("done.\n") 