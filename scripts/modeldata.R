library(dplyr)
library(yaml)
library(origami)

schools <- readRDS("data/derived/school_shootings.rds")
school_exposures <- readRDS("data/derived/school_exposures_reduced.rds")

# Load variables file
vars <- read_yaml("scripts/vars.yaml")

covar <- names(vars$confounders)

confounders <- 
  select(schools, ID, GID, any_of(covar)) |> 
  mutate(SCH_FTETEACH_TOT = ifelse(SCH_FTETEACH_TOT == "MissingY" , NA_real_, SCH_FTETEACH_TOT),
         SCH_FTETEACH_TOT = as.numeric(ifelse(SCH_FTETEACH_TOT == "" , NA_real_, SCH_FTETEACH_TOT)),
         TOT_ENR = TOT_ENR_F + TOT_ENR_M, 
         across(c("vot_pdem_idx", "vot_prep_idx"), 
                \(x) ifelse(x == "Missing", NA_real_, x)), 
         across(c("vot_pdem_idx", "vot_prep_idx"), 
                \(x) ifelse(x == 0, NA_real_, x))) |> 
  select(-vot_pdem_idx, -vot_prep_idx, -TOT_ENR_F, -TOT_ENR_M)

confounders <- 
  mutate(confounders, 
         across(where(\(x) any(is.na(x))), \(x) ifelse(is.na(x), 1, 0), .names = "imputed_{.col}"),
         across(where(\(x) any(is.na(x))), \(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

saveRDS(confounders, "data/derived/school_confounders.rds")

outcomes <- 
  select(schools, ID, GID, OutcomeStatus) |> 
  mutate(weights = ifelse(OutcomeStatus == 1, 0.00042, 1 - 0.00042))

saveRDS(outcomes, "data/derived/school_outcome_with_weights.rds")

folds <- make_folds(outcomes, cluster_ids = outcomes$GID)
saveRDS(folds, "data/derived/crossfit_folds.rds")
