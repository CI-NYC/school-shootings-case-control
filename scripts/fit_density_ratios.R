library(tidyverse)

source("R/density_ratio.R")

exposures <- readRDS("data/derived/school_exposures_reduced.rds")
confounders <- readRDS("data/derived/school_confounders.rds")
outcomes <- readRDS("data/derived/school_outcome_with_weights.rds")
folds <- readRDS("data/derived/crossfit_folds.rds")

schools <- left_join(outcomes, exposures) |> 
  left_join(confounders) |> 
  select(-OutcomeStatus)

make_alternate <- function(data, exposure, a) {
  others <- setdiff(setdiff(names(exposures), c("ID", "GID")), exposure)
  mutate(data, {{ exposure }} := a) |>  
    select(all_of(names(exposures)))
}

A <- setdiff(names(exposures), c("ID", "GID"))

Rr <- vector("list", length(A))
names(Rr) <- A

for (a in A) {
  Rr[[a]] <- fit_density_ratios(
    schools, 
    make_alternate(schools, a, 1), 
    "weights", 
    "GID", 
    folds
  )
}

Rr <- do.call(cbind, Rr)
colnames(Rr) <- A

saveRDS(Rr, "data/derived/density_ratios_1.rds")
