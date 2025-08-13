library(tidyverse)

source("R/density_ratio.R")

exposures <- readRDS("data/derived/school_exposures_reduced.rds")
confounders <- readRDS("data/derived/school_confounders.rds")
outcomes <- readRDS("data/derived/school_outcome_with_weights.rds")
folds <- readRDS("data/derived/crossfit_folds.rds")

schools <- left_join(outcomes, exposures) |> 
  left_join(confounders)

no_folds <- length(folds)
fits <- vector("list", no_folds) 

X <- setdiff(c(names(exposures), names(confounders)), c("ID", "GID"))

SL.1se.glmnet = function(...) {
  SL.glmnet(..., useMin = FALSE)
}

for (v in seq_len(no_folds)) {
  train <- schools[folds[[v]]$training_set, ]
  
  fits[[v]] <- SuperLearner(
    Y = train$OutcomeStatus, 
    X = select(train, all_of(X)), 
    family = "binomial", 
    SL.library = c("SL.glm", "SL.1se.glmnet"),
    id = train$GID, 
    obsWeights = train$weights
  )
}

saveRDS(fits, "data/derived/outcome_regressions.rds")

make_alternate <- function(data, exposure, a) {
  others <- setdiff(setdiff(names(exposures), c("ID", "GID")), exposure)
  mutate(data, {{ exposure }} := a) |>  
    select(any_of(X))
}

preds <- matrix(nrow = nrow(schools), ncol = ncol(exposures) - 1)
colnames(preds) <- c("observed", setdiff(names(exposures), c("ID", "GID")))

for (v in seq_len(no_folds)) {
  valid <- schools[folds[[v]]$validation_set, ]
  model <- fits[[v]]
  for (a in colnames(preds)) {
    if (a == "observed") {
      alternate <- select(valid, all_of(X))
    } else {
      alternate <- make_alternate(valid, a, 1)
    }
    preds[folds[[v]]$validation_set, a] <- predict(model, alternate)$pred
  }
}

saveRDS(preds, "data/derived/outcome_regression_predictions_1.rds")
