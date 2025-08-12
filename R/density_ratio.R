library(SuperLearner)
library(dplyr)

fit_density_ratios <- function(data, alternate, weights, id, folds) {
  no_folds <- length(folds)
  Rr <- matrix(nrow = nrow(data), ncol = 1)
  covar <- setdiff(names(data), names(alternate))
  exposures <- setdiff(names(alternate), c("ID", "GID"))
  
  data <- mutate(data, delta = 0)
  
  alternate <- select(data, ID, GID, all_of(covar)) |> 
    left_join(alternate) |> 
    mutate(delta = 1)

  for (v in seq_len(no_folds)) {
    train_observed <- data[folds[[v]]$training_set, ]
    train_alternate <- alternate[folds[[v]]$training_set, ]
    valid <- data[folds[[v]]$validation_set, ]
    
    train <- bind_rows(
      select(train_observed, all_of(names(alternate))), 
      train_alternate
    )

    fit <- SuperLearner(Y = train$delta, 
                        X = select(train, setdiff(covar, c(weights, id)), 
                                   all_of(exposures)), 
                        family = "binomial", 
                        SL.library = c("SL.glm", "SL.glmnet"), 
                        id = train[[id]], 
                        obsWeights = train[[weights]])

    pred <- predict(fit, 
                    select(valid, setdiff(covar, c(weights, id)), 
                           all_of(exposures)), 
                    onlySL = TRUE)$pred
    Rr[folds[[v]]$validation_set, ] <- pred / (1 - pred)
  }
  
  Rr
}

# test <- select(schools, GID, ETH_ILL, ETH_LOC)
# test2 <- mutate(test, ETH_LOC = 1) |> select(ETH_LOC)
# 
# fit_density_ratios(test, test2, NULL, "GID", make_folds(test, cluster_ids = test$GID))
