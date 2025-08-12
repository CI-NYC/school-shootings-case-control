library(ife)

tmle <- function(Y, exposure, outcome_regressions, riesz, weights) {
  density_ratio <- riesz[, exposure]
  observed <- outcome_regressions[, "observed"]
  shifted <- outcome_regressions[, exposure]
  # for (i in 1:10) {
    fit <- glm(Y ~ -1 + offset(qlogis(observed)) + density_ratio, 
               weights = weights, family = "binomial")
    eps <- coef(fit)
    shifted <- plogis(density_ratio*eps + qlogis(shifted))
    observed <- plogis(density_ratio*eps + qlogis(observed)) 
  # }
  eif <- density_ratio * (Y - observed) + shifted - weighted.mean(shifted, weights)
  ife(weighted.mean(shifted, weights), eif, weights = weights)
}

onestep <- function(Y, exposure, outcome_regressions, riesz, weights) {
  density_ratio <- riesz[, exposure]
  observed <- outcome_regressions[, "observed"]
  shifted <- outcome_regressions[, exposure]
  eif <- density_ratio * (Y - observed) + shifted
  theta <- weighted.mean(eif, weights)
  ife(weighted.mean(eif, weights), eif, weights = weights)
}
