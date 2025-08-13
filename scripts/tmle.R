library(tidyverse)
library(kableExtra)
library(glue)

source("R/case-control-tmle.R")

# d1 ----------------------------------------------------------------------

a <- 1

# load outcome data
outcomes <- readRDS("data/derived/school_outcome_with_weights.rds")

# load estimated Riesz representers
Rr <- readRDS(glue("data/derived/density_ratios_{a}.rds"))

# load estimated outcome regression
outcome_regressions <- 
  readRDS(glue("data/derived/outcome_regression_predictions_{a}.rds"))

estimates_d1 <- map(colnames(Rr), \(x) tmle(outcomes$OutcomeStatus, x, outcome_regressions, Rr, outcomes$weights)) 

names(estimates_d1) <- colnames(Rr)

# d0 ----------------------------------------------------------------------

a <- 0

# load outcome data
outcomes <- readRDS("data/derived/school_outcome_with_weights.rds")

# load estimated Riesz representers
Rr <- readRDS(glue("data/derived/density_ratios_{a}.rds"))

# load estimated outcome regression
outcome_regressions <- 
  readRDS(glue("data/derived/outcome_regression_predictions_{a}.rds"))

estimates_d0 <- map(colnames(Rr), \(x) tmle(outcomes$OutcomeStatus, x, outcome_regressions, Rr, outcomes$weights)) 

names(estimates_d0) <- colnames(Rr)

# \theta_1 ----------------------------------------------------------------

cm <- sum(1 / seq_along(estimates))

rd_theta1 <- 
  map(estimates_d1, \(x) x - 0.00042) |> 
  map(ife::tidy) |> 
  list_rbind(names_to = "var")

rd_theta1$p.value <- pnorm(abs(rd_theta1$estimate) / rd_theta1$std.error, lower.tail = FALSE) * 2

rd_theta1 <- 
  arrange(rd_theta1, p.value) |> 
  mutate(BY_cv = (row_number() / n()) * 0.05 * cm, 
         BY_reject = p.value <= BY_cv)

logrr_theta1 <- 
  map(estimates, \(x) log(x / 0.00042)) |> 
  map(ife::tidy) |> 
  list_rbind(names_to = "var")

logrr_theta1$p.value <- pnorm(abs(logrr_theta1$estimate) / logrr_theta1$std.error, lower.tail = FALSE) * 2

rr_theta1 <- 
  arrange(logrr_theta1, p.value) |> 
  mutate(BY_cv = (row_number() / n()) * 0.05 * cm, 
         BY_reject = p.value <= BY_cv)

rr_theta1 <- mutate(rr_theta1, across(c(estimate, conf.low, conf.high), exp))

# \theta_2 ----------------------------------------------------------------

rd_theta2 <- 
  map2(estimates_d1, estimates_d0, \(x, y) x - y) |> 
  map(ife::tidy) |> 
  list_rbind(names_to = "var")

rd_theta2 <- mutate(rd_theta2, p.value = pnorm(abs(estimate) / std.error, lower.tail = FALSE) * 2)

rd_theta2 <- 
  arrange(rd_theta2, p.value) |> 
  mutate(BY_cv = (row_number() / n()) * 0.05 * cm, 
         BY_reject = p.value <= BY_cv)

logrr_theta2 <- 
  map2(estimates_d1, estimates_d0, \(x, y) log(x / y)) |> 
  map(ife::tidy) |> 
  list_rbind(names_to = "var")

logrr_theta2$p.value <- pnorm(abs(logrr_theta2$estimate) / logrr_theta2$std.error, lower.tail = FALSE) * 2

rr_theta2 <- 
  arrange(logrr_theta2, p.value) |> 
  mutate(BY_cv = (row_number() / n()) * 0.05 * cm, 
         BY_reject = p.value <= BY_cv)

rr_theta2 <- mutate(rr_theta2, across(c(estimate, conf.low, conf.high), exp))

# to latex ----------------------------------------------------------------

format_sci_latex <- function(x, digits = 2) {
  if (x == 0) return("0")
  exponent <- floor(log10(abs(x)))
  mantissa <- x / (10^exponent)
  sprintf("%.2f $\\times$ 10$^{%d}$", mantissa, exponent)
}

map(estimates_d0, \(x) ife::tidy(x * 100)) |> 
  list_rbind(names_to = "var") |> 
  mutate(across(where(is.numeric), \(x) round(x, 4))) |> 
  select(-std.error) |> 
  mutate(across(where(is.numeric), \(x) paste0(x, "%"))) |> 
  kable(format = "latex", booktabs = TRUE, linesep = "", 
        col.names = c("", "Estimate", "LB", "UB"))

mutate(rd_theta2, across(c(estimate, conf.low, conf.high), \(x) map_chr(x, format_sci_latex)), 
       across(c(p.value, BY_cv), \(x) round(x, 3))) |> 
  select(-std.error) |> 
  kable(format = "latex", booktabs = TRUE, linesep = "", 
        col.names = c("", "Risk difference", "LB", "UB", "p-value", "BY cv", "Reject null"), 
        escape = FALSE)

mutate(rr_theta1, across(where(is.numeric), \(x) round(x, 3))) |> 
  select(-std.error) |> 
  kable(format = "latex", booktabs = TRUE, linesep = "", 
        col.names = c("", "Risk ratio", "LB", "UB", "p-value", "BY cv", "Reject null"))
         