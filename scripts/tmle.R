library(tidyverse)
library(kableExtra)
library(gt)

source("R/case-control-tmle.R")

# load outcome data
outcomes <- readRDS("data/derived/school_outcome_with_weights.rds")

# load estimated Riesz representers
Rr <- readRDS("data/derived/density_ratios.rds")

# load estimated outcome regression
outcome_regressions <- 
  readRDS("data/derived/outcome_regression_predictions.rds")

estimates <- map(colnames(Rr), \(x) tmle(outcomes$OutcomeStatus, x, outcome_regressions, Rr, outcomes$weights)) 

names(estimates) <- colnames(Rr)

cm <- sum(1 / seq_along(estimates))

rd <- 
  map(estimates, \(x) x - 0.00042) |> 
  map(ife::tidy) |> 
  list_rbind(names_to = "var")

rd$p.value <- pnorm(abs(rd$estimate) / rd$std.error, lower.tail = FALSE) * 2

rd <- 
  arrange(rd, p.value) |> 
  mutate(BY_cv = (row_number() / n()) * 0.05 * cm, 
         BY_reject = p.value <= BY_cv)

logrr <- 
  map(estimates, \(x) log(x / 0.00042)) |> 
  map(ife::tidy) |> 
  list_rbind(names_to = "var")

logrr$p.value <- pnorm(abs(logrr$estimate) / logrr$std.error, lower.tail = FALSE) * 2

rr <- 
  arrange(logrr, p.value) |> 
  mutate(BY_cv = (row_number() / n()) * 0.05 * cm, 
         BY_reject = p.value <= BY_cv)

rr <- mutate(rr, across(c(estimate, conf.low, conf.high), exp))

# to latex ----------------------------------------------------------------

format_sci_latex <- function(x, digits = 2) {
  if (x == 0) return("0")
  exponent <- floor(log10(abs(x)))
  mantissa <- x / (10^exponent)
  sprintf("%.2f $\\times$ 10$^{%d}$", mantissa, exponent)
}

map(estimates, \(x) ife::tidy(x * 100)) |> 
  list_rbind(names_to = "var") |> 
  mutate(across(where(is.numeric), \(x) round(x, 4))) |> 
  select(-std.error) |> 
  mutate(across(where(is.numeric), \(x) paste0(x, "%"))) |> 
  kable(format = "latex", booktabs = TRUE, linesep = "", 
        col.names = c("", "Estimate", "LB", "UB"))

mutate(rd, across(c(estimate, conf.low, conf.high), \(x) map_chr(x, format_sci_latex)), 
       across(c(p.value, BY_cv), \(x) round(x, 3))) |> 
  select(-std.error) |> 
  kable(format = "latex", booktabs = TRUE, linesep = "", 
        col.names = c("", "Risk difference", "LB", "UB", "p-value", "BY cv", "Reject null"), 
        escape = FALSE)

mutate(rr, across(is.numeric, \(x) round(x, 3))) |> 
  select(-std.error) |> 
  kable(format = "latex", booktabs = TRUE, linesep = "", 
        col.names = c("", "Risk ratio", "LB", "UB", "p-value", "BY cv", "Reject null"))
         