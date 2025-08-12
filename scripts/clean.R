library(tidyverse)
library(haven)
library(yaml)

# Load raw data
schools <- read_stata("data/source/NICHD Index database V7 20250718.dta")

# Load variables file
vars <- read_yaml("scripts/vars.yaml")

# Cleaning
schools <- 
  mutate(schools, OutcomeStatus = ifelse(OutcomeStatus == "Case", 1, 0)) |> 
  
  # Imputing missing values as 0... TO DISCUSS
  mutate(across(all_of(as.vector(unlist(vars$exposures))), ~ case_when(
    .x == "Yes" ~ 1, 
    .x == "No" ~ 0,
    TRUE ~ 0)
  )) |> 
  select(ID, GID, State, Urbancity, Schoollevel, OutcomeStatus, 
         all_of(as.vector(unlist(vars$exposures))), 
         any_of(names(vars$confounders)))

saveRDS(schools, "data/derived/school_shootings.rds")
