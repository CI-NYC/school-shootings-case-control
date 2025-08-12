library(tidyverse)
library(arules)
library(yaml)

schools <- readRDS("data/derived/school_shootings.rds")

# Load variables file
vars <- read_yaml("scripts/vars.yaml")

school_exposures <-   
  select(schools, all_of(as.vector(unlist(vars$exposures))))

exposure_props <- lapply(school_exposures, mean) |> unlist()
# Remove exposures uncommon exposures
exposure_props <- exposure_props[which(exposure_props > 0.15)]
# Remove too common of exposures
exposure_props <- exposure_props[which(exposure_props < 0.85)]

school_exposures <- select(schools, ID, GID, all_of(names(exposure_props)))

school_transactions <- 
  select(school_exposures, all_of(names(exposure_props))) |> 
  as.matrix() |> 
  transactions()

rules <- apriori(school_transactions, parameter = list(conf = 0.9, maxlen = 3))
inspect(rules)

# Collapsing ID variables
school_exposures <- 
  mutate(school_exposures, 
         MON_IDENTIFICATION = sum(MON_IDT, MON_IDS, MON_VIS, MON_IDN) > 1) |> 
  select(-MON_IDT, -MON_IDS, -MON_VIS, -MON_IDN)

# All schools have some sort of identification, so just going to remove
school_exposures <- select(school_exposures, -MON_IDENTIFICATION)

# class_rules <- function(vars, conf = 0.9, maxlen) {
#   school_transactions <- 
#     select(school_exposures, any_of(vars)) |> 
#     as.matrix() |> 
#     transactions()
#   
#   if (missing(maxlen)) maxlen <- length(vars)
#   
#   rules <- apriori(school_transactions, 
#                    parameter = list(conf = conf, maxlen = maxlen))
#   
#   inspect(rules)
# }
# 
# class_rules(names(school_exposures), 0.85, 2)

saveRDS(school_exposures, "data/derived/school_exposures_reduced.rds")
