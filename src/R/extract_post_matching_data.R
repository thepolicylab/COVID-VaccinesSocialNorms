## Extract datasets used in final analyses as csv files for use in other
## programs

library(tidyverse)
library(here)
source(here("src", "R", "000_constants_and_utils.R"))
source(here("src/R", "rmd_setup.R"))

## Study 1 final dataset is called dat3
load(file = here(MATCHES_DIR, "outcome_analysis_study1.rda"),verbose=TRUE)
write_csv(dat3,file=here(DATA_DIR, "study1_dat_plus_matches_dat3.csv"))

## Study 2 final dataset is called dat5
load(file = here(MATCHES_DIR, "outcome_analysis_study2.rda"),verbose=TRUE)
write_csv(dat5,file=here(DATA_DIR, "study1_dat_plus_matches_dat5.csv"))


