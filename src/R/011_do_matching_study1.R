#' This file actually runs all of our analyses with different levesl of
#' duplicate dropping

library(here)
source(here("src", "R", "010_create_matched_design_study1.R"))

# Original analysis. No dupe dropping
do_matching(
  file.path(DATA_DIR, "MERGE_NR_2.3.21.csv"),
  file.path(MATCHES_DIR, "dat_plus_matches_study1.rda"),
  file.path(MATCHES_DIR, "datasets_study1.rda")
)

# Same analysis, but only keeping the first in time matches
## debugonce(do_matching) ## use this to fine tune the research design for example to choose pair_limits
## Values below chosen to restrict extreme matches and ensure close matches. See the code itself.
do_matching(
  file.path(DATA_DIR, "deduplicated_data_first.csv"),
  file.path(MATCHES_DIR, "dat_plus_matches_study1_first.rda"),
  file.path(MATCHES_DIR, "datasets_study1_first.rda"),
  pair_limits = c(
    agegood = 10,
    ideo5_Imp = 2.5,
    educ = 1,
    faminc_new_Imp = 3,
    faminc_new.NA = 1,
    trust_in_science = .4,
    relig_scale = .451,
    covid_know = .375,
    covid_subj_know = .5
  )
)

# Same analysis, but only keeping the *last* in time matches
do_matching(
  file.path(DATA_DIR, "deduplicated_data_last.csv"),
  file.path(MATCHES_DIR, "dat_plus_matches_study1_last.rda"),
  file.path(MATCHES_DIR, "datasets_study1_last.rda")
)
