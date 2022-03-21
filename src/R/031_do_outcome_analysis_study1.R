library(here)

source(here("src", "R", "030_outcome_analysis_study1.R"))

# Initial analysis. No dropping duplicates
do_outcome_analysis(
  file.path(MATCHES_DIR, "dat_plus_matches_study1.rda"),
  file.path(MATCHES_DIR, "outcome_analysis_study1.rda")
)

# Analysis keeping only first responses
do_outcome_analysis(
  file.path(MATCHES_DIR, "dat_plus_matches_study1_first.rda"),
  file.path(MATCHES_DIR, "outcome_analysis_study1_first.rda")
)

# Analysis keeping only last responses
do_outcome_analysis(
  file.path(MATCHES_DIR, "dat_plus_matches_study1_last.rda"),
  file.path(MATCHES_DIR, "outcome_analysis_study1_last.rda")
)
