## Identify people who were interviewed in both november and december and save a
## file with an indicator for who these people are.

library(here)
source(here("src", "R", "000_constants_and_utils.R"))

library(tidyverse)

id_df <- read.csv(file.path(DATA_DIR, "BROW0016_OUTPUT_20220207.csv"))
id_df <- id_df %>%
  select(
    caseid,
    caseid_13,
    caseid_14,
    caseid_15,
  ) %>%
  mutate(user_id = row_number()) %>%
  rename(caseid_16 = caseid) %>%
  pivot_longer(
    cols = starts_with("caseid"),
    names_to = "which_survey",
    values_to = "caseid",
  ) %>%
  drop_na()

# Focus only on the surveys we care about
# Read in the survey responses data for Study 1
data_df <- read.csv(file.path(DATA_DIR, "MERGE_NR_2.3.21.csv")) %>%
  filter(survey %in% c(5, 6, 7, 8))

data_df %>%
  left_join(id_df, by = "caseid") %>%
  group_by(which_survey) %>%
  count()

merged_df <- data_df %>%
  left_join(id_df, by = "caseid")
stopifnot(nrow(merged_df) == nrow(data_df))

## Verifying that the dataset that is piped into the function is the left one
## merged_df2 <- left_join(data_df,id_df, by="caseid")
## all.equal(merged_df,merged_df2)

## How many user_ids are duplicated? (178 of 500)
table(table(merged_df$user_id))

## Look at a few
some_dups <- as.numeric(names(table(merged_df$user_id)))[table(merged_df$user_id) == 2][1:4]
merged_df %>%
  filter(user_id %in% some_dups) %>%
  select(caseid, user_id, survey, which_survey) %>%
  arrange(user_id, which_survey)
id_df %>% filter(user_id %in% some_dups)
## Person with caseid_15 in november of 1266266221 was re-interviewed in
## december with caseid_16 of 1297699113

## Separate people who were not in either survey 5 or 8 OR who were only
## interviewed in survey 5 (so not interviewed twice).
table(merged_df$which_survey, merged_df$survey, exclude = c())

na_df <- merged_df %>% filter(is.na(which_survey))

## Focus on people possibly interviewed twice
not_na_df <- merged_df %>% filter(!is.na(which_survey))

## Create a dataset containing only the first interview for the people
## interviewed twice
first_answer_df <- not_na_df %>%
  arrange(which_survey) %>%
  distinct(user_id, .keep_all = TRUE)
stopifnot(nrow(first_answer_df) == 500)

final_df <- rbind(na_df, first_answer_df)

final_df %>% write_csv(file.path(DATA_DIR, "deduplicated_data_first.csv"))


last_answer_df <- not_na_df %>%
  arrange(desc(which_survey)) %>%
  distinct(user_id, .keep_all = TRUE)

final_df <- rbind(na_df, last_answer_df)

final_df %>% write_csv(file.path(DATA_DIR, "deduplicated_data_last.csv"))
