## Make tables and a few plots describing the differences on covariates within pairs after matching

library(here)
source(here("src", "R", "000_constants_and_utils.R"))
source(here("src/R", "001_rmd_setup.R"))
library(tidyverse)
library(estimatr)
library(flextable)
set_flextable_defaults(digits = 2)

load(file = here(MATCHES_DIR, "dat_plus_matches_study1_first.rda"), verbose = TRUE)
dat3_1 <- dat3
load(file = here(MATCHES_DIR, "datasets_study1_first.rda"), verbose = TRUE)
stopifnot(all.equal(dat3_1,dat3))
rm(dat3_1)

matched_dat_study1 <- dat3
xbres_study1 <- xbres
xbres_vars_study1 <- xbres_vars
pair_diffs_study1 <- pair_diffs
## The data pre-matching --- no missing data on outcomes or perceptions. So not *all* the data, but the *valid* data
valid_dat_study1 <- dat2
rm(dat0, dat1, dat2, dat3, xbres, xbres_vars)

load(file = here(MATCHES_DIR, "dat_plus_matches_study2.rda"), verbose = TRUE)
load(file = here(MATCHES_DIR, "datasets_study2.rda"), verbose = TRUE)

matched_dat_study2 <- dat5
xbres_study2 <- xbres
xbres_vars_study2 <- xbres_vars
pair_diffs_study2 <- pair_diffs
## The data pre-matching --- no missing data on outcomes or perceptions. So not *all* the data, but the *valid* data
valid_dat_study2 <- dat3
rm(dat3, dat5, xbres, xbres_vars)

# Balance Tables and Omnibus test

xbres_study1$overall
xbres_study2$overall

xbres_vars_study1$var <- row.names(xbres_vars_study1)
xbres_vars_study2$var <- row.names(xbres_vars_study2)
xbres_vars_study1$var[xbres_vars_study1$var == "agegood"] <- "age"

balance_table_study1 <- flextable(xbres_vars_study1)
balance_table_study1 <- theme_booktabs(balance_table_study1)
balance_table_study1 <- autofit(balance_table_study1, add_w = 0, add_h = 0)
balance_table_study1 <- set_caption(balance_table_study1, "Balance Table for Study 1")
save_as_docx("Balance Table for Study 1" = balance_table_study1, path = here(OUTPUT_DIR, "balance_table_study1.docx"))

balance_table_study2 <- flextable(xbres_vars_study2)
balance_table_study2 <- theme_booktabs(balance_table_study2)
balance_table_study2 <- autofit(balance_table_study2, add_w = 0, add_h = 0)
balance_table_study2 <- set_caption(balance_table_study2, "Balance Table for Study 2")
save_as_docx("Balance Table for Study 2" = balance_table_study2, path = here(OUTPUT_DIR, "balance_table_study2.docx"))

### Make a combined object for showing all together
xbres <- full_join(select(xbres_vars_study1, var, std.diff),
  select(xbres_vars_study2, var, std.diff),
  by = "var", suffix = c(".study1", ".study2")
)
xbres <- xbres %>% arrange(std.diff.study2, std.diff.study1)
xbres
names(xbres) <- c("Covariate", "Study 1", "Study 2")
xbres$Covariate <- c(
  "Education", "Trust In Govt", "Age", "COVID Subjective Knowledge",
  "Family Income", "Trust In Science", "Lib/Con Ideology",
  "Missing Family Income", "Religiosity", "Missing Covid Knowledge",
  "Missing COVID Subjective Knowledge", "COVID Knowledge"
)

xbres <- xbres %>% mutate(across(where(is.numeric), ~ round(.x, 3)))


table1_ft <- flextable(xbres)
table1_ft <- add_footer_lines(table1_ft, paste("Pair-adjusted standardized differences in means for continuous or ordinary covariates or proportions for binary covariates for both study 1 and study 2. Study 1 N = ", nrow(matched_dat_study1), ". Study 2 N = ", nrow(matched_dat_study2), ".", sep = ""))
table1_ft <- theme_booktabs(table1_ft)
# set_table_properties(table1_ft, width = .6, layout = "autofit")
table1_ft <- autofit(table1_ft, add_w = 0, add_h = 0)
table1_ft <- set_caption(table1_ft, "Balance Table for Study 1 and Study 2")
save_as_docx("table1" = table1_ft, path = here(OUTPUT_DIR, "table1.docx"))


## Perhaps an easier way to show the same information using boxplots:

library(ggrepel)
xbres_long <- pivot_longer(xbres, cols = c("Study 1", "Study 2"))

fig1_boxplot <- ggplot(data = na.omit(xbres_long), aes(x = name, y = value)) +
  geom_point() +
  geom_boxplot(fill = NA) +
  ylab("Pair-adjusted standardized differences in means") +
  xlab("") +
  geom_text_repel(aes(label = Covariate), segment.color = "gray") +
  theme_classic(base_size = 14)
fig1_boxplot

ggsave(file = here(FIGURES_DIR, "fig1_boxplot.png"), dpi = 300, type = "cairo-png", width = 10, height = 10)
ggsave(file = here(FIGURES_DIR, "fig1_boxplot.pdf"), device = cairo_pdf, width = 10, height = 10)


# Study 1 within pair differences

pd_quants_study1 <- as.data.frame(sapply(pair_diffs_study1, function(x) {
  quantile(abs(x), probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE)
}))

pd_quants_study1 <- pd_quants_study1[, grep("bm|percdiff", names(pd_quants_study1), invert = TRUE)]

pd_quants_study1_long <- as.data.frame(t(pd_quants_study1[c("10%", "50%", "90%", "100%"), ]))

## Add ranges given the matched data
covs_study1 <- row.names(pd_quants_study1_long)
names(covs_study1) <- covs_study1
covs_study1[c("ideo", "age", "faminc", "faminc_NA", "covid_know_NA", "covid_subj_know_NA")] <- c("ideo5", "agegood", "faminc_new_Imp", "faminc_new.NA", "covid_know.NA", "covid_subj_know.NA")

cov_ranges_study1 <- matched_dat_study1 %>%
  ungroup() %>%
  summarize(across(one_of(covs_study1), ~ range(.x, na.rm = TRUE)))
cov_ranges2_study1 <- valid_dat_study1 %>%
  ungroup() %>%
  summarize(across(one_of(covs_study1), ~ range(.x, na.rm = TRUE)))

pd_study1 <- cbind(round(pd_quants_study1_long, digits = 2), t(cov_ranges2_study1))
pd_study1 <- rename(pd_study1, "Min" = "1", "Max" = "2")
pd_study1$var <- row.names(pd_study1)
## No need to show differences for exact matches

pd_study1 <- filter(pd_study1, !(var %in% c("female", "race_new", "dem_rep_oth")))
pd_study1$Covariate <- c(
  "Lib/Con Ideology", "Education", "Age", "Family Income", "Missing Family Income", "Trust In Science", "COVID Knowledge", "Missing Covid Knowledge", "COVID Subjective Knowledge",
  "Missing COVID Subjective Knowledge", "Religiosity"
)
pd_study1

pd_study1_ft <- flextable(pd_study1, col_keys = c("Covariate", "10%", "50%", "90%", "100%", "Min", "Max"))
pd_study1_ft
pd_study1_ft <- add_footer_lines(pd_study1_ft, "Percentiles of differences within pairs after matching. The `Min` and `Max` columns show the range of the variable before matching.")
pd_study1_ft <- theme_booktabs(pd_study1_ft)
pd_study1_ft <- autofit(pd_study1_ft, add_w = 0, add_h = 0)
pd_study1_ft <- set_caption(pd_study1_ft, "Within Pair-Differences for Study 1")
pd_study1_ft
save_as_docx("pd_study1" = pd_study1_ft, path = here(OUTPUT_DIR, "pd_study1.docx"))


## Trying a plot. Problem is difference scales.

pd_study1_long <- pivot_longer(pair_diffs_study1, cols = names(pair_diffs_study1))
pd_study1_long <- filter(pd_study1_long, !(name %in% c("bm", "percdiff")))

pd_study1_boxplot <- ggplot(data = pd_study1_long, aes(y = name, x = value)) +
  geom_boxplot(fill = NA) +
  xlab("Pair-adjusted standardized differences in means") +
  theme_classic(base_size = 14)
pd_study1_boxplot


# Study 2 within pair differences

pd_quants_study2 <- as.data.frame(sapply(pair_diffs_study2, function(x) {
  quantile(abs(x), probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE)
}))

pd_quants_study2 <- pd_quants_study2[, grep("bm|percdiff", names(pd_quants_study2), invert = TRUE)]

pd_quants_study2_long <- as.data.frame(t(pd_quants_study2[c("10%", "50%", "90%", "100%"), ]))

## Add ranges given the matched data
covs_study2 <- row.names(pd_quants_study2_long)
names(covs_study2) <- covs_study2
covs_study2[c("ideo", "faminc", "famincNA")] <- c("ideo5", "faminc_new_Imp", "faminc_new.NA")

cov_ranges_study2 <- matched_dat_study2 %>%
  ungroup() %>%
  summarize(across(one_of(covs_study2), ~ range(.x, na.rm = TRUE)))
cov_ranges2_study2 <- valid_dat_study2 %>%
  ungroup() %>%
  summarize(across(one_of(covs_study2), ~ range(.x, na.rm = TRUE)))

pd_study2 <- cbind(round(pd_quants_study2_long, digits = 2), t(cov_ranges2_study2))
pd_study2 <- rename(pd_study2, "Min" = "1", "Max" = "2")
pd_study2$var <- row.names(pd_study2)
## No need to show differences for exact matches

pd_study2 <- filter(pd_study2, !(var %in% c("female", "race_new", "dem_rep_oth")))
pd_study2$Covariate <- c("Lib/Con Ideology", "Education", "Age", "Family Income", "Missing Family Income", "Trust In Science", "Trust in Government", "COVID Subjective Knowledge", "Religiosity")
pd_study2

pd_study2_ft <- flextable(pd_study2, col_keys = c("Covariate", "10%", "50%", "90%", "100%", "Min", "Max"))

pd_study2_ft
pd_study2_ft <- add_footer_lines(pd_study2_ft, "Percentiles of differences within pairs after matching. The `Min` and `Max` columns show the range of the variable before matching.")
pd_study2_ft <- theme_booktabs(pd_study2_ft)
pd_study2_ft <- autofit(pd_study2_ft, add_w = 0, add_h = 0)
pd_study2_ft <- set_caption(pd_study2_ft, "Within Pair-Differences for Study 2")
pd_study2_ft
save_as_docx("pd_study2" = pd_study2_ft, path = here(OUTPUT_DIR, "pd_study2.docx"))
