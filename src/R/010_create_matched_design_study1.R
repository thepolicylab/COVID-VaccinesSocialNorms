#' Setup distance matrices, handle missing covariates, etc.
#'
#' NOTE: Right now this uses the Gurobi optimizer by default inside of designmatch
#' The Gurobi optimizer requires a separate installation and is free for academic users.
#' See README for more details

library(optmatch)
library(designmatch)
library(RItools)
library(tidyverse)
library(estimatr)
library(here)

source(here("src", "R", "000_constants_and_utils.R"))
solverlist <- get_solverlist()

dat0 <- read.csv(file.path(DATA_DIR, "MERGE_NR_2.3.21.csv"))
dat0$row_id <- seq_len(nrow(dat0))
dat0$id <- dat0$caseid

## Survey
table(dat0$survey, exclude = c())

# Causal Variables

## Perceptions
## We have to exclude NAs on these.
## Perceptions of testing willingness among family and friends also: q25, q25_number
## Perceptions of mask wearing willingness among family and friends also: q100
## Not including q100 for now. Too many missing values.
perceptions <- c("q60_1", "q60_2", "q60_3", "q60_4", "q25") ## ,"q100")
summary(dat0[, perceptions])
## Recode 98 and 99 and 8 and 9 to NA
dat0 <- dat0 %>%
  mutate(across(
    one_of(perceptions[1:4]),
    ~ if_else(is.na(.x) | .x > 7 | .x == "NA",
      NA_integer_,
      as.integer(.x)
    )
  ))

summary(dat0[, perceptions])
for (col in perceptions[1:4]) {
  stopifnot(max(dat0[[col]], na.rm = TRUE) == 7)
}

## Have to combine q25 and q25_number
dat0$q25[dat0$q25 > 7] <- NA
dat0 <- dat0 %>% mutate(q25new = coalesce(q25, q25_number))
with(dat0, table(q25, q25new, exclude = c()))
with(dat0, table(q25_number, q25new, exclude = c()))
perceptions[perceptions == "q25"] <- "q25new"

## Outcomes: not looked at here but only match using people with valid outcomes.
# q1 - Intention to get vaccinated
# q29 - Willingness to test
# q7 - Mask wearing
outcomes <- c("q1", "q29", "q7")
summary(dat0[, outcomes])
dat0$q29[dat0$q29 > 6] <- NA
dat0$q7[dat0$q7 > 5] <- NA
dat0 %>%
  filter(survey %in% c(5, 6, 7, 8)) %>%
  dplyr::select(all_of(outcomes)) %>%
  summary()
stopifnot(max(dat0$q7, na.rm = TRUE) == 5 & max(dat0$q29, na.rm = TRUE) == 6 & max(dat0$q1, na.rm = TRUE) == 5)

## Only focus on data that is not missing.
### this focuses on only surveys 5,6,7,8
dat1 <- na.omit(dat0[, c("row_id", "id", "survey", outcomes, perceptions)])

## We only use surveys 5,6,7,8
dat1 <- dat1 %>%
  filter(survey %in% c(5, 6, 7, 8)) %>%
  droplevels()

##########
## COVARIATES
### Here we recode and otherwise clean up covariate data
dat0$female <- as.numeric(dat0$gender == 2)

## move "Other" combine indep and other and dk pids: 1=dem, 2=republican, 3=indep,
dat0$dem_rep_oth <- with(dat0, case_when(pid3 == 1 ~ 1, pid3 == 2 ~ 2, TRUE ~ 3))
with(dat0, table(pid3, dem_rep_oth, exclude = c()))
dat0$pid3F <- factor(dat0$pid3)

## Create Measured Knowledge Scale
dat0 <- dat0 %>% mutate(across(one_of(c("q55_3", "q55_4", "q55_7", "q55_8")), ~ if_else(.x > 2, NA_integer_, .x - 1L), .names = "{.col}_correct"))
dat0 <- dat0 %>% mutate(across(one_of(c("q55_1", "q55_2", "q55_5", "q55_6")), ~ if_else(.x > 2, NA_integer_, .x), .names = "{.col}_correct"))
dat0 %>%
  select(one_of(c("q55_1_correct", "q55_2_correct", "q55_5_correct", "q55_6_correct", "q55_3_correct", "q55_4_correct", "q55_7_correct", "q55_8_correct"))) %>%
  summary()
## Notice that the correct answers are scored 1 but the incorrect answers are
## scored 2 for certain of the variables.
## So, if any of the variables has a 2, convert it to 0 for "incorrect" (and
## then take the mean so that we have a proportion of correct answers
## Not creating new variables here, overwriting the old ones, because (1) this
## is simple and (2) we don't work with them directly after this point.
dat0 <- dat0 %>% mutate(across(one_of(c("q55_1_correct", "q55_2_correct", "q55_5_correct", "q55_6_correct")), ~ if_else(.x == 2, 0L, .x)))
dat0 %>%
  select(one_of(c("q55_1_correct", "q55_2_correct", "q55_5_correct", "q55_6_correct", "q55_3_correct", "q55_4_correct", "q55_7_correct", "q55_8_correct"))) %>%
  summary()
dat0 <- dat0 %>%
  rowwise() %>%
  mutate(covid_know = mean(c_across(one_of(c("q55_1_correct", "q55_2_correct", "q55_5_correct", "q55_6_correct", "q55_3_correct", "q55_4_correct", "q55_7_correct", "q55_8_correct"))))) %>%
  ungroup()
table(dat0$covid_know, exclude = c())

## Trust in scientists
## q18, q108
with(dat0, table(q18, exclude = c()))
with(dat0, table(q108, exclude = c()))
dat0 <- dat0 %>% mutate(
  q18 = if_else(q18 == 8, NA_integer_, q18),
  trust_in_science = rowMeans(across(one_of(c("q18", "q108"))), na.rm = TRUE)
)
with(dat0, table(q18, exclude = c()))

## COVID Subjective Knowledge
## q107
with(dat0, table(q107, exclude = c()))
dat0$covid_subj_know <- dat0$q107

## Religiosity
## pew_religimp, pew_churatd, pew_prayer
## Below will keep these continuous: matching a "never" to a "dk" is better than a "never" to "all the time", etc..
with(dat0, table(pew_religimp, exclude = c()))
with(dat0, table(pew_churatd, exclude = c()))
with(dat0, table(pew_prayer, exclude = c()))
## Someone who says "DK" for prayer frequency, seeing options like Never, is probably more like Seldom than Never.
dat0$pew_prayer <- if_else(dat0$pew_prayer == 8, 6L, dat0$pew_prayer)
with(dat0, table(pew_religimp, pew_churatd, exclude = c()))
with(dat0, table(pew_religimp, pew_prayer, exclude = c()))

dat0 <- dat0 %>% mutate(across(one_of(c("pew_religimp", "pew_prayer", "pew_churatd")), ~ if_else(.x > 7, NA_integer_, .x)))

cor(dat0[, c("pew_religimp", "pew_prayer", "pew_churatd")], use = "complete")
## Combine the relgiosity variables
dat0 <- dat0 %>% mutate(across(one_of(c("pew_religimp", "pew_prayer", "pew_churatd")), rank, .names = "{.col}_rank"))
dat0$relig_scale0 <- rowMeans(dat0[, c("pew_religimp_rank", "pew_prayer_rank", "pew_churatd_rank")])
dat0$relig_scale <- with(dat0, (relig_scale0 - min(relig_scale0)) / (max(relig_scale0) - min(relig_scale0)))
summary(select(dat0, relig_scale, relig_scale0))
with(dat0, cor(relig_scale, relig_scale0))

cor(dat0[, c(
  "pew_religimp", "pew_prayer", "pew_churatd",
  "pew_religimp_rank", "pew_prayer_rank", "pew_churatd_rank",
  "relig_scale"
)])

## Simplify the race variable: white, black, "latino"
## Also create factor versions for use in making distance matrices (factors -->
## binary indicators)
dat0$raceF <- factor(dat0$race)
dat0$race_new <- ifelse(dat0$race > 3, 4, dat0$race)
with(dat0, table(race, race_new, exclude = c()))
dat0$race_newF <- factor(dat0$race_new)

## Age has some weird values, so recode them
## We assume that a large age value over 1000 means the survey participant
## input their year of birth and not their age. So take 2020 - age as their implied age
summary(dat0$age)
dat0$agegood <- with(dat0, ifelse(age < 0, NA, age))
## 120 is the maximum age that is less than 1000
non_year_max_age <- max(dat0$age[dat0$age < 1000], na.rm = TRUE)
dat0$agegood <- with(dat0, ifelse(agegood > non_year_max_age, 2020 - agegood, agegood))
## Assuming that 0 is minimum age (i.e. 18 as of 2020)
dat0$agegood[dat0$agegood == 0] <- 18
summary(dat0$agegood)

## Fix weird values in family income
dat0$faminc_new[dat0$faminc_new > 18] <- NA
dat0$ideo5[dat0$ideo5 == 8] <- NA
covs <- c(
  "ideo5", "dem_rep_oth", "agegood", "female", "race_new", "race_newF", "faminc_new",
  "educ", "trust_in_science", "covid_know", "covid_subj_know", "relig_scale"
)

### Merge the data for the recoded covariates with the data where people have valid
### outcomes and causal variables.
dat2 <- inner_join(dat1, dat0[, c("row_id", covs)], by = "row_id")
stopifnot(nrow(dat2) == nrow(dat1))
summary(dat2[, covs])

## Get ready to impute missing data to match on
cov.factors <- sapply(dat2[, covs], is.factor)
cov.contrasts <- lapply(
  dat2[, names(cov.factors)[cov.factors], drop = FALSE],
  contrasts,
  contrasts = FALSE
)
covsdat2 <- optmatch::fill.NAs(
  dat2[, covs],
  all.covs = TRUE,
  contrasts.arg = cov.contrasts
)
stopifnot(nrow(covsdat2) == nrow(dat2))
covsdat2$id <- dat2$row_id
covsdat2 <- covsdat2 %>% mutate(across(where(is.logical), ~ as.numeric(.x)))
summary(covsdat2)
summary(dat2[, covs])

## Some variables have so few missing values that it is not worth balancing them
## directly (age, ideo5, pid for example)
## Add the variables for which we are imputing missings and the missing indicator
## back to the main data.
dat2$faminc_new.NA <- covsdat2$faminc_new.NA
dat2$faminc_new_Imp <- covsdat2$faminc_new
dat2$covid_subj_know.NA <- covsdat2$covid_subj_know.NA
dat2$covid_subj_know_Imp <- covsdat2$covid_subj_know
dat2$covid_know.NA <- covsdat2$covid_know.NA
dat2$covid_know_Imp <- covsdat2$covid_know
dat2$ideo5.NA <- covsdat2$ideo5.NA
dat2$ideo5_Imp <- covsdat2$ideo5

## Collapse perceptions into a single score.  We want to compare people who are
## *dissimilar* on this score (actually disimilar on each perceptions variable,
## but this is easier so we don't need different designs for each variable)
percdat2 <- dat2[, perceptions[perceptions != "q25new"]]

## Not huge differences in variance so no fancy-ness needed in the mahalanobis distance
dplyr::summarize_all(percdat2, sd)
dat2$percdist <- mahalanobis(
  x = percdat2,
  center = col_means(percdat2),
  cov = cov(percdat2)
)
summary(dat2$percdist)
dat2 <- dat2 %>% mutate(across(one_of(perceptions), rank, .names = "{.col}_rank"))
dat2$avg_perc_rank <- rowMeans(dat2[, paste(perceptions[1:4], "_rank", sep = "")])
summary(select(dat2, avg_perc_rank))

perc_cors <- cor(dat2[, c(
  perceptions,
  paste(perceptions[1:4], "_rank", sep = ""),
  "avg_perc_rank", "percdist"
)])

## The average rank of the mean perceptions variable correlates much more highly
## with any given perceptions variable than the distance based one.
perc_cors[c("avg_perc_rank", "percdist"), ]


##########
# Make distance matrices

# Create MH Distance of all covariates.
## Differences in variance. So convert to ranks
## Removing columns otherwise can't invert matrix also don't match on stuff we are matching on exactly
covsdat2rank <- covsdat2 %>%
  mutate(across(-one_of(c("id", "dem_rep_oth")), rank)) %>%
  dplyr::select(-one_of(c(
    "id", "female", "dem_rep_oth",
    "race_new", "race_newF1", "race_newF2", "race_newF3", "race_newF4",
    "ideo5.NA"
  )))

stopifnot(all.equal(dat2$row_id, covsdat2$id))
dat2$covmh <- mahalanobis(
  x = covsdat2rank,
  center = col_means(covsdat2rank),
  cov = cov(covsdat2rank)
)
mhdist_mat <- outer(dat2$covmh, dat2$covmh, FUN = \(x, y) abs(x - y))

## Like a propensity distance?
library(splines)

mod1 <- lm(
  avg_perc_rank ~
  ns(ideo5_Imp, df = 3) +
    ns(agegood, df = 3) +
    ns(faminc_new_Imp, df = 3) +
    ns(educ, df = 3) +
    ns(trust_in_science, df = 3) +
    ns(covid_know_Imp, df = 3) +
    ns(covid_subj_know_Imp, df = 3) +
    ns(relig_scale, df = 3),
  data = dat2
)
dat2$ps <- predict(mod1)

ps_dist_mat <- outer(dat2$ps, dat2$ps, FUN = \(x, y) abs(x - y))
agedist_mat <- outer(dat2$agegood, dat2$agegood, FUN = \(x, y) abs(x - y))
educ_dist_mat <- outer(dat2$educ, dat2$educ, FUN = \(x, y) abs(x - y))

faminc_newdist_mat <- outer(dat2$faminc_new_Imp, dat2$faminc_new_Imp, FUN = \(x, y) abs(x - y))
faminc_newdist_mat <- round(faminc_newdist_mat, 2)

ideodist_mat <- outer(dat2$ideo5_Imp, dat2$ideo5_Imp, FUN = \(x, y) abs(x - y))
trust_science_dist_mat <- outer(dat2$trust_in_science, dat2$trust_in_science, FUN = \(x, y) abs(x - y))
relig_dist_mat <- outer(dat2$relig_scale, dat2$relig_scale, FUN = \(x, y) abs(x - y))
covid_subj_know_dist_mat <- outer(dat2$covid_subj_know_Imp, dat2$covid_subj_know_Imp, FUN = \(x, y) abs(x - y))
covid_know_dist_mat <- outer(dat2$covid_know_Imp, dat2$covid_know_Imp, FUN = \(x, y) abs(x - y))

## This is just to see how many pairs would have people who perceive the same thing --- we want to avoid this
avg_perc_rank_dist_mat <- outer(dat2$avg_perc_rank, dat2$avg_perc_rank, FUN = \(x, y) abs(x - y))

## Make entries that are very different in certain covariates huge so to discourage
## those matches. This is a called putting a penalty on the distance matrix
maxmhdist <- max(mhdist_mat)
mh_pen <- quantile(as.vector(mhdist_mat), .9)
ps_pen <- quantile(as.vector(ps_dist_mat), .5)

matchdist_mat <- mhdist_mat +
  1000 * maxmhdist * (mhdist_mat > mh_pen) +
  1000 * maxmhdist * (ps_dist_mat > ps_pen)

######
# Setup for the matching algorithm

## Rescale and round mhdist mat --- helps for some reason, not 100% clear on the
## need for not too many digits of precision by nmatch()
mhdist_mat <- round(mhdist_mat / mean(mhdist_mat), 2)
mhdist_mat[1:5, 1:6]
sort(unique(as.vector(mhdist_mat)))[1:10]
sort(unique(as.vector(mhdist_mat)), decreasing = TRUE)[1:10]
## Also rescale the distance matrix with penalties applied
matchdist_mat <- round(matchdist_mat / mean(matchdist_mat), 2)
matchdist_mat[1:5, 1:6]

## Allow a minimum perceptions distance within pair of more than 0.
perc_dists <- sort(unique(as.vector(avg_perc_rank_dist_mat[lower.tri(avg_perc_rank_dist_mat, diag = FALSE)])))[1:10]
min_non_zero_perc_dist <- min(perc_dists[perc_dists > 0])
farlist <- list(covs = as.matrix(dat2$avg_perc_rank), pairs = c(avg_perc_rank = min_non_zero_perc_dist / 2))

## Choose the boundaries at which point the match should be avoided.
## i.e. don't match people who differ by more than X years in age, etc..
summary(as.vector(trust_science_dist_mat))
summary(as.vector(ideodist_mat))
table(as.vector(ideodist_mat))
table(as.vector(trust_science_dist_mat))
table(as.vector(educ_dist_mat))
table(as.vector(relig_dist_mat))
table(as.vector(faminc_newdist_mat))
table(as.vector(covid_subj_know_dist_mat))
# We are not using all of these matrices and variables in the final analysis.
# We could remove some of this code if we wanted.
educ_pen <- quantile(as.vector(educ_dist_mat), .9)
faminc_pen <- quantile(as.vector(faminc_newdist_mat), .9)
trust_science_pen <- quantile(as.vector(trust_science_dist_mat), .4)
relig_pen <- quantile(as.vector(relig_dist_mat), .9)
covid_know_pen <- quantile(as.vector(covid_know_dist_mat), .9)
covid_subj_know_pen <- quantile(as.vector(covid_subj_know_dist_mat), .5)

nearlist <- list(
  covs = as.matrix(dplyr::select(
    dat2, agegood, ideo5_Imp, educ,
    faminc_new_Imp, faminc_new.NA, trust_in_science, relig_scale, covid_know, covid_subj_know
  )),
  pairs = c(
    agegood = 9.5, ideo5_Imp = 2.5, educ = 1,
    ## We use some contants based on what we take to be substantively meaningful
    ## differences (like no more than 9.5 years of age differences) and also
    ## some that just restrict the differences based on the distributions of
    ## differences above (using quantiles).
    faminc_new_Imp = 3, faminc_new.NA = 1, trust_in_science = .9, relig_scale = relig_pen, covid_know = covid_know_pen,
    covid_subj_know = .5
  )
)

## Match exactly within survey and partisanship and female and race
exact_covs_list <- list(covs = as.matrix(dat2[, c("survey", "dem_rep_oth", "female", "race_new")]))

## Do the matching
print(paste0("Matching with ", names(solverlist), " = ", solverlist))
res <- nmatch(
  dist_mat = mhdist_mat,
  far = farlist,
  near = nearlist,
  exact = exact_covs_list,
  subset_weight = 1,
  solver = solverlist
)

length(res$id_1)

## The nmatch_to_df function creates a column labeled "bm" which contains
## indicators of match/pair membership
res_df <- nmatch_to_df(res, origid = dat2$row_id)
dat2$orig_id <- dat2$id
dat2$id <- dat2$row_id
dat3 <- inner_join(dat2, res_df, by = "id")
dat3 <- droplevels(dat3)
stopifnot(nrow(dat3) == nrow(res_df))

## Number of matches:
# dat3$bm is the matched set indicator.
stopifnot(length(unique(dat3$bm)) == nrow(dat3) / 2)
nrow(dat2)
nrow(dat3)

## Did we make exact matches by pid?
pidtest <- dat3 %>%
  group_by(bm) %>%
  summarize(uniqpid = length(unique(dem_rep_oth)))
stopifnot(all(pidtest$uniqpid == 1))

## Make a new variable that is 1 for the person who is higher on the perceptions index
## and 0 for the person who is lower.
## We'd like to show that the covariates are not related to perceptions within pair.
dat3 <- dat3 %>%
  group_by(bm) %>%
  mutate(rankperc = rank(avg_perc_rank) - 1) %>%
  arrange(bm) %>%
  ungroup()

## Once we have a match, assess it using xBalance
balfmla <- reformulate(covs, response = "rankperc")
balfmla <- update(balfmla, . ~ . - dem_rep_oth - female - race_newF - race_new)
## Using only the matched data and also conditional within sets
xbres <- xBalance(balfmla, strata = list(matched = ~bm), data = dat3, report = "all")
xbres$overall
xbres_vars <- data.frame(xbres$results[, c("rankperc=0", "rankperc=1", "adj.diff", "std.diff", "p"), "matched"])
xbres_vars$padj <- p.adjust(xbres_vars$p, method = "holm")
options(digits = 3)
arrange(xbres_vars, p) %>% zapsmall(digits = 5)
stopifnot(xbres$overall[, "p.value"] > .3)

## Not strong evidence against the idea that the higher perceiver differs from the
## lower perceiver in terms of covariates
## padj is adjusting the individual p-values for multiple testing (using a
## non-conservative adjustment the Holm approach)
## No standardized difference larger than about .1 (after adjusting for pair)

## Distribution of differences within pair
pair_diffs <- dat3 %>%
  group_by(bm) %>%
  arrange(avg_perc_rank) %>%
  summarize(
    ideo = diff(ideo5), educ = diff(educ), age = diff(agegood),
    female = diff(female == TRUE), ## to test exact matching
    race_new = diff(race_new), ## to test exact matching
    dem_rep_oth = diff(dem_rep_oth), ## to test exact matching
    faminc = diff(faminc_new_Imp),
    faminc_NA = diff(faminc_new.NA == 1),
    trust_in_science = diff(trust_in_science),
    covid_know = diff(covid_know_Imp),
    covid_know_NA = diff(covid_know.NA),
    covid_subj_know = diff(covid_subj_know_Imp),
    covid_subj_know_NA = diff(covid_subj_know.NA),
    relig_scale = diff(relig_scale),
    percdiff = diff(avg_perc_rank)
  )
head(pair_diffs)
## Here are two pairs:
## Allowing perceptions to differ (not even controlling them now above)
## Notice differences in age etc.. We can restrict this further if we'd like.
## But it would not be surprising to see some such differences in a randomized experiment (from xBalance above)
dat3 %>%
  filter(bm %in% c(1, 2)) %>%
  dplyr::select(bm, avg_perc_rank, ideo5, educ, agegood, dem_rep_oth, female, race_new, faminc_new)
## Half of the pairs had no difference in ideo. And about half the time the person
## higher in perceptions had higher ideo, and about half the time the person higher in
## perceptions had lower ideo

sapply(pair_diffs, quantile, probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE)
sapply(pair_diffs, mean)
sapply(pair_diffs, function(x) {
  quantile(abs(x), probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE)
})

# Assess mean balance
covsnumeric <- c(covs, "faminc_new.NA")
names(covsnumeric) <- c(covs, "faminc_new.NA")
covsnumeric["race_newF"] <- "race_new"
covsnumeric["faminc_new"] <- "faminc_new_Imp"

dat3 <- dat3 %>%
  group_by(bm) %>%
  mutate(grp = c(1, 0))

tab0 <- dat3 %>%
  group_by(grp) %>%
  summarize(across(one_of(covsnumeric), mean, na.rm = TRUE))
tab <- data.frame(t(tab0))[-1, ]
tab$Diffs <- tab$X1 - tab$X2
names(tab)[1:3] <- c("Mean 1", "Mean 2", "Diffs")
tab

## Do we have differences in perceptions?

## What kinds of differences in perceptions do we see overall.
avg_perc_rank <- dist(dat3[, perceptions], diag = FALSE, upper = FALSE)
summary(as.vector(avg_perc_rank))

## Add q100 to the data that we will use for outcome analysis, q100 is an
## outcome with a lot of missing data. In the final paper we do not analyze it
## although we present the analysis in the supplementary materials.
## Also add two variables to help calibrate / interpret / explain our sensitivity analysis:
## q22  ( likelihood of getting infected with coronavirus in the next
## month (1-100 %))
## and q59_1 (agreement with the statement "Getting vaccinated would put me at
## risk for vaccine side effects" (1-5 scale))
dat0$orig_id <- dat0$id
dat3 <- left_join(dat3, dat0[, c("orig_id", "q100", "q22", "q59_1")], by = "orig_id")

## Should be no missing data on either of these variables on dat3:
stopifnot(any(!is.na(dat3$q22)))
stopifnot(any(!is.na(dat3$q59_1)))

save(dat3, tab, pair_diffs, xbres_vars, xbres, file = file.path(MATCHES_DIR, "dat_plus_matches_study1.rda"))
save(dat0, dat1, dat2, dat3, file = file.path(MATCHES_DIR, "datasets_study1.rda"))
