#' Setup distance matrices, handle missing covariates, etc., Part II
#'
#' NOTE: Right now I am doing matching inside this file using the designmatch
#' package and the Gurobi optimizer. The Gurobi optimizer requires a separate
#' installation and is free for academic users.

library(optmatch)
library(designmatch)
library(RItools)
library(tidyverse)
library(estimatr)
library(here)

source(here("src", "R", "000_constants_and_utils.R"))
solverlist <- get_solverlist()

dat0 <- read.csv(file.path(DATA_DIR, "TPL_Testing_Survey_FifthWave_YouGov_MERGEDWITHFOURTHWAVEFORMESSING.csv"))
dat0$row_id <- seq_len(nrow(dat0))

## Survey
table(dat0$sample, exclude = c())

# The data set appended with MERGEDWITHFOURTHWAVEFORMESSING indicates truthfully
# that this includes data from Survey 4. Filter it out by removing data where sample=3
# (for the record, sample=1 is the US population from March 2021, and sample=2 is the
# RI population from the same period.

dat1 <- dat0 %>%
  filter(sample != 3) %>%
  droplevels()
table(dat1$sample, exclude = c())

## 2. Key variables with uninterpretable names are:
## q126 Do you plan to get vaccinated as soon as it's possible to do so? (only asked of people who said they have not already begun vaccination, q120=2)
## q60_1 How many people in your network of friends and family do you believe will definitely get vaccinated for coronavirus
## q60_2 How many people in the neigborhood where you live do you believe will definitely get vaccinated for coronavirus
## q60_3 How many people in your city or town do you believe will definitely get vaccinated for coronavirus
## q60_4 How many people in your state do you believe will definitely get vaccinated for coronavirus
## q60_5 How many Democrats do you believe will definitely get vaccinated for coronavirus
## q60_6 How many Republicans do you believe will definitely get vaccinated for coronavirus
## q60_7 How many Independents do you believe will definitely get vaccinated for coronavirus

## already_vaccinated_or_definitely_will_get_vaccinated A composite variable
## that gives people a 1 if either q120=1 (they got vaccinated) or q126=5 (they
## will definitely get vaccinated). Make this the outcome:

dat1$outcome <- with(dat1, ifelse(q126 == 5 | q120 == 1, 1, 0))
with(dat1, table(is.na(q120), sample, exclude = c()))

with(dat1, table(outcome, q126, exclude = c()))
with(dat1, table(outcome, q120, exclude = c()))
with(dat1, ftable(outcome, q120, q126, col.vars = 1, exclude = c()))
with(dat1, table(is.na(q120), is.na(q126)))

## Causal Variables
perceptions <- c("q60_1", "q60_2", "q60_3", "q60_4", "q60_5", "q60_6", "q60_7", "q25_new")
summary(dat1[, c(perceptions, "outcome")])
## Recode 98 and 99 and 8 and 9 to NA
dat1 <- dat1 %>% mutate(across(contains("q60_"), ~ ifelse(.x > 7, NA, .x)))
dat1$q25_new[dat1$q25_new > 7] <- NA
summary(dat1[, c(perceptions)])
stopifnot(max(dat1$q60_1, na.rm = TRUE) == 7)
stopifnot(max(dat1$q60_2, na.rm = TRUE) == 7)
stopifnot(max(dat1$q60_3, na.rm = TRUE) == 7)
stopifnot(max(dat1$q60_4, na.rm = TRUE) == 7)
stopifnot(max(dat1$q60_5, na.rm = TRUE) == 7)
stopifnot(max(dat1$q60_6, na.rm = TRUE) == 7)
stopifnot(max(dat1$q60_7, na.rm = TRUE) == 7)

## Outcomes:
outcomes <- c("outcome")
summary(dat1[, outcomes])

## Only focus on data that is not missing on outcomes or perceptions.
dat2 <- na.omit(dat1[, c("row_id", "sample", "outcome", perceptions)])
nrow(dat2) ## only dropping 3 obs
nrow(dat1)

## COVARIATES
dat1$gender <- dat1$gender_dummy_coded_female
dat1$female <- dat1$gender_dummy_coded_female ## a better name
## Get rid of duplicated columns in the data now that we have better names
dat1$gender <- NULL
dat1$gender_dummy_coded_female <- NULL

## Insist on exactly matching on PID.
table(dat1$pid3, exclude = c())
table(dat1$pid3_t, exclude = c())
table(dat1$pid7, exclude = c())
## pid7:1,2 == dem, 6,7== rep,
with(dat1, table(pid3, pid7, exclude = c()))

## move "Other" combine indep and other and dk pids: 1=dem, 2=republication, 3=indep,
dat1$dem_rep_oth <- with(dat1, case_when(pid3 == 1 ~ 1, pid3 == 2 ~ 2, TRUE ~ 3))
dat1$pid3F <- factor(dat1$dem_rep_oth)
with(dat1, table(pid3, dem_rep_oth, exclude = c()))

## Trust in govt
## q128_1, q128_2, q128_3
with(dat1, table(q128_1, exclude = c()))
with(dat1, table(q128_2, exclude = c()))
with(dat1, table(q128_3, exclude = c()))
dat1 <- dat1 %>% mutate(across(contains("q128_"), ~ ifelse(.x > 7, NA, .x)))
with(dat1, table(q128_1, exclude = c()))
with(dat1, table(q128_2, exclude = c()))
with(dat1, table(q128_3, exclude = c()))
dat1 <- dat1 %>% mutate(trust_in_govt = rowMeans(across(one_of(c("q128_1", "q128_2", "q128_3"))), na.rm = TRUE))

## Trust in scientists
## q18, q108
with(dat1, table(q18, exclude = c()))
with(dat1, table(q108, exclude = c()))
dat1 <- dat1 %>% mutate(trust_in_science = rowMeans(across(one_of(c("q18", "q108"))), na.rm = TRUE))

## COVID Knowledge
## q107
with(dat1, table(q107, exclude = c()))
dat1$covid_subj_know <- dat1$q107

## Religiosity
## pew_religimp, pew_churatd, pew_prayer
## Below will keep these continuous: matching a "never" to a "dk" is better than a "never" to "all the time", etc..
with(dat1, table(pew_religimp, exclude = c()))
with(dat1, table(pew_churatd, exclude = c()))
with(dat1, table(pew_prayer, exclude = c()))
## Someone who says "DK" for prayer frequency, seeing options like Never, is probably
## more like Seldom than Never.
dat1$pew_prayer <- if_else(dat1$pew_prayer == 8, 6L, dat1$pew_prayer)
with(dat1, table(pew_religimp, pew_churatd, exclude = c()))
with(dat1, table(pew_religimp, pew_prayer, exclude = c()))
dat1 <- dat1 %>% mutate(across(one_of(c("pew_religimp", "pew_prayer", "pew_churatd")), ~ if_else(.x > 7, NA_integer_, .x)))

cor(dat1[, c("pew_religimp", "pew_prayer", "pew_churatd")])
## Combine the relgiosity variables
dat1 <- dat1 %>% mutate(across(one_of(c("pew_religimp", "pew_prayer", "pew_churatd")), rank, .names = "{.col}_rank"))
dat1$relig_scale0 <- rowMeans(dat1[, c("pew_religimp_rank", "pew_prayer_rank", "pew_churatd_rank")])
dat1$relig_scale <- with(dat1, (relig_scale0 - min(relig_scale0)) / (max(relig_scale0) - min(relig_scale0)))
summary(select(dat1, relig_scale, relig_scale0))
with(dat1, cor(relig_scale, relig_scale0))

cor(dat1[, c(
  "pew_religimp", "pew_prayer", "pew_churatd",
  "pew_religimp_rank", "pew_prayer_rank", "pew_churatd_rank",
  "relig_scale"
)])

## Simplify the race variable: white, black, "latino"
dat1$raceF <- factor(dat1$race)
dat1$race_new <- ifelse(dat1$race > 3, 4, dat1$race)
with(dat1, table(race, race_new, exclude = c()))
## dat1$race_newF <- factor(dat1$race_new)

## Fix coding of family income to indicate NA
dat1$faminc_new[dat1$faminc_new > 18] <- NA

## Now add the covariates to the data where obs have valid outcomes and perceptions variables
covs <- c(
  "ideo5", "dem_rep_oth", "age", "female", "race_new", "faminc_new", "educ",
  "trust_in_govt", "trust_in_science", "covid_subj_know", "relig_scale"
)
summary(dat1[, covs])

dat3 <- inner_join(dat2, dat1[, c("row_id", covs)], by = "row_id")
stopifnot(nrow(dat2) == nrow(dat3))
dat3 <- droplevels(dat3)
summary(dat3[, covs])

## Get ready to impute missing data to match on
cov.factors <- sapply(dat3[, covs], is.factor)
cov.contrasts <- lapply(dat3[, names(cov.factors)[cov.factors], drop = FALSE],
  contrasts,
  contrasts = FALSE
)
covsdat3 <- optmatch::fill.NAs(dat3[, covs], all.covs = TRUE, contrasts.arg = cov.contrasts)
stopifnot(nrow(covsdat3) == nrow(dat3))
covsdat3$id <- dat3$row_id
covsdat3 <- covsdat3 %>% mutate(across(where(is.logical), ~ as.numeric(.x)))
summary(covsdat3)
## Add the variables for which we are imputing missings and the missing indicator back to the main data.
dat3$faminc_new.NA <- covsdat3$faminc_new.NA
dat3$faminc_new_Imp <- covsdat3$faminc_new

## Collapse perceptions into a single score.  We want to compare people who are
## *dissimilar* on this score (actually disimilar on each perceptions variable,
## but this is easier so we don't need different designs for each variable)

percdat3 <- dat3[, perceptions[perceptions != "q25_new"]]
## Not huge differences in variance so no fancy-ness needed in the mahalanobis distance
dplyr::summarize_all(percdat3, sd)
dat3$percdist <- mahalanobis(x = percdat3, center = col_means(percdat3), cov = cov(percdat3))
summary(dat3$percdist)
dat3 <- dat3 %>% mutate(across(one_of(perceptions), rank, .names = "{.col}_rank"))
dat3$avg_perc_rank <- rowMeans(dat3[, paste(perceptions[1:7], "_rank", sep = "")])
summary(select(dat3, avg_perc_rank))

perc_cors <- cor(dat3[, c(
  perceptions,
  paste(perceptions[1:7], "_rank", sep = ""),
  "avg_perc_rank", "percdist"
)])

perc_cors[c("avg_perc_rank", "percdist"), ]

##########
# Distance Matrices
## Differences in variance. So convert to ranks
## Removing columns otherwise can't invert matrix also don't match on stuff we are matching on exactly
covsdat3rank <- covsdat3 %>%
  mutate(across(-one_of(c("id", "dem_rep_oth")), rank)) %>%
  dplyr::select(-one_of(c("id", "female", "dem_rep_oth", "race_new")))

stopifnot(all.equal(dat3$row_id, covsdat3$id))
dat3$covmh <- mahalanobis(x = covsdat3rank, center = col_means(covsdat3rank), cov = cov(covsdat3rank))
mhdist_mat <- outer(dat3$covmh, dat3$covmh, FUN = \(x, y) abs(x - y))

## Like a propensity distance?
library(splines)

mod1 <- lm(avg_perc_rank ~ ns(ideo5, df = 3) + ns(age, df = 3) + ns(faminc_new_Imp, df = 3) + ns(educ, df = 3) + ns(trust_in_govt, df = 3) + ns(trust_in_science, df = 3) + ns(covid_subj_know, df = 3) + ns(relig_scale, df = 3), data = dat3)
dat3$ps <- predict(mod1)

create_distance_matrix <- function(col) {
  round(outer(dat3[[col]], dat3[[col]], FUN = \(x, y) abs(x - y)), 2)
}

ps_dist_mat <- create_distance_matrix("ps")
agedist_mat <- create_distance_matrix("age")
educ_dist_mat <- create_distance_matrix("educ")
faminc_newdist_mat <- create_distance_matrix("faminc_new_Imp")
ideodist_mat <- create_distance_matrix("ideo5")
trust_govt_dist_mat <- create_distance_matrix("trust_in_govt")
trust_science_dist_mat <- create_distance_matrix("trust_in_science")
relig_dist_mat <- create_distance_matrix("relig_scale")
covid_know_dist_mat <- create_distance_matrix("covid_subj_know")
avg_perc_rank_dist_mat <- create_distance_matrix("avg_perc_rank")

## Make entries that are very different in certain covariates huge so discourage those
## matches. This is a called putting a penalty on the distance matrix
maxmhdist <- max(mhdist_mat)
mh_pen <- quantile(as.vector(mhdist_mat), .25)
ps_pen <- quantile(as.vector(ps_dist_mat), .25)
educ_pen <- quantile(as.vector(educ_dist_mat), .9)
faminc_pen <- quantile(as.vector(faminc_newdist_mat), .9)
trust_govt_pen <- quantile(as.vector(trust_govt_dist_mat), .6)
trust_science_pen <- quantile(as.vector(trust_science_dist_mat), .6)
relig_pen <- quantile(as.vector(relig_dist_mat), .9)
covid_know_pen <- quantile(as.vector(covid_know_dist_mat), .9)

matchdist_mat <- mhdist_mat + 1000 * maxmhdist * (mhdist_mat > mh_pen) + 1000 * maxmhdist * (ps_dist_mat > ps_pen)

## Rescale mhdist mat --- helps for some reason.
mhdist_mat <- round(mhdist_mat / mean(mhdist_mat), 2)
mhdist_mat[1:5, 1:6]
sort(unique(as.vector(mhdist_mat)))[1:10]
sort(unique(as.vector(mhdist_mat)), decreasing = TRUE)[1:10]
matchdist_mat <- round(matchdist_mat / mean(matchdist_mat), 2)
matchdist_mat[1:5, 1:6]

## Setup matching
### Tell the algorithmn to look for matches such that the differences within pair are
## less than some amount (measured in standard deviations.

## Allow a minimum perceptions distance within pair of more than 0.
perc_dists <- sort(unique(as.vector(avg_perc_rank_dist_mat[lower.tri(avg_perc_rank_dist_mat, diag = FALSE)])))[1:10]
min_non_zero_perc_dist <- min(perc_dists[perc_dists > 0])
farlist <- list(covs = as.matrix(dat3$avg_perc_rank), pairs = c(avg_perc_rank = min_non_zero_perc_dist / 2))

nearlist <- list(
  covs = as.matrix(dplyr::select(dat3, age, ideo5, educ, faminc_new_Imp, trust_in_govt, trust_in_science)),
  pairs = c(age = 7, ideo5 = 1, educ = 2, faminc_new_Imp = 3, trust_in_govt = 1, trust_in_science = 1)
)

## Match exactly within party, gender, and race category
exact_covs_list <- list(covs = as.matrix(dat3[, c("dem_rep_oth", "female", "race_new")]))

## Do the matching
print(paste0("Matching with ", names(solverlist), "=", solverlist))
res <- nmatch(
  dist_mat = matchdist_mat,
  near = nearlist,
  far = farlist,
  exact = exact_covs_list,
  subset_weight = 1,
  solver = solverlist
)

length(res$id_1)

res_df <- nmatch_to_df(res, origid = dat3$row_id)
dat3$orig_id <- dat3$id
dat3$id <- dat3$row_id
dat4 <- inner_join(dat3, res_df, by = "id")
dat4 <- droplevels(dat4)
stopifnot(nrow(dat4) == nrow(res_df))

## Number of matches:
stopifnot(length(unique(dat4$bm)) == nrow(dat4) / 2)
nrow(dat3)
nrow(dat4)

## Did we make exact matches by pid?
pidtest <- dat4 %>%
  group_by(bm) %>%
  summarize(uniqpid = length(unique(dem_rep_oth)))
stopifnot(all(pidtest$uniqpid == 1))

## Make a new variable that is 1 for the person who is higher on the perceptions index and 0 for the person who is lower.
## We'd like to show that the covariates are not related to perceptions within pair.
dat4 <- dat4 %>%
  group_by(bm) %>%
  mutate(rankperc = rank(avg_perc_rank) - 1) %>%
  arrange(bm) %>%
  ungroup()

## Once we have a match, assess it using xBalance
## Not actually sure we want this.
balfmla <- reformulate(covs, response = "rankperc")
balfmla <- update(balfmla, . ~ . - dem_rep_oth - female - race_new)
## Using only the matched data and also conditional within sets
xbres <- xBalance(balfmla, strata = list(matched = ~bm), data = dat4, report = "all")
xbres$overall
xbres_vars <- data.frame(xbres$results[, c("rankperc=0", "rankperc=1", "adj.diff", "std.diff", "p"), "matched"])
xbres_vars$padj <- p.adjust(xbres_vars$p, method = "holm")
options(digits = 3)
arrange(xbres_vars, p) %>% zapsmall(digits = 5)
stopifnot(xbres$overall[, "p.value"] > .3)

## Not strong evidence against the idea that the higher perceiver differs from the lower perceiver in terms of covariates
## padj is adjusting the individual p-values for multiple testing (using a non-conservative adjustment the Holm approach)
## No standardized difference larger than about .1 (after adjusting for pair)

## Distribution of differences within pair
pair_diffs <- dat4 %>%
  group_by(bm) %>%
  arrange(avg_perc_rank) %>%
  summarize(
    ideo = diff(ideo5), educ = diff(educ), age = diff(age),
    female = diff(female == TRUE), ## to test exact matching
    race_new = diff(race_new), ## to test exact matching
    dem_rep_oth = diff(dem_rep_oth), ## to test exact matching
    faminc = diff(faminc_new_Imp), famincNA = diff(faminc_new.NA == 1),
    trust_in_science = diff(trust_in_science),
    trust_in_govt = diff(trust_in_govt),
    covid_subj_know = diff(covid_subj_know),
    relig_scale = diff(relig_scale),
    percdiff = diff(avg_perc_rank)
  )
head(pair_diffs)
## Here are two pairs:
## Allowing perceptions to differ (not even controlling them now above)
## Notice differences in age etc.. We can restrict this further if we'd like.
## But it would not be surprising to see some such differences in a randomized experiment (from xBalance above)
dat4 %>%
  filter(bm %in% c(1, 2)) %>%
  dplyr::select(bm, avg_perc_rank, ideo5, educ, age, dem_rep_oth, female, race_new, faminc_new)
## Half of the pairs had no difference in ideo. And about half the time the person higher in perceptions had higher ideo, and about half the time the person higher in perceptions had lower ideo

sapply(pair_diffs, quantile, probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE)
sapply(pair_diffs, mean)
sapply(pair_diffs, function(x) {
  quantile(abs(x), probs = c(0, .1, .25, .5, .75, .9, 1))
})

# Assess mean balance
## a <- apply(covsdat5[res$id_1, ], 2, mean)
## b <- apply(covsdat5[res$id_2, ], 2, mean)
covsnumeric <- c(covs, "faminc_new.NA")
names(covsnumeric) <- c(covs, "faminc_new.NA")
covsnumeric["race_newF"] <- "race_new"
covsnumeric["faminc_new"] <- "faminc_new_Imp"

dat4 <- dat4 %>%
  group_by(bm) %>%
  mutate(grp = c(1, 0))

tab0 <- dat4 %>%
  group_by(grp) %>%
  summarize(across(one_of(covsnumeric), mean))
tab <- data.frame(t(tab0))[-1, ]
tab$Diffs <- tab$X1 - tab$X2
names(tab)[1:3] <- c("Mean 1", "Mean 2", "Diffs")
tab

## Do we have differences in perceptions?

## What kinds of differences in perceptions do we see overall.
avg_perc_rank <- dist(dat4[, perceptions], diag = FALSE, upper = FALSE)
summary(as.vector(avg_perc_rank))

stopifnot(all.equal(dat4$id, dat4$row_id))
stopifnot(all.equal(dat3$id, dat3$row_id))
dat5a <- left_join(dat4, dat3[, c("id", "row_id", "outcome")], by = "row_id")
dat5 <- left_join(dat5a, dat1[, c("row_id", "q22", "q59_1")], by = "row_id")
stopifnot(nrow(dat5) == nrow(dat5a))

save(dat5, tab, pair_diffs, xbres_vars, xbres, file = file.path(MATCHES_DIR, "dat_plus_matches_study2.rda"))
save(dat5, dat4, dat3, file = file.path(MATCHES_DIR, "datasets_study2.rda"))
