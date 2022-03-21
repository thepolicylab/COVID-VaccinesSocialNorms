## Outcome analysis study 2
## Do the analyses and save them as an R binary file for use in a results .Rmd
## document

library(tidyverse)
library(here)
library(coin)
library(estimatr)
library(SensitivityCaseControl)
library(RItools)
library(haven)

source(here("src", "R", "000_constants_and_utils.R"))

load(file = here(MATCHES_DIR, "dat_plus_matches_study2.rda"), verbose = TRUE)

stopifnot(all.equal(dat5$outcome.x, dat5$outcome.y))
dat5$outcome <- dat5$outcome.x
outcomes <- "outcome"
summary(dat5[, outcomes])
## What are the sds of the outcome variables?
dat5 %>% summarize(across(contains(outcomes), sd))

## 2. Key variables with uninterpretable names are:
## q126 Do you plan to get vaccinated as soon as it's possible to do so? (only asked of people who said they have not already begun vaccination, q120=2)
## q60_1 How many people in your network of friends and family do you believe will definitely get vaccinated for coronavirus
## q60_2 How many people in the neigborhood where you live do you believe will definitely get vaccinated for coronavirus
## q60_3 How many people in your city or town do you believe will definitely get vaccinated for coronavirus
## q60_4 How many people in your state do you believe will definitely get vaccinated for coronavirus
## q60_5 How many Democrats do you believe will definitely get vaccinated for coronavirus
## q60_6 How many Republicans do you believe will definitely get vaccinated for coronavirus
## q60_7 How many Independents do you believe will definitely get vaccinated for coronavirus

## Recode explanatories to have a real zero
perceptions <- c("q60_1", "q60_2", "q60_3", "q60_4", "q60_5", "q60_6", "q60_7", "q25_new")
summary(dat5[, perceptions])
dat5$q60_1a <- dat5$q60_1 - 1
dat5$q60_2a <- dat5$q60_2 - 1
dat5$q60_3a <- dat5$q60_3 - 1
dat5$q60_4a <- dat5$q60_4 - 1
dat5$q60_5a <- dat5$q60_5 - 1
dat5$q60_6a <- dat5$q60_6 - 1
dat5$q60_7a <- dat5$q60_7 - 1

summary(dat5$outcome)
sd(dat5$outcome)

## First set of analyses on individual outcomes with the different perceptions variables.
## Willingness to be vaccinated or Definitely will be vaccinated
lm_perc_fam <- lm_robust(outcome ~ q60_1a, fixed_effects = ~bm, data = dat5)
lm_perc_neigh <- lm_robust(outcome ~ q60_2a, fixed_effects = ~bm, data = dat5)
lm_perc_city <- lm_robust(outcome ~ q60_3a, fixed_effects = ~bm, data = dat5)
lm_perc_state <- lm_robust(outcome ~ q60_4a, fixed_effects = ~bm, data = dat5)

lm_perc_fam
lm_perc_neigh
lm_perc_city
lm_perc_state



## Do perceptions of Democrats or Republicans matter differently for Democrats/Republicans?
## dem_rep_oth: 1=dem, 2=rep,3=other (indep, dk, other)
lm_perc_dem_dem <- lm_robust(outcome ~ q60_5a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 1)
lm_perc_dem_rep <- lm_robust(outcome ~ q60_5a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 2)
lm_perc_dem_ind <- lm_robust(outcome ~ q60_5a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 3)
lm_perc_rep_dem <- lm_robust(outcome ~ q60_6a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 1)
lm_perc_rep_rep <- lm_robust(outcome ~ q60_6a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 2)
lm_perc_rep_ind <- lm_robust(outcome ~ q60_6a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 3)
lm_perc_ind_dem <- lm_robust(outcome ~ q60_7a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 1)
lm_perc_ind_rep <- lm_robust(outcome ~ q60_7a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 2)
lm_perc_ind_ind <- lm_robust(outcome ~ q60_7a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 3)

lm_perc_dem_dem ## ceiling effect among dems
lm_perc_dem_rep
lm_perc_dem_ind

lm_perc_rep_dem
lm_perc_rep_rep
lm_perc_rep_ind

lm_perc_ind_dem
lm_perc_ind_rep
lm_perc_ind_ind

## Effect of in-group
lm_perc_dem_dem
lm_perc_rep_rep

## Effect of out-group
lm_perc_dem_rep
lm_perc_rep_dem

dat5$q60_ingroup <- with(dat5, case_when(dem_rep_oth == 1 ~ q60_5a, dem_rep_oth == 2 ~ q60_6a, dem_rep_oth == 3 ~ NA_real_))
## Check the recode
with(dat5, table(dem_rep_oth, q60_ingroup, exclude = c()))
with(dat5, table(q60_5, q60_ingroup, dem_rep_oth, exclude = c()))
with(dat5, table(q60_6, q60_ingroup, dem_rep_oth, exclude = c()))

dat5$q60_outgroup <- with(dat5, case_when(dem_rep_oth == 1 ~ q60_6a, dem_rep_oth == 2 ~ q60_5a, dem_rep_oth == 3 ~ NA_real_))
## Check the recode
with(dat5, table(dem_rep_oth, q60_outgroup, exclude = c()))
with(dat5, table(q60_5, q60_outgroup, dem_rep_oth, exclude = c()))
with(dat5, table(q60_6, q60_outgroup, dem_rep_oth, exclude = c()))

## Average effect of ingroup perceptions (focusing only on dem and reps)
lm_perc_ingroup <- lm_robust(outcome ~ q60_ingroup, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth != 3)
lm_perc_outgroup <- lm_robust(outcome ~ q60_outgroup, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth != 3)

lm_perc_ingroup
lm_perc_outgroup

## PID is perfectly correlated with pair indicator. So can't use interaction effects in a simple way here to assess whether effects are really different between indeps versus dem versus rep. One workaround is random effects

library(lme4)
lme_perc_ingroup <- lmer(outcome ~ q60_ingroup * I(dem_rep_oth == 2) + (1 | bm), data = dat5, subset = dem_rep_oth != 3)
summary(lme_perc_ingroup)
confint(lme_perc_ingroup)

lme_perc_outgroup <- lmer(outcome ~ q60_outgroup * I(dem_rep_oth == 2) + (1 | bm), data = dat5, subset = dem_rep_oth != 3)
summary(lme_perc_outgroup)
confint(lme_perc_outgroup)

## Family by PID

lme_perc_fam <- lmer(outcome ~ q60_1a * I(dem_rep_oth == 2) + (1 | bm), data = dat5, subset = dem_rep_oth != 3)
summary(lme_perc_fam)
confint(lme_perc_fam)

lme_perc_fam0 <- lmer(outcome ~ q60_1a + (1 | bm), data = dat5)

## Willingness to be vaccinated or Definitely will be vaccinated
lm_perc_fam_dem <- lm_robust(outcome ~ q60_1a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 1)
lm_perc_fam_rep <- lm_robust(outcome ~ q60_1a, fixed_effects = ~bm, data = dat5, subset = dem_rep_oth == 2)

lm_perc_fam_dem
lm_perc_fam_rep

## To look at whether democrat pairs differ from republican pairs, we could reduce the data to pair level:

dat5_paired <- dat5 %>%
  group_by(bm) %>%
  summarize(
    outcome = diff(outcome),
    q60_1a = diff(q60_1a),
    q60_2a = diff(q60_2a),
    q60_3a = diff(q60_3a),
    q60_4a = diff(q60_4a),
    q60_5a = diff(q60_5a),
    q60_6a = diff(q60_6a),
    q60_7a = diff(q60_7a),
    pid = unique(dem_rep_oth)
  )
head(dat5_paired)
# in bm=1, person 1 has outcome ===1, and person 2 has outcome ==0, both are republicans.
# person 1 perceives more people in their family vaccinated, etc.. (i.e. 2 is a positive number)

## Are more positive numbers on perceptions associated with more positive numbers on outcomes?
lm_robust(outcome ~ q60_1a, data = dat5_paired)
## compare to the fixed effects version --- very close
lm_perc_fam

## But now we can test for differences in effects between partisan pairs
lm_robust(outcome ~ q60_1a * I(pid == 2), data = dat5_paired, subset = pid != 3)


## Does risk perceptions predict (1) other-preceptions and (2) own
## intentions?

## First transform to ranks within set: changes the meaning of the coefficients
## but doesn't change the statistical inference in any meaningful way or the
## signs of the coefs. This makes it easier to use the "odds" language and
## framework of Rosenbaum et all.
dat5 <- dat5 %>%
  group_by(bm) %>%
  mutate(
    q60_1a_rank = rank(q60_1a) - 1,
    q60_2a_rank = rank(q60_2a) - 1,
    q60_3a_rank = rank(q60_3a) - 1,
    q60_4a_rank = rank(q60_4a) - 1,
    q60_5a_rank = rank(q60_5a) - 1,
    q60_6a_rank = rank(q60_6a) - 1,
    q60_7a_rank = rank(q60_7a) - 1,
    outcome_rank = rank(outcome) - 1, ## outcome rank
    q22_rank = rank(q22) - 1, ##  likelihood of getting infected with coronavirus in the next month (1-100 %).
    q59_1_rank = rank(q59_1) - 1 ## agreement with the statement "Getting vaccinated would put me at risk for vaccine side effects" (1-5 scale)
  ) %>%
  ungroup()

## Notice that sometimes the higher perceiver has a 1 and sometimes a 6 (mostly higher numbers of course).
## Sometimes the higher ranked person within a pair has a lower value than
## another person in another pair. This is the benefit of our paired design.
## a rank of .5 means the two people are the same
with(dat5, table(q60_1a_rank, q60_1a, exclude = c()))
with(dat5, table(outcome_rank, outcome, exclude = c()))
with(dat5, table(q22_rank, q22, exclude = c()))

library(epitools)

### For perceived COVID risk
risk_perc_fam_or <- with(
  filter(dat5, q22_rank != .5 & q60_1a_rank != .5),
  oddsratio(x = q22_rank, y = q60_1a_rank)
)
risk_perc_neigh_or <- with(filter(dat5, q22_rank != .5 & q60_2a_rank != .5), oddsratio(x = q22_rank, y = q60_2a_rank))
risk_perc_city_or <- with(filter(dat5, q22_rank != .5 & q60_3a_rank != .5), oddsratio(x = q22_rank, y = q60_3a_rank))
risk_perc_state_or <- with(filter(dat5, q22_rank != .5 & q60_4a_rank != .5), oddsratio(x = q22_rank, y = q60_4a_rank))
### Odds on outcomes:
risk_intention_or <- with(filter(dat5, q22_rank != .5 & outcome_rank != .5), oddsratio(x = q22_rank, y = outcome_rank))
## Vaccination Risk Perception results
risk_res <- rbind(
  family = risk_perc_fam_or$measure["1", ],
  neighbors = risk_perc_neigh_or$measure["1", ],
  city = risk_perc_city_or$measure["1", ],
  state = risk_perc_state_or$measure["1", ],
  own_intentions = risk_intention_or$measure["1", ]
)


## Calc by hand for easier and more clear interpretation
## OR = odds of event in exposed group/odds of event in not exposed group
## OR for us = odds of event for high risk perceptions/odds of event for low risk
## perceptions

## See help page for oddsratio. This table structure is preferred
risk_perc_fam_tab <- risk_perc_fam_or$data
## Odds of higher perceptions of family among those low in risk perceptions
A <- risk_perc_fam_tab[1, 2] / risk_perc_fam_tab[1, 1]
## Odds of higher perceptions of family among those high in risk perceptions
B <- risk_perc_fam_tab[2, 2] / risk_perc_fam_tab[2, 1]
c(A, B, B / A)
risk_perc_fam_or$measure

### Now for risk of vaccination inconvenience.
vaxrisk_perc_fam_or <- with(filter(dat5, q59_1_rank != .5 & q60_1a_rank != .5), oddsratio(x = q59_1_rank, y = q60_1a_rank))
vaxrisk_perc_neigh_or <- with(filter(dat5, q59_1_rank != .5 & q60_2a_rank != .5), oddsratio(x = q59_1_rank, y = q60_2a_rank))
vaxrisk_perc_city_or <- with(filter(dat5, q59_1_rank != .5 & q60_3a_rank != .5), oddsratio(x = q59_1_rank, y = q60_3a_rank))
vaxrisk_perc_state_or <- with(filter(dat5, q59_1_rank != .5 & q60_4a_rank != .5), oddsratio(x = q59_1_rank, y = q60_4a_rank))
### Odds on outcomes:
vaxrisk_intention_or <- with(filter(dat5, q59_1_rank != .5 & outcome_rank != .5), oddsratio(x = q59_1_rank, y = outcome_rank))
## Vaccination Risk Perception results
vaxrisk_res <- rbind(
  family = vaxrisk_perc_fam_or$measure["1", ],
  neighbors = vaxrisk_perc_neigh_or$measure["1", ],
  city = vaxrisk_perc_city_or$measure["1", ],
  state = vaxrisk_perc_state_or$measure["1", ],
  own_intentions = vaxrisk_intention_or$measure["1", ]
)

## Flipping coding to q59_1 so that we have "increased" odds:

dat5$q59_1_rank_inv <- 1 - dat5$q59_1_rank
vaxrisk_inv_perc_fam_or <- with(filter(dat5, q59_1_rank_inv != .5 & q60_1a_rank != .5), oddsratio(x = q59_1_rank_inv, y = q60_1a_rank))
vaxrisk_inv_perc_neigh_or <- with(filter(dat5, q59_1_rank_inv != .5 & q60_2a_rank != .5), oddsratio(x = q59_1_rank_inv, y = q60_2a_rank))
vaxrisk_inv_perc_city_or <- with(filter(dat5, q59_1_rank_inv != .5 & q60_3a_rank != .5), oddsratio(x = q59_1_rank_inv, y = q60_3a_rank))
vaxrisk_inv_perc_state_or <- with(filter(dat5, q59_1_rank_inv != .5 & q60_4a_rank != .5), oddsratio(x = q59_1_rank_inv, y = q60_4a_rank))
### Odds on outcomes:
vaxrisk_inv_intention_or <- with(filter(dat5, q59_1_rank_inv != .5 & outcome_rank != .5), oddsratio(x = q59_1_rank_inv, y = outcome_rank))
## Vaccination Risk Perception results
vaxrisk_inv_res <- rbind(
  family = vaxrisk_inv_perc_fam_or$measure["1", ],
  neighbors = vaxrisk_inv_perc_neigh_or$measure["1", ],
  city = vaxrisk_inv_perc_city_or$measure["1", ],
  state = vaxrisk_inv_perc_state_or$measure["1", ],
  own_intentions = vaxrisk_inv_intention_or$measure["1", ]
)

vaxrisk_intention_or
vaxrisk_inv_intention_or
chisq.test(vaxrisk_inv_intention_or$data)
chisq.test(vaxrisk_intention_or$data)

save(list = c("dat5", "xbres", "risk_res", "vaxrisk_res", "vaxrisk_inv_res", ls(patt = "lm")), file = here::here(MATCHES_DIR, "outcome_analysis_study2.rda"))

write_csv(dat5, file = here::here(DATA_DIR, "outcome_analysis_study2_data.csv"))
write_sav(dat5, path = here::here(DATA_DIR, "outcome_analysis_study2_data.sav"))
