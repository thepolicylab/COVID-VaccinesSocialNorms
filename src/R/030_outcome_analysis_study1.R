## Outcome analysis for Study 1
## Creates objects for use and interpretation in results_study1.Rmd

library(tidyverse)
library(here)
library(coin)
library(estimatr)
library(SensitivityCaseControl)
library(RItools)
library(haven)

source(here("src", "R", "000_constants_and_utils.R"))

do_outcome_analysis <- function(infile, outfile) {
  load(file = infile, verbose = TRUE)

  ## Outcomes: not looked at here
  # q1 - Intention to get vaccinated
  # q29 - Willingness to test
  # q7 - Mask wearing
  outcomes <- c("q1", "q29", "q7")
  summary(dat3[, outcomes])
  ## What are the sds of the outcome variables?
  dat3 %>% summarize(across(contains(outcomes), sd))
  stopifnot(max(dat3$q7, na.rm = TRUE) == 5 & max(dat3$q29, na.rm = TRUE) == 6 & max(dat3$q1, na.rm = TRUE) == 5)

  ## Recode explanatories to have a real zero
  perceptions <- c("q60_1", "q60_2", "q60_3", "q60_4") ## ,"q100")
  summary(dat3[, perceptions])
  dat3$q60_1a <- dat3$q60_1 - 1
  dat3$q60_2a <- dat3$q60_2 - 1
  dat3$q60_3a <- dat3$q60_3 - 1
  dat3$q60_4a <- dat3$q60_4 - 1

  summary(dat3$q1)
  sd(dat3$q1)

  # Note that the fixed_effects here are meaningless. This just allows us to get an
  # easily readable description of these variables. The real workhorse is the next
  # set of lines utilizing `lm_robust`
  lm(q60_1a ~ 1, data = dat3)
  lm(q60_2a ~ 1, data = dat3)
  lm(q60_3a ~ 1, data = dat3)
  lm(q60_4a ~ 1, data = dat3)

  ## First set of analyses on individual outcomes with the different perceptions variables.
  ## Willingness to be vaccinated
  lm_perc_fam <- lm_robust(q1 ~ q60_1a, fixed_effects = ~bm, data = dat3)
  lm_perc_neigh <- lm_robust(q1 ~ q60_2a, fixed_effects = ~bm, data = dat3)
  lm_perc_city <- lm_robust(q1 ~ q60_3a, fixed_effects = ~bm, data = dat3)
  lm_perc_state <- lm_robust(q1 ~ q60_4a, fixed_effects = ~bm, data = dat3)

  lm_perc_fam
  lm_perc_neigh
  lm_perc_city
  lm_perc_state

  ## We don't want to "control for" each perceptions so we don't do this
  ## lm_perc_all <- lm_robust(q1~q60_1a+q60_2a+q60_3a+q60_4a,fixed_effects=~bm,data=dat3)

  ## an overall test.
  xb1 <- xBalance(q1 ~ q60_1a + q60_2a + q60_3a + q60_4a, strata = list(pairs = ~bm), data = dat3, report = "all")
  xb1
  ## Could try https://www.rdocumentation.org/packages/SensitivityCaseControl/versions/2.1/topics/adaptive.noether.brown as we discussed in the analysis plan. This will have more power and also enable sensitivity analysis.
  ## adaptive.noether.brown(y,Gamma,alpha=.05,lambda1=1/3,lambda2=2/3)

  ## Another multivariate test
  dat3$bmF <- factor(dat3$bm)
  mvtest1 <- independence_test(q1 ~ q60_1a + q60_2a + q60_3a + q60_4a | bmF, data = dat3)
  mvtest1

  ## Another version (both same)
  mvtest2 <- independence_test(q60_1a + q60_2a + q60_3a + q60_4a ~ q1 | bmF, data = dat3)
  mvtest2

  ## from Rosenbaum 1994 (https://mran.revolutionanalytics.com/snapshot/2015-07-13/web/packages/coin/coin.pdf)
  coherence <- function(data, block = NULL) {
    ## an observation is "smaller" than another obs when all outcomes are are smaller
    ## score is a rank
    ## - compare all outcomes of observation i with all outcomes of all others.
    ## - say we have 4 outcomes, count number of times obs i has outcomes lower
    ##     than each other observation. (say, 0 times lower, or only one out
    ##     of the 4 lower, or all four lower).
    ## - count the number of times that an observation has all four below the
    ##     other obs (i.e. is the lowest ranked of all of the other obs).
    ## - also count the number of times that an observation has all four
    ##     above the other obs.
    ## - subtract these two numbers to get a rank.
    simp_coherence_fn <- function(data) {
      x <- as.matrix(data)
      res0 <- apply(x, 1, function(y) {
        sum(colSums(t(x) < y) == ncol(x)) -
          sum(colSums(t(x) > y) == ncol(x))
      })
      res <- matrix(res0, ncol = 1)
      return(res)
    }
    if (is.null(block)) {
      simp_coherence_fn(data)
    } else {
      res0 <- unsplit(lapply(split(data, block), function(dat) {
        simp_coherence_fn(dat)
      }), block)
      res <- matrix(res0, ncol = 1)
    }
  }

  junk <- coherence(dat3[, c("q60_1a", "q60_2a", "q60_3a", "q60_4a")], block = dat3$bmF)

  ## Somehow I had to hard code the blocking into a function definition for coin
  coherence_blocked <- function(data) {
    coherence(data, block = dat3$bmF)
  }

  mvtest3 <- independence_test(q60_1a + q60_2a + q60_3a + q60_4a ~ q1 | bmF,
    data =
      dat3, ytrafo = coherence_blocked
  )
  mvtest3

  mvtest4 <- independence_test(q1 ~ q60_1a + q60_2a + q60_3a + q60_4a | bmF, data = dat3, xtrafo = coherence_blocked)
  mvtest4

  ## dat2 <- dat3 %>%
  ##  filter(bmF == "2") %>%
  ##  select(matches("q60_[0-9]a$"))
  ## Putting on hold idea of testing hypotheses of order in general
  ## in favor of the seeming unrelated regression approach below

  ## Willingness to be tested
  lm_test_fam <- lm_robust(q29 ~ q25new, fixed_effects = ~bm, data = dat3)
  lm_test_fam

  ## Mask wearing
  lm_mask_fam <- lm_robust(q7 ~ q100, fixed_effects = ~bm, data = dat3)
  table(q7 = dat3$q7, q100 = dat3$q100)
  lm_mask_fam


  ## Testing hypotheses of differences in effect of different perceptions
  library(systemfit)
  ## First matched-set mean center the variables so avoid fixed effectrs problems
  ## This is what is going on behind the scenes in lm_robust anyway

  dat3 <- dat3 %>%
    group_by(bm) %>%
    mutate(
      q1_c = q1 - mean(q1),
      q60_1a_c = q60_1a - mean(q60_1a),
      q60_2a_c = q60_2a - mean(q60_2a),
      q60_3a_c = q60_3a - mean(q60_3a),
      q60_4a_c = q60_4a - mean(q60_4a)
    ) %>%
    ungroup()

  ## Test to show that fixed effects are the same as within-matched set mean deviating
  test1 <- lm_robust(q1_c ~ q60_1a_c, data = dat3)
  test2 <- lm_robust(q1 ~ q60_1a, fixed_effects = ~bm, data = dat3)
  stopifnot(all.equal(test1$coef[["q60_1a_c"]], test2$coef[["q60_1a"]]))

  fameq <- q1_c ~ q60_1a_c
  neigheq <- q1_c ~ q60_2a_c
  cityeq <- q1_c ~ q60_3a_c
  stateeq <- q1_c ~ q60_4a_c

  sursfit <- systemfit(list(fam = fameq, neigh = neigheq, city = cityeq, state = stateeq), data = dat3)

  ## test the hypothesis that perceptions of family differ from all other perceptions in their effects.
  restriction1 <- c("fam_q60_1a_c - neigh_q60_2a_c = 0", "fam_q60_1a_c - city_q60_3a_c = 0", "fam_q60_1a_c - state_q60_4a_c = 0")
  test_fam_vs_bigger <- linearHypothesis(sursfit, restriction1, test = "Chisq")

  restriction2 <- c("neigh_q60_2a_c - city_q60_3a_c = 0", "neigh_q60_2a_c - state_q60_4a_c = 0")
  test_neigh_vs_bigger <- linearHypothesis(sursfit, restriction2, test = "Chisq")

  restriction3 <- c("city_q60_3a_c - state_q60_4a_c = 0")
  test_city_vs_bigger <- linearHypothesis(sursfit, restriction3, test = "Chisq")

  ## Does risk perceptions predict (1) other-preceptions and (2) own
  ## intentions?

  ## First transform to ranks within set: changes the meaning of the coefficients
  ## but doesn't change the statistical inference in any meaningful way or the
  ## signs of the coefs. This makes it easier to use the "odds" language and
  ## framework of Rosenbaum et all.
  dat3 <- dat3 %>%
    group_by(bm) %>%
    mutate(
      q60_1a_rank = rank(q60_1a) - 1,
      q60_2a_rank = rank(q60_2a) - 1,
      q60_3a_rank = rank(q60_3a) - 1,
      q60_4a_rank = rank(q60_4a) - 1,
      q100_rank = rank(q100) - 1,
      q25_new_rank = rank(q25new) - 1,
      q1_rank = rank(q1) - 1, ## outcome rank
      q22_rank = rank(q22) - 1, ##  likelihood of getting infected with coronavirus in the next month (1-100 %).
      q59_1_rank = rank(q59_1) - 1 ## agreement with the statement "Getting vaccinated would put me at risk for vaccine side effects" (1-5 scale)
    ) %>%
    ungroup()

  ## Notice that sometimes the higher perceiver has a 1 and sometimes a 6 (mostly higher numbers of course).
  ## Sometimes the higher ranked person within a pair has a lower value than
  ## another person in another pair. This is the benefit of our paired design.
  ## a rank of .5 means the two people are the same
  with(dat3, table(q60_1a_rank, q60_1a, exclude = c()))
  with(dat3, table(q1_rank, q1, exclude = c()))
  with(dat3, table(q22_rank, q22, exclude = c()))


  ## Notice below that the coefficient estimate doesn't change with fixed effects
  ## because we have within-pair differences as our variables. This means we can
  ## use logit to get closer to "odds" or maybe even just calculate with a table.
  risk_perc_famA <- lm_robust(q60_1a_rank ~ q22_rank, fixed_effects = ~bm, data = filter(dat3, q22_rank != .5 & q60_1a_rank != .5))
  risk_perc_famB <- lm_robust(q60_1a_rank ~ q22_rank, data = filter(dat3, q22_rank != .5 & q60_1a_rank != .5))
  risk_perc_famA
  risk_perc_famB


  ## alternatively useing logit on pair differences
  ## FYI https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression

  ## Here we play around with different approaches to getting CIs for Odds Ratios.
  ## The idea of the CI is that it gives us a reasonable range to calibrate the
  ## sensitivity analysis.
  risk_perc_fam_glm <- glm(q60_1a_rank ~ q22_rank, data = filter(dat3, q22_rank != .5 & q60_1a_rank != .5), family = binomial(link = "logit"))
  risk_perc_fam_glm
  ## The Odds Ratio:
  exp(coef(risk_perc_fam_glm))
  ## library(epiDisplay)
  ## logistic.display(risk_perc_fam_glm)
  library(oddsratio)
  or_glm(data = filter(dat3, q22_rank != .5 & q60_1a_rank != .5), model = risk_perc_fam_glm, incr = list(q22_rank = 1))

  library(epitools)
  risk_perc_fam_tab <- with(filter(dat3, q22_rank != .5 & q60_1a_rank != .5), table(q22_rank, q60_1a_rank))
  oddsratio(risk_perc_fam_tab)
  risk_perc_fam_or <- with(filter(dat3, q22_rank != .5 & q60_1a_rank != .5), oddsratio(x = q22_rank, y = q60_1a_rank))
  risk_perc_fam_or$p.value
  risk_perc_fam_or$measure["1", ]

  ## I like the epitools way but am leaving the other approaches for others to
  ## learn from.
  risk_perc_neigh_or <- with(filter(dat3, q22_rank != .5 & q60_2a_rank != .5), oddsratio(x = q22_rank, y = q60_2a_rank))
  risk_perc_city_or <- with(filter(dat3, q22_rank != .5 & q60_3a_rank != .5), oddsratio(x = q22_rank, y = q60_3a_rank))
  risk_perc_state_or <- with(filter(dat3, q22_rank != .5 & q60_4a_rank != .5), oddsratio(x = q22_rank, y = q60_4a_rank))

  ### Odds on outcomes:
  risk_intention_or <- with(filter(dat3, q22_rank != .5 & q1_rank != .5), oddsratio(x = q22_rank, y = q1_rank))

  ## Risk Perception results
  risk_res <- rbind(
    family = risk_perc_fam_or$measure["1", ],
    neighbors = risk_perc_neigh_or$measure["1", ],
    city = risk_perc_city_or$measure["1", ],
    state = risk_perc_state_or$measure["1", ],
    own_intentions = risk_intention_or$measure["1", ]
  )

  ### Now for risk of vaccination inconvenience.
  vaxrisk_perc_fam_or <- with(filter(dat3, q59_1_rank != .5 & q60_1a_rank != .5), oddsratio(x = q59_1_rank, y = q60_1a_rank))
  vaxrisk_perc_neigh_or <- with(filter(dat3, q59_1_rank != .5 & q60_2a_rank != .5), oddsratio(x = q59_1_rank, y = q60_2a_rank))
  vaxrisk_perc_city_or <- with(filter(dat3, q59_1_rank != .5 & q60_3a_rank != .5), oddsratio(x = q59_1_rank, y = q60_3a_rank))
  vaxrisk_perc_state_or <- with(filter(dat3, q59_1_rank != .5 & q60_4a_rank != .5), oddsratio(x = q59_1_rank, y = q60_4a_rank))
  ### Odds on outcomes:
  vaxrisk_intention_or <- with(filter(dat3, q59_1_rank != .5 & q1_rank != .5), oddsratio(x = q59_1_rank, y = q1_rank))
  ## Vaccination Risk Perception results
  vaxrisk_res <- rbind(
    family = vaxrisk_perc_fam_or$measure["1", ],
    neighbors = vaxrisk_perc_neigh_or$measure["1", ],
    city = vaxrisk_perc_city_or$measure["1", ],
    state = vaxrisk_perc_state_or$measure["1", ],
    own_intentions = vaxrisk_intention_or$measure["1", ]
  )

  ## Flipping coding to q59_1 so that we have "increased" odds:
  dat3$q59_1_rank_inv <- 1 - dat3$q59_1_rank
  vaxrisk_inv_perc_fam_or <- with(filter(dat3, q59_1_rank_inv != .5 & q60_1a_rank != .5), oddsratio(x = q59_1_rank_inv, y = q60_1a_rank))
  vaxrisk_inv_perc_neigh_or <- with(filter(dat3, q59_1_rank_inv != .5 & q60_2a_rank != .5), oddsratio(x = q59_1_rank_inv, y = q60_2a_rank))
  vaxrisk_inv_perc_city_or <- with(filter(dat3, q59_1_rank_inv != .5 & q60_3a_rank != .5), oddsratio(x = q59_1_rank_inv, y = q60_3a_rank))
  vaxrisk_inv_perc_state_or <- with(filter(dat3, q59_1_rank_inv != .5 & q60_4a_rank != .5), oddsratio(x = q59_1_rank_inv, y = q60_4a_rank))
  ### Odds on outcomes:
  vaxrisk_inv_intention_or <- with(filter(dat3, q59_1_rank_inv != .5 & q1_rank != .5), oddsratio(x = q59_1_rank_inv, y = q1_rank))
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

  save(dat3, xbres, lm_mask_fam, lm_test_fam, lm_perc_fam, lm_perc_neigh, lm_perc_city, lm_perc_state, xb1, mvtest1, mvtest2, mvtest3, mvtest4,
    test_city_vs_bigger, test_neigh_vs_bigger, test_fam_vs_bigger, sursfit, risk_res, vaxrisk_res, vaxrisk_inv_res,
    file = outfile
  )

  write_csv(dat3, paste0(gsub(".rda", "", outfile), "_data.csv"))
  write_sav(dat3, paste0(gsub(".rda", "", outfile), "_data.sav"))
}
