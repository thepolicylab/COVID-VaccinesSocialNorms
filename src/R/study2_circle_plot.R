## This file produces the numbers for the circle plot for study 2

library(here)
library(estimatr)
library(tidyverse)

# Relationship between perceptions of others' intentions and own intentions

### In full data
#### Data setup: fixing coding of missing data, etc.. Copied from matching_setup_study2.R

source(here("src", "R", "000_constants_and_utils.R"))

load(file = here(MATCHES_DIR, "datasets_study2.rda"), verbose = TRUE)

matched_dat_study2 <- dat5
## The data pre-matching --- no missing data on outcomes or perceptions. So not *all* the data, but the *valid* data
valid_dat_study2 <- dat3
rm(dat3, dat5)

#### Allow a real zero point to make interpreting intercepts easier
valid_dat_study2$q60_1a <- valid_dat_study2$q60_1 - 1
valid_dat_study2$q60_2a <- valid_dat_study2$q60_2 - 1
valid_dat_study2$q60_3a <- valid_dat_study2$q60_3 - 1
valid_dat_study2$q60_4a <- valid_dat_study2$q60_4 - 1
valid_dat_study2$q60_5a <- valid_dat_study2$q60_5 - 1
valid_dat_study2$q60_6a <- valid_dat_study2$q60_6 - 1
valid_dat_study2$q60_7a <- valid_dat_study2$q60_7 - 1
#### The global bivariate relationships

## First set of analyses on individual outcomes with the different perceptions variables.
## Willingness to be vaccinated or actuall vaccinated

# N
nrow(valid_dat_study2)

lm_perc_global_fam <- lm_robust(outcome ~ q60_1a, data = valid_dat_study2)
lm_perc_global_neigh <- lm_robust(outcome ~ q60_2a, data = valid_dat_study2)
lm_perc_global_city <- lm_robust(outcome ~ q60_3a, data = valid_dat_study2)
lm_perc_global_state <- lm_robust(outcome ~ q60_4a, data = valid_dat_study2)
lm_perc_global_fam
lm_perc_global_neigh
lm_perc_global_city
lm_perc_global_state


## Do perc_globaleptions of Democrats or Republicans matter differently for Democrats/Republicans?
## dem_rep_oth: 1=dem, 2=rep,3=other (indep, dk, other)
lm_perc_global_dem_dem <- lm_robust(outcome ~ q60_5a, data = valid_dat_study2, subset = dem_rep_oth == 1)
lm_perc_global_dem_rep <- lm_robust(outcome ~ q60_5a, data = valid_dat_study2, subset = dem_rep_oth == 2)
lm_perc_global_rep_dem <- lm_robust(outcome ~ q60_6a, data = valid_dat_study2, subset = dem_rep_oth == 1)
lm_perc_global_rep_rep <- lm_robust(outcome ~ q60_6a, data = valid_dat_study2, subset = dem_rep_oth == 2)
lm_perc_global_dem_dem
lm_perc_global_dem_rep
lm_perc_global_rep_dem
lm_perc_global_rep_rep

study2_raw_coefs <- c(
  fam = coef(lm_perc_global_fam)[2],
  neigh = coef(lm_perc_global_neigh)[2],
  city = coef(lm_perc_global_city)[2],
  state = coef(lm_perc_global_state)[2],
  demdem = coef(lm_perc_global_dem_dem)[2],
  demrep = coef(lm_perc_global_dem_rep)[2],
  repdem = coef(lm_perc_global_rep_dem)[2],
  reprep = coef(lm_perc_global_rep_rep)[2]
)

## How to save these out? Just csv?
print(study2_raw_coefs)

### Conditional on matching (this repeats analyses from outcome_analysis.R)
load(file = here::here(MATCHES_DIR, "outcome_analysis_study2.rda"))

# N
nrow(dat5)
# Npairs
nrow(dat5) / 2

study2_adjusted_coefs <- c(
  coef(lm_perc_fam)[1],
  coef(lm_perc_neigh)[1], coef(lm_perc_city)[1],
  coef(lm_perc_state)[1], coef(lm_perc_dem_dem)[1],
  coef(lm_perc_dem_rep)[1], coef(lm_perc_rep_dem)[1],
  coef(lm_perc_rep_rep)[1]
)
print(study2_adjusted_coefs)

## comparing the two results

rbind(unadjusted = study2_raw_coefs, adjusted = study2_adjusted_coefs)

## Differences in effects across levels
rbind(
  diff(study2_raw_coefs),
  diff(study2_adjusted_coefs)
)


## The product:

study2_coefs <- rbind(unadjusted = study2_raw_coefs, adjusted = study2_adjusted_coefs)

write.csv(study2_coefs, file = here(MATCHES_DIR, "study2_coefs.csv"))


## Figure
library(ggforce)
library(ggrepel)

## will have to add some of the partisan data by hand
study2_circle_dat <- data.frame(
  raw_coefs = study2_raw_coefs[c(1:5, 8)],
  adj_coefs = study2_adjusted_coefs[c(1:5, 8)],
  centers = c(.5, .4, .2, .1, .6, .6),
  radius = c(.1, .2, .38, .5, .5, .5),
  ypos = c(rep(0, 4), -.2, -.2),
  offset = c(0, .1, .25, .35, -.2, -.2),
  community = c("Family/Friends", "Neighborhood", "City", "State", "Partisan In-Group (Dems)", "Partisan In-Group (Reps)"),
  selector = c(1, 1, 1, 1, 2, 2),
  party = c(rep(NULL, 4), "Dems", "Reps")
)

g_circles_study2 <- ggplot(
  data = study2_circle_dat,
  aes(x0 = centers, r = radius, y0 = ypos)
) +
  geom_circle(aes(linetype = community)) +
  geom_circle(aes(x0 = .52, y0 = .07, r = .02), fill = "gray", color = "white") +
  geom_text(aes(x = .52, y = .07, label = "Self"), color = "black") +
  ylab("") +
  xlab("") +
  geom_text(data = filter(study2_circle_dat, selector == 1), aes(
    x = centers - offset,
    y = -.01,
    label = paste("beta[full~data]==", round(raw_coefs, 2))
  ), parse = TRUE) +
  geom_text(data = filter(study2_circle_dat, selector == 1), aes(
    x = centers - offset,
    y = -.03,
    label = paste("beta[pairs]==", round(adj_coefs, 2))
  ), parse = TRUE) +
  geom_text_repel(data = filter(study2_circle_dat, selector == 1), aes(x = centers - offset, y = 0, label = community), nudge_y = .02, segment.color = "gray") +
  geom_text(aes(x = .8, y = .02, label = "Partisan In-Group")) +
  geom_text(
    data = filter(study2_circle_dat, selector == 2),
    aes(
      x = centers - offset, y = c(-.01, -.04),
      label = paste("beta[full~data]==", round(raw_coefs, 2), "~", party)
    ), parse = TRUE
  ) +
  geom_text(
    data = filter(study2_circle_dat, selector == 2),
    aes(
      x = centers - offset, y = c(-.07, -.10),
      label = paste("beta[pairs]==", round(adj_coefs, 2), "~", party)
    ), parse = TRUE
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    legend.position = "none"
  )
g_circles_study2

ggsave(filename = here(FIGURES_DIR, "study2_circle_plot.png"), dpi = 300, type = "cairo-png", width = 13, height = 10)
ggsave(file = here(FIGURES_DIR, "study2_circle_plot.pdf"), device = cairo_pdf, width = 13, height = 10)
