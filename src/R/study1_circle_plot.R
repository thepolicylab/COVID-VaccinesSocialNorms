## This file produces the numbers for the circle plot for study 1

library(here)
library(estimatr)
library(tidyverse)

# Relationship between perceptions of others' intentions and own intentions in full data
source(here("src", "R", "000_constants_and_utils.R"))

load(file = here(MATCHES_DIR, "datasets_study1.rda"), verbose = TRUE)

matched_dat_study1 <- dat3
## The data pre-matching --- no missing data on outcomes or perceptions. So not *all* the data, but the *valid* data
valid_dat_study1 <- dat2
valid_dat_study1 <- dat2
full_dat_study1 <- dat0

#### Allow a real zero point to make interpreting intercepts easier
valid_dat_study1$q60_1a <- valid_dat_study1$q60_1 - 1
valid_dat_study1$q60_2a <- valid_dat_study1$q60_2 - 1
valid_dat_study1$q60_3a <- valid_dat_study1$q60_3 - 1
valid_dat_study1$q60_4a <- valid_dat_study1$q60_4 - 1

#### The global bivariate relationships

## First set of analyses on individual outcomes with the different perceptions variables.
## Willingness to be vaccinated

# N
nrow(valid_dat_study1)
nrow(matched_dat_study1)

lm_perc_global_fam <- lm_robust(q1 ~ q60_1a, data = valid_dat_study1)
lm_perc_global_neigh <- lm_robust(q1 ~ q60_2a, data = valid_dat_study1)
lm_perc_global_city <- lm_robust(q1 ~ q60_3a, data = valid_dat_study1)
lm_perc_global_state <- lm_robust(q1 ~ q60_4a, data = valid_dat_study1)
lm_perc_global_fam
lm_perc_global_neigh
lm_perc_global_city
lm_perc_global_state

study1_raw_coefs <- c(fam = coef(lm_perc_global_fam)[2], neigh = coef(lm_perc_global_neigh)[2], city = coef(lm_perc_global_city)[2], state = coef(lm_perc_global_state)[2])
print(study1_raw_coefs)

### Conditional on matching (this repeats analyses from outcome_analysis_study1.R)
load(file = here::here(MATCHES_DIR, "outcome_analysis_study1.rda"), verbose = TRUE)

# N
nrow(matched_dat_study1)
# Npairs
nrow(matched_dat_study1) / 2

study1_adjusted_coefs <- c(fam = coef(lm_perc_fam)[1], neigh = coef(lm_perc_neigh)[1], city = coef(lm_perc_city)[1], state = coef(lm_perc_state)[1])
print(study1_adjusted_coefs)

## comparing the two results

rbind(unadjusted = study1_raw_coefs, adjusted = study1_adjusted_coefs)

## Differences in effects across levels
rbind(
  diff(study1_raw_coefs),
  diff(study1_adjusted_coefs)
)


## The product:

study1_coefs <- rbind(unadjusted = study1_raw_coefs, adjusted = study1_adjusted_coefs)

write.csv(study1_coefs, file = here(MATCHES_DIR, "study1_coefs.csv"))


library(ggforce)
library(ggrepel)


study1_circle_dat <- data.frame(
  raw_coefs = study1_raw_coefs,
  adj_coefs = study1_adjusted_coefs,
  centers = c(.5, .4, .2, .1),
  radius = c(.1, .2, .38, .5),
  ypos = rep(0, 4),
  offset = c(0, .1, .25, .35),
  community = c("Family/Friends", "Neighborhood", "City", "State")
)

g_circles_study1 <- ggplot(data = study1_circle_dat, aes(x0 = centers, r = radius, y0 = ypos)) +
  geom_circle(aes(linetype = community)) +
  geom_circle(aes(x0 = .52, y0 = .07, r = .02), fill = "gray", color = "white") +
  geom_text(aes(x = .52, y = .07, label = "Self"), color = "black") +
  ylab("") +
  xlab("") +
  geom_text(aes(
    x = centers - offset,
    y = 0,
    label = paste("beta[full~data]==", round(raw_coefs, 2))
  ), parse = TRUE) +
  geom_text(aes(
    x = centers - offset,
    y = -.02,
    label = paste("beta[pairs]==", round(adj_coefs, 2))
  ), parse = TRUE) +
  geom_text_repel(aes(x = centers - offset, y = 0, label = community), nudge_y = .02) +
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
g_circles_study1

ggsave(filename = here(FIGURES_DIR, "study1_circle_plot.png"), dpi = 300, type = "cairo-png", width = 10, height = 10)
ggsave(file = here(FIGURES_DIR, "study1_circle_plot.pdf"), device = cairo_pdf, width = 10, height = 10)


################ Old stuff#######
#
# ## Using pdf and will convert by hand to png or something. Having trouble with
# ## getting the png at the right resolution without messing with fonts (could
# ## put this in Makefile using imagemagick convert for example).
# library(plotrix)
#
# pdf(
#   file = here("src/R/figures", "study1_circleplot.pdf"),
#   bg = "transparent", width = 12, height = 4
# )
# par(mfrow = c(1, 2), pty = "m", oma = rep(0, 4), mar = rep(0, 4), mgp = rep(0, 3))
# thexlim <- c(.02, .6)
# plot(thexlim, c(-2, 2), type = "n", axes = FALSE, xlab = "", ylab = "")
# radii <- c(.08, .13, .15, .19)
# theoffset <- c(0, .05, .1, .1)
# for (i in seq_len(length(study1_raw_coefs))) {
#   print(study1_raw_coefs[[i]])
#   draw.circle(x = round(study1_raw_coefs[[i]], 4), y = 0, radius = radii[i], border = "black", lty = i, col = gray(i / 4, alpha = .25))
# }
# text(x = study1_raw_coefs - theoffset, y = 0, label = round(study1_raw_coefs, 2))
# draw.circle(x = .49, y = .3, radius = .02, col = "black")
# text(x = .49, y = .3, label = "Self", col = "white")
#
# plot(thexlim, c(-2, 2), type = "n", axes = FALSE, xlab = "", ylab = "")
# # radii <- c(.08,.13,.15,.16)
# theoffset2 <- c(0, .07, .12, .16)
# for (i in seq_len(length(study1_adjusted_coefs))) {
#   print(study1_adjusted_coefs[[i]])
#   draw.circle(x = round(study1_adjusted_coefs[[i]], 4), y = 0, radius = radii[i], border = "black", lty = i, col = gray(i / 4, alpha = .25))
# }
# text(x = study1_adjusted_coefs - theoffset2, y = 0, label = round(study1_adjusted_coefs, 2))
# draw.circle(x = study1_adjusted_coefs[1] + .07, y = .3, radius = .02, col = "black")
# text(x = study1_adjusted_coefs[1] + .07, y = .3, label = "Self", col = "white")
# dev.off()
