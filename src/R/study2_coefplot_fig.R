## Make the figure showing all estimated effects

library(here)
library(tidyverse)
library(estimatr)

source(here("src", "R", "000_constants_and_utils.R"))

load(file = here::here(MATCHES_DIR, "outcome_analysis_study2.rda"))

lm_res_lst <- lapply(sort(ls(patt = "lm_perc")), function(lmnm) {
  res <- tidy(get(lmnm))
  res$model <- lmnm
  return(res)
})
lm_res <- bind_rows(lm_res_lst) %>%
  select(term, estimate, p.value, conf.low, conf.high, model, outcome) %>%
  mutate(across(where(is.numeric), round, 3))
lm_res

lm_res$term_label <- c(
  "City",
  "Dems among Dems", "Dems among Inds", "Dems among Reps",
  "Family/Friends",
  "Family among Dems", "Family among Reps",
  "Inds among Dems", "Inds among Inds", "Inds among Reps",
  "Ingroup (among Dems/Reps)",
  "Neighbors", "Outgroup (among Dems/Reps)",
  "Reps among Dems", "Reps among Inds", "Reps among Reps",
  "State"
)
lm_res

lm_basic <- lm_res[grep("fam$|neigh$|city$|state$|dem_dem|dem_rep|rep_rep|rep_dem", lm_res$model), ]

lm_basic$term_labelF <- factor(lm_basic$term_label, levels = c(
  "Family/Friends", "Neighbors", "City", "State",
  "Dems among Dems", "Reps among Reps", "Dems among Reps", "Reps among Dems"
))

with(lm_basic, table(term_label, term_labelF, exclude = c()))

lm_basic <- lm_basic %>% arrange(factor(lm_basic$term_label, levels = c(
  "Family/Friends", "Neighbors", "City", "State",
  "Dems among Dems", "Reps among Reps", "Dems among Reps", "Reps among Dems"
)))

## Open Sans is close to Whitney (used by Nature Human Behavior)
g_basic <- ggplot(data = lm_basic, aes(y = term_labelF, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_vline(aes(xintercept = 0), color = "grey") +
  geom_point() +
  geom_errorbarh(height = .2) +
  xlab("Average Effect of 1 Unit Difference of Perceptions on \n Vaccination or Vaccination Intention (and 95% CI)") +
  geom_text(aes(label = term_labelF), check_overlap = TRUE, nudge_y = .2, family = "Open Sans", size = 3) +
  ylab("") +
  theme_classic(base_family = "Open Sans") +
  theme(
    text = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )
## Line below prints the graph to an active device, but fails because of the font choice.
## g_basic

ggsave(file = here(FIGURES_DIR, "study2_coefplot_vax.pdf"), plot = g_basic, device = cairo_pdf)
ggsave(file = here(FIGURES_DIR, "study2_coefplot_vax.png"), plot = g_basic, type = "cairo-png", dpi = 300)
