## Make the figure showing all estimated effects for study1

library(here)
library(tidyverse)
library(estimatr)

source(here("src", "R", "000_constants_and_utils.R"))

load(file = here(MATCHES_DIR, "outcome_analysis_study1.rda"))

lm_res_lst <- lapply(ls(patt = "lm_perc|lm_test|lm_mask"), function(lmnm) {
  res <- tidy(get(lmnm))
  res$model <- lmnm
  return(res)
})
lm_res <- bind_rows(lm_res_lst) %>%
  select(term, estimate, p.value, conf.low, conf.high, model, outcome) %>%
  mutate(across(where(is.numeric), round, 3))
lm_res

lm_res$term_label <- c("Mask Wearing", "City", "Family/Friends", "Neighbors", "State", "Testing")
lm_res <- lm_res %>% arrange(desc(estimate))
lm_res$term_labelF <- factor(lm_res$term_label, levels = lm_res$term_label)
vax_plot_data <- lm_res[grep("q60", lm_res$term), ] %>% droplevels()
oth_plot_data <- lm_res[lm_res$term_label %in% c("Mask Wearing", "Testing"), ] %>% droplevels()

## Setup the figure
## Open Sans is close to Whitney (used by Nature Human Behavior)
g_vax <- ggplot(data = vax_plot_data, aes(y = term_labelF, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_vline(aes(xintercept = 0), color = "grey") +
  geom_point() +
  geom_errorbarh(height = .2) +
  xlab("Average Effect of 1 Unit Difference of Perceptions (scale 0 to 6) on \n Vaccination Intentions (scale 1 to 5) (and 95% CI)") +
  geom_text(aes(label = term_labelF), check_overlap = TRUE, nudge_y = .2, family = "Open Sans", size = 3) +
  ylab("") +
  theme_classic(base_family = "Open Sans") +
  scale_x_continuous(limits = c(0, .5)) +
  theme(
    text = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )
## Line below prints the graph to an active device, but fails because of the font choice.
# g_vax

g_other <- g_vax %+% oth_plot_data +
  geom_errorbarh(height = .1) +
  xlab("Average Effect of 1 Unit Difference of Perceptions of Family/Friend Behavior on \n Testing and Masking Intentions (and 95% CI)") +
  scale_x_continuous(limits = c(.15, 1))
# g_other

ggsave(file = here(FIGURES_DIR, "study1_coefplot_vax.pdf"), plot = g_vax, device = cairo_pdf)
ggsave(file = here(FIGURES_DIR, "study1_coefplot_vax.png"), plot = g_vax, type = "cairo-png", dpi = 300)
ggsave(file = here(FIGURES_DIR, "study1_coefplot_other.pdf"), plot = g_other, device = cairo_pdf)
ggsave(file = here(FIGURES_DIR, "study1_coefplot_other.png"), plot = g_other, type = "cairo-png", dpi = 300)
