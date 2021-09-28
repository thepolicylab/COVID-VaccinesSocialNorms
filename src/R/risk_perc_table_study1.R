## Make a table showing how risk perceptions (risk of COVID itself, risk of
## vaccine complications) relate to both other perceptions
## and own intentions.

library(here)
library(tidyverse)
library(flextable)
source(here("src", "R", "000_constants_and_utils.R"))
load(file = here::here(MATCHES_DIR, "outcome_analysis_study1.rda"))

set_flextable_defaults(digits = 2)

risk_res_df <- as.data.frame(round(risk_res, 3))
names(risk_res_df) <- c("Odds", "Lower CI", "Upper CI")
risk_res_df$var <- rownames(risk_res_df)
risk_res_study1 <- flextable(risk_res_df)
risk_res_study1 <- theme_booktabs(risk_res_study1)
risk_res_study1 <- autofit(risk_res_study1, add_w = 0, add_h = 0)
risk_res_study1 <- set_caption(risk_res_study1, "Does the person who has higher COVID Risk Perceptions within a pair also tend to perceive more support for vaccinations in their social environment and/or tend to support vaccination for themselves? Study 1")
risk_res_study1 <- footnote(risk_res_study1,
  i = 1, j = 1:3,
  value = as_paragraph(
    c("Entries are odds of the person with higher COVID risk perceptions within a pair having the higher value on the variable (perceptions or intentions). Analysis restricted to pairs that differ on both COVID risk perceptions and value of the other-perceptions or own-intentions variable.")
  ),
  part = "header"
)
risk_res_study1 <- valign(risk_res_study1, valign = "bottom", part = "header")
risk_res_study1 <- autofit(risk_res_study1)
save_as_docx(
  "COVID Risk Perceptions Effects Study 1" = risk_res_study1,
  path = here(OUTPUT_DIR, "risk_res_study1.docx")
)


vaxrisk_res_df <- as.data.frame(round(vaxrisk_res, 3))
names(vaxrisk_res_df) <- c("Odds", "Lower CI", "Upper CI")
vaxrisk_res_df$var <- rownames(vaxrisk_res_df)
vaxrisk_res_study1 <- flextable(vaxrisk_res_df)
vaxrisk_res_study1 <- theme_booktabs(vaxrisk_res_study1)
vaxrisk_res_study1 <- autofit(vaxrisk_res_study1, add_w = 0, add_h = 0)
vaxrisk_res_study1 <- set_caption(vaxrisk_res_study1, "Does the person who has higher Vaccine Risk Perceptions within a pair also tend to perceive more support for vaccinations in their social environment and/or tend to support vaccination for themselves? Study 1")
vaxrisk_res_study1 <- footnote(vaxrisk_res_study1,
  i = 1, j = 1:3,
  value = as_paragraph(
    c("Entries are odds of the person with higher Vaccination Risk perceptions within a pair having the higher value on the variable (perceptions or intentions). Analysis restricted to pairs that differ on both Vaccine Risk perceptions and value of the other-perceptions or own-intentions variable.")
  ),
  part = "header"
)
vaxrisk_res_study1 <- valign(vaxrisk_res_study1, valign = "bottom", part = "header")
vaxrisk_res_study1 <- autofit(vaxrisk_res_study1)
save_as_docx(
  "COVID vaxrisk Perceptions Effects Study 1" = vaxrisk_res_study1,
  path = here(OUTPUT_DIR, "vaxrisk_res_study1.docx")
)

## Flipping the q59_1 rank (high -> low, low->high)
vaxrisk_inv_res_df <- as.data.frame(round(vaxrisk_inv_res, 3))
names(vaxrisk_inv_res_df) <- c("Odds", "Lower CI", "Upper CI")
vaxrisk_inv_res_df$var <- rownames(vaxrisk_inv_res_df)
vaxrisk_inv_res_study1 <- flextable(vaxrisk_inv_res_df)
vaxrisk_inv_res_study1 <- theme_booktabs(vaxrisk_inv_res_study1)
vaxrisk_inv_res_study1 <- autofit(vaxrisk_inv_res_study1, add_w = 0, add_h = 0)
vaxrisk_inv_res_study1 <- set_caption(vaxrisk_inv_res_study1, "Does the person who has lower Vaccine Risk Perceptions within a pair also tend to perceive more support for vaccinations in their social environment and/or tend to support vaccination for themselves? Study 1")
vaxrisk_inv_res_study1 <- footnote(vaxrisk_inv_res_study1,
  i = 1, j = 1:3,
  value = as_paragraph(
    c("Entries are odds of the person with lower Vaccination Risk perceptions within a pair having the higher value on the variable (perceptions or intentions). Analysis restricted to pairs that differ on both Vaccine Risk perceptions and value of the other-perceptions or own-intentions variable.")
  ),
  part = "header"
)
vaxrisk_inv_res_study1 <- valign(vaxrisk_inv_res_study1, valign = "bottom", part = "header")
vaxrisk_inv_res_study1 <- autofit(vaxrisk_inv_res_study1)
save_as_docx(
  "COVID vaxrisk_inv Perceptions Effects Study 1" = vaxrisk_inv_res_study1,
  path = here(OUTPUT_DIR, "vaxrisk_inv_res_study1.docx")
)

save_as_image(vaxrisk_inv_res_study1, path = here(FIGURES_DIR, "vaxrisk_inv_res_study1.png"), webshot = "webshot")
save_as_image(vaxrisk_res_study1, path = here(FIGURES_DIR, "vaxrisk_res_study1.png"), webshot = "webshot")
