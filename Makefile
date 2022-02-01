## A Makefile to organize our work
SHELL = /bin/sh
NB_DIR = notebooks/external
DATA_DIR = data
RES_DIR = resources
INTERM_DIR = data_interm
R_ANALYSIS_DIR=src/R
R_DATA_DIR=data
R_MATCHES_DIR=$(R_DATA_DIR)/calculated
R_OUTPUT_DIR=build
R_FIGURES_DIR=$(R_OUTPUT_DIR)/figures

.PHONY: all allfigs allresults

all: allresults allfigs

allresults: $(R_OUTPUT_DIR)/results_study1.pdf \
	$(R_OUTPUT_DIR)/results_study2.pdf

allfigs: $(R_FIGURES_DIR)/study1_coefplot_vax.pdf \
	$(R_FIGURES_DIR)/study2_coefplot_vax.pdf \
	$(R_FIGURES_DIR)/study1_circle_plot.pdf \
	$(R_FIGURES_DIR)/study2_circle_plot.pdf \
	$(R_FIGURES_DIR)/fig1_boxplot.pdf \
	$(R_OUTPUT_DIR)/table1.docx \
	$(R_OUTPUT_DIR)/pd_study1.docx \
	$(R_OUTPUT_DIR)/pd_study2.docx

## Set up R package environment
renv/activate.R: renv.lock
	Rscript -e 'if(!requireNamespace("remotes")){install.packages("remotes")} else {remotes::install_github("rstudio/renv")};renv::init();renv::restore()'

## Make and evaluate the matched research design for study 1
$(R_MATCHES_DIR)/dat_plus_matches_study1.rda: $(R_ANALYSIS_DIR)/010_create_matched_design_study1.R \
	$(DATA_DIR)/MERGE_NR_2.3.21.csv \
	renv.lock
	R -f $(R_ANALYSIS_DIR)/010_create_matched_design_study1.R

$(R_MATCHES_DIR)/datasets_study1.rda: $(R_MATCHES_DIR)/dat_plus_matches_study1.rda

## Make and evaluate the matched research design for Study 2
$(R_MATCHES_DIR)/dat_plus_matches_study2.rda: $(R_ANALYSIS_DIR)/020_create_matched_design_study2.R \
	$(DATA_DIR)/TPL_Testing_Survey_FifthWave_YouGov_MERGEDWITHFOURTHWAVEFORMESSING.csv \
	renv.lock
	R -f $(R_ANALYSIS_DIR)/020_create_matched_design_study2.R

$(R_MATCHES_DIR)/datasets_study2.rda: $(R_MATCHES_DIR)/dat_plus_matches_study2.rda

## Analyze outcomes for study 1
$(R_MATCHES_DIR)/outcome_analysis_study1.rda: $(R_MATCHES_DIR)/dat_plus_matches_study1.rda \
	$(R_ANALYSIS_DIR)/outcome_analysis_study1.R
	R -f $(R_ANALYSIS_DIR)/outcome_analysis_study1.R

$(R_OUTPUT_DIR)/results_study1.pdf: $(R_MATCHES_DIR)/outcome_analysis_study1.rda \
	$(R_ANALYSIS_DIR)/results_study1.Rmd
	Rscript -e "library(rmarkdown); render('$(R_ANALYSIS_DIR)/results_study1.Rmd')" && mv $(R_ANALYSIS_DIR)/results_study1.pdf $(R_OUTPUT_DIR)/results_study1.pdf

## Analyze outcomes for Study 2
$(R_OUTPUT_DIR)/results_study2.pdf: $(R_MATCHES_DIR)/outcome_analysis_study2.rda \
	$(R_ANALYSIS_DIR)/results_study2.Rmd
	Rscript -e "library(rmarkdown); render('$(R_ANALYSIS_DIR)/results_study2.Rmd')" &&  mv $(R_ANALYSIS_DIR)/results_study2.pdf $(R_OUTPUT_DIR)/results_study2.pdf

$(R_MATCHES_DIR)/outcome_analysis_study2.rda: $(R_MATCHES_DIR)/dat_plus_matches_study2.rda \
	$(R_ANALYSIS_DIR)/outcome_analysis_study2.R
	R -f $(R_ANALYSIS_DIR)/outcome_analysis_study2.R

## Figures and tables
$(R_FIGURES_DIR)/study1_coefplot_vax.pdf: $(R_ANALYSIS_DIR)/study1_coefplot_fig.R \
	$(R_MATCHES_DIR)/outcome_analysis_study1.rda
	R -f $(R_ANALYSIS_DIR)/study1_coefplot_fig.R

$(R_FIGURES_DIR)/study1_coefplot_other.pdf: $(R_FIGURES_DIR)/study1_coefplot_vax.pdf

$(R_FIGURES_DIR)/study2_coefplot_vax.pdf: $(R_ANALYSIS_DIR)/study2_coefplot_fig.R \
	$(R_MATCHES_DIR)/outcome_analysis_study2.rda
	R -f $(R_ANALYSIS_DIR)/study2_coefplot_fig.R

$(R_FIGURES_DIR)/study2_coefplot_vax.png: $(R_FIGURES_DIR)/study2_coefplot_vax.pdf

$(R_MATCHES_DIR)/study1_coefs.csv: $(R_ANALYSIS_DIR)/study1_circle_plot.R \
	$(R_MATCHES_DIR)/outcome_analysis_study1.rda
	R -f $(R_ANALYSIS_DIR)/study1_circle_plot.R

$(R_FIGURES_DIR)/study1_circle_plot.pdf: $(R_MATCHES_DIR)/study1_coefs.csv

$(R_MATCHES_DIR)/study2_coefs.csv: $(R_ANALYSIS_DIR)/study2_circle_plot.R \
	$(R_MATCHES_DIR)/outcome_analysis_study2.rda
	R -f $(R_ANALYSIS_DIR)/study2_circle_plot.R

$(R_FIGURES_DIR)/study2_circle_plot.pdf: $(R_MATCHES_DIR)/study2_coefs.csv

## This next should be divided into tasks. Too many products for one file.
$(R_FIGURES_DIR)/fig1_boxplot.pdf: $(R_ANALYSIS_DIR)/tables.R \
	$(R_ANALYSIS_DIR)/000_constants_and_utils.R \
	$(R_ANALYSIS_DIR)/rmd_setup.R \
	$(R_MATCHES_DIR)/dat_plus_matches_study1.rda \
	$(R_MATCHES_DIR)/datasets_study1.rda \
	$(R_MATCHES_DIR)/dat_plus_matches_study2.rda \
	$(R_MATCHES_DIR)/datasets_study2.rda
	R -f $(R_ANALYSIS_DIR)/tables.R

$(R_OUTPUT_DIR)/table1.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf
$(R_OUTPUT_DIR)/pd_study1.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf
$(R_OUTPUT_DIR)/pd_study2.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf
$(R_OUTPUT_DIR)/balance_table_study1.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf
$(R_OUTPUT_DIR)/balance_table_study2.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf

$(R_OUTPUT_DIR)/risk_res_study1.docx: $(R_ANALYSIS_DIR)/risk_perc_table_study1.R \
	$(R_ANALYSIS_DIR)/000_constants_and_utils.R
	R -f $(R_ANALYSIS_DIR)/risk_perc_table_study1.R

$(R_OUTPUT_DIR)/vaxrisk_res_study1.docx: $(R_OUTPUT_DIR)/risk_res_study1.docx
$(R_OUTPUT_DIR)/vaxrisk_inv_res_study1.docx: $(R_OUTPUT_DIR)/risk_res_study1.docx

$(R_OUTPUT_DIR)/risk_res_study2.docx: $(R_ANALYSIS_DIR)/risk_perc_table_study2.R \
	$(R_ANALYSIS_DIR)/000_constants_and_utils.R
	R -f $(R_ANALYSIS_DIR)/risk_perc_table_study2.R

$(R_OUTPUT_DIR)/vaxrisk_res_study2.docx: $(R_OUTPUT_DIR)/risk_res_study2.docx
$(R_OUTPUT_DIR)/vaxrisk_inv_res_study2.docx: $(R_OUTPUT_DIR)/risk_res_study2.docx

## Data used in results

$(R_DATA_DIR)/study1_dat_plus_matches_dat3.csv: $(R_ANALYSIS_DIR)/000_constants_and_utils.R \
	$(R_ANALYSIS_DIR)/rmd_setup.R \
	$(R_MATCHES_DIR)/outcome_analysis_study2.rda \
	$(R_MATCHES_DIR)/outcome_analysis_study1.rda \
	$(R_ANALYSIS_DIR)/extract_post_matching_data.R
	R -f $(R_ANALYSIS_DIR)/extract_post_matching_data.R

$(R_DATA_DIR)/study1_dat_plus_matches_dat5.csv: $(R_DATA_DIR)/study1_dat_plus_matches_dat3.csv

## An image of the makefile

workflowplots: build/workflow.png build/workflow.pdf

build/workflow.pdf: Makefile src/tplutils/make_p_to_json.py src/tplutils/json_to_dot.py src/tplutils/cli.py
	make -qp | python3 -m src.tplutils.cli db-to-json | python3 -m src.tplutils.cli json-to-dot | dot -Tpdf >| build/workflow.pdf

build/workflow.png: Makefile src/tplutils/make_p_to_json.py src/tplutils/json_to_dot.py src/tplutils/cli.py
	make -qp | python3 -m src.tplutils.cli db-to-json | python3 -m src.tplutils.cli json-to-dot | dot -Tpng >| build/workflow.png

## Run all stylers
style:
	Rscript -e 'styler::style_dir("src", filetype = c("R", "Rmd"))'
