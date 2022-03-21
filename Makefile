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
	$(R_OUTPUT_DIR)/results_study1_first.pdf \
	$(R_OUTPUT_DIR)/results_study1_last.pdf \
	$(R_OUTPUT_DIR)/results_study2.pdf

allfigs: $(R_FIGURES_DIR)/study1_coefplot_vax.pdf \
	$(R_FIGURES_DIR)/study2_coefplot_vax.pdf \
	$(R_FIGURES_DIR)/study1_circle_plot.pdf \
	$(R_FIGURES_DIR)/study2_circle_plot.pdf \
	$(R_FIGURES_DIR)/fig1_boxplot.pdf \
	$(R_OUTPUT_DIR)/table1.docx \
	$(R_OUTPUT_DIR)/pd_study1.docx \
	$(R_OUTPUT_DIR)/pd_study2.docx \
	$(R_OUTPUT_DIR)/risk_res_study1.docx \
	$(R_OUTPUT_DIR)/risk_res_study2.docx \
	$(R_OUTPUT_DIR)/balance_table_study1.docx \
	$(R_OUTPUT_DIR)/balance_table_study1.docx 

## Set up R package environment
renv/activate.R: renv.lock
	Rscript -e 'if(!requireNamespace("remotes")){install.packages("remotes")} else {remotes::install_github("rstudio/renv")};renv::init();renv::restore()'

## Create deduplicated data sets
$(DATA_DIR)/deduplicated_data_first.csv: $(R_ANALYSIS_DIR)/005_make_id_crosswalk.R \
	$(DATA_DIR)/MERGE_NR_2.3.21.csv \
	$(DATA_DIR)/BROW0016_OUTPUT_20220207.csv \
	renv.lock
	R -f $(R_ANALYSIS_DIR)/005_make_id_crosswalk.R

$(DATA_DIR)/deduplicated_data_last.csv: $(DATA_DIR)/deduplicated_data_first.csv

## Make and evaluate the matched research design for study 1
## Currently creates three sets of output depending on how we handle the duplicated observations
$(R_MATCHES_DIR)/dat_plus_matches_study1.rda: $(R_ANALYSIS_DIR)/011_do_matching_study1.R \
	$(R_ANALYSIS_DIR)/010_create_matched_design_study1.R \
	$(DATA_DIR)/MERGE_NR_2.3.21.csv \
	$(DATA_DIR)/deduplicated_data_first.csv \
	$(DATA_DIR)/deduplicated_data_last.csv \
	renv.lock
	R -f $(R_ANALYSIS_DIR)/011_do_matching_study1.R

## FIRST
$(R_MATCHES_DIR)/dat_plus_matches_study1_first.rda: $(R_MATCHES_DIR)/dat_plus_matches_study1.rda


## LAST
$(R_MATCHES_DIR)/dat_plus_matches_study1_last.rda: $(R_MATCHES_DIR)/dat_plus_matches_study1.rda

## Make and evaluate the matched research design for Study 2
$(R_MATCHES_DIR)/dat_plus_matches_study2.rda: $(R_ANALYSIS_DIR)/020_create_matched_design_study2.R \
	$(DATA_DIR)/TPL_Testing_Survey_FifthWave_YouGov_MERGEDWITHFOURTHWAVEFORMESSING.csv \
	renv.lock
	R -f $(R_ANALYSIS_DIR)/020_create_matched_design_study2.R

## Analyze outcomes for study 1
$(R_MATCHES_DIR)/outcome_analysis_study1.rda: $(R_MATCHES_DIR)/dat_plus_matches_study1.rda \
	$(R_MATCHES_DIR)/dat_plus_matches_study1_first.rda \
	$(R_MATCHES_DIR)/dat_plus_matches_study1_last.rda \
	$(R_ANALYSIS_DIR)/031_do_outcome_analysis_study1.R \
	$(R_ANALYSIS_DIR)/030_outcome_analysis_study1.R
	R -f $(R_ANALYSIS_DIR)/031_do_outcome_analysis_study1.R

$(R_MATCHES_DIR)/outcome_analysis_study1_first.rda: $(R_MATCHES_DIR)/outcome_analysis_study1.rda

$(R_MATCHES_DIR)/outcome_analysis_study1_last.rda: $(R_MATCHES_DIR)/outcome_analysis_study1.rda

$(DATA_DIR)/outcome_analysis_study1_first_data.csv:  $(R_MATCHES_DIR)/outcome_analysis_study1_first.rda
	mv  $(R_MATCHES_DIR)/outcome_analysis_study1_first_data.csv $(DATA_DIR)/

$(DATA_DIR)/outcome_analysis_study1_first_data.sav:  $(R_MATCHES_DIR)/outcome_analysis_study1_first.rda
	mv  $(R_MATCHES_DIR)/outcome_analysis_study1_first_data.sav $(DATA_DIR)/

$(R_MATCHES_DIR)/outcome_analysis_study1_last_data.csv: $(R_MATCHES_DIR)/outcome_analysis_study1_last.rda
$(R_MATCHES_DIR)/outcome_analysis_study1_data.csv: $(R_MATCHES_DIR)/outcome_analysis_study1.rda


## Build actual PDFs of the outcomes

$(R_OUTPUT_DIR)/results_study1.pdf: $(R_MATCHES_DIR)/outcome_analysis_study1.rda \
	$(R_ANALYSIS_DIR)/results_study1.Rmd
	Rscript -e "library(rmarkdown); render('$(R_ANALYSIS_DIR)/results_study1.Rmd')" && mv $(R_ANALYSIS_DIR)/results_study1.pdf $(R_OUTPUT_DIR)/results_study1.pdf

$(R_OUTPUT_DIR)/results_study1_first.pdf: $(R_MATCHES_DIR)/outcome_analysis_study1_first.rda \
	$(R_ANALYSIS_DIR)/results_study1_first.Rmd
	Rscript -e "library(rmarkdown); render('$(R_ANALYSIS_DIR)/results_study1_first.Rmd')" && mv $(R_ANALYSIS_DIR)/results_study1_first.pdf $(R_OUTPUT_DIR)/results_study1_first.pdf

$(R_OUTPUT_DIR)/results_study1_last.pdf: $(R_MATCHES_DIR)/outcome_analysis_study1_last.rda \
	$(R_ANALYSIS_DIR)/results_study1_last.Rmd
	Rscript -e "library(rmarkdown); render('$(R_ANALYSIS_DIR)/results_study1_last.Rmd')" && mv $(R_ANALYSIS_DIR)/results_study1_last.pdf $(R_OUTPUT_DIR)/results_study1_last.pdf


## Analyze outcomes for Study 2
$(R_OUTPUT_DIR)/results_study2.pdf: $(R_MATCHES_DIR)/outcome_analysis_study2.rda \
	$(R_ANALYSIS_DIR)/results_study2.Rmd
	Rscript -e "library(rmarkdown); render('$(R_ANALYSIS_DIR)/results_study2.Rmd')" &&  mv $(R_ANALYSIS_DIR)/results_study2.pdf $(R_OUTPUT_DIR)/results_study2.pdf

$(R_MATCHES_DIR)/outcome_analysis_study2.rda: $(R_MATCHES_DIR)/dat_plus_matches_study2.rda \
	$(R_ANALYSIS_DIR)/032_outcome_analysis_study2.R
	R -f $(R_ANALYSIS_DIR)/032_outcome_analysis_study2.R

$(DATA_DIR)/outcome_analysis_study2_data.csv: $(R_MATCHES_DIR)/outcome_analysis_study2.rda
$(DATA_DIR)/outcome_analysis_study2_data.sav: $(R_MATCHES_DIR)/outcome_analysis_study2.rda
$(R_MATCHES_DIR)/outcome_analysis_study2_data.csv: $(R_MATCHES_DIR)/outcome_analysis_study2.rda

## Figures and tables
$(R_FIGURES_DIR)/study1_coefplot_vax.pdf: $(R_ANALYSIS_DIR)/063_study1_coefplot_fig.R \
	$(R_MATCHES_DIR)/outcome_analysis_study1_first.rda
	R -f $(R_ANALYSIS_DIR)/063_study1_coefplot_fig.R

$(R_FIGURES_DIR)/study1_coefplot_other.pdf: $(R_ANALYSIS_DIR)/study1_coefplot_vax.pdf

$(R_FIGURES_DIR)/study2_coefplot_vax.pdf: $(R_ANALYSIS_DIR)/065_study2_coefplot_fig.R \
	$(R_MATCHES_DIR)/outcome_analysis_study2.rda
	R -f $(R_ANALYSIS_DIR)/065_study2_coefplot_fig.R

$(R_FIGURES_DIR)/study2_coefplot_vax.png: $(R_FIGURES_DIR)/study2_coefplot_vax.pdf

$(R_MATCHES_DIR)/study1_coefs.csv: $(R_ANALYSIS_DIR)/062_study1_circle_plot.R \
	$(R_MATCHES_DIR)/outcome_analysis_study1_first.rda
	R -f $(R_ANALYSIS_DIR)/062_study1_circle_plot.R

$(R_FIGURES_DIR)/study1_circle_plot.pdf: $(R_MATCHES_DIR)/study1_coefs.csv

$(R_MATCHES_DIR)/study2_coefs.csv: $(R_ANALYSIS_DIR)/064_study2_circle_plot.R \
	$(R_MATCHES_DIR)/outcome_analysis_study2.rda
	R -f $(R_ANALYSIS_DIR)/064_study2_circle_plot.R

$(R_FIGURES_DIR)/study2_circle_plot.pdf: $(R_MATCHES_DIR)/study2_coefs.csv

## This next should be divided into tasks. Too many products for one file.
$(R_FIGURES_DIR)/fig1_boxplot.pdf: $(R_ANALYSIS_DIR)/066_tables.R \
	$(R_ANALYSIS_DIR)/000_constants_and_utils.R \
	$(R_ANALYSIS_DIR)/001_rmd_setup.R \
	$(R_MATCHES_DIR)/dat_plus_matches_study1_first.rda \
	$(R_MATCHES_DIR)/datasets_study1_first.rda \
	$(R_MATCHES_DIR)/dat_plus_matches_study2.rda \
	$(R_MATCHES_DIR)/datasets_study2.rda
	R -f $(R_ANALYSIS_DIR)/066_tables.R

$(R_OUTPUT_DIR)/table1.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf
$(R_OUTPUT_DIR)/pd_study1.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf
$(R_OUTPUT_DIR)/pd_study2.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf
$(R_OUTPUT_DIR)/balance_table_study1.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf
$(R_OUTPUT_DIR)/balance_table_study2.docx: $(R_FIGURES_DIR)/fig1_boxplot.pdf

$(R_OUTPUT_DIR)/risk_res_study1.docx: $(R_ANALYSIS_DIR)/060_risk_perc_table_study1.R \
	$(R_ANALYSIS_DIR)/000_constants_and_utils.R \
	$(R_MATCHES_DIR)/outcome_analysis_study1_first.rda
	R -f $(R_ANALYSIS_DIR)/060_risk_perc_table_study1.R

$(R_OUTPUT_DIR)/vaxrisk_res_study1.docx: $(R_OUTPUT_DIR)/risk_res_study1.docx
$(R_OUTPUT_DIR)/vaxrisk_inv_res_study1.docx: $(R_OUTPUT_DIR)/risk_res_study1.docx

$(R_OUTPUT_DIR)/risk_res_study2.docx: $(R_ANALYSIS_DIR)/061_risk_perc_table_study2.R \
	$(R_ANALYSIS_DIR)/000_constants_and_utils.R \
	$(R_MATCHES_DIR)/outcome_analysis_study2.rda
	R -f $(R_ANALYSIS_DIR)/061_risk_perc_table_study2.R

$(R_OUTPUT_DIR)/vaxrisk_res_study2.docx: $(R_OUTPUT_DIR)/risk_res_study2.docx
$(R_OUTPUT_DIR)/vaxrisk_inv_res_study2.docx: $(R_OUTPUT_DIR)/risk_res_study2.docx

## An image of the makefile

workflowplots: build/workflow.png build/workflow.pdf

build/workflow.pdf: Makefile src/tplutils/make_p_to_json.py src/tplutils/json_to_dot.py src/tplutils/cli.py
	make -qp | python3 -m src.tplutils.cli db-to-json | python3 -m src.tplutils.cli json-to-dot | dot -Tpdf >| build/workflow.pdf

build/workflow.png: Makefile src/tplutils/make_p_to_json.py src/tplutils/json_to_dot.py src/tplutils/cli.py
	make -qp | python3 -m src.tplutils.cli db-to-json | python3 -m src.tplutils.cli json-to-dot | dot -Tpng >| build/workflow.png

## Run all stylers
style:
	Rscript -e 'styler::style_dir("src", filetype = c("R", "Rmd"))'
