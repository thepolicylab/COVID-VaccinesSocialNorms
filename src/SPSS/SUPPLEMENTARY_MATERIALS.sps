* Encoding: UTF-8.




/* S2. Does using a continuous rather than binary outcome variable in Study 2 alter the results?
/* Use file TPL_Testing_Survey_FifthWave_YouGov_MERGEDWITHFOURTHWAVEFORMESSING.sav

USE ALL.
COMPUTE filter_$=(sample < 3).
VARIABLE LABELS filter_$ 'sample < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

WEIGHT OFF.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q126
  /METHOD=ENTER q60_1 gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT already_vaccinated_or_definitely_will_get_vaccinated
  /METHOD=ENTER q60_1 gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q126
  /METHOD=ENTER q60_2 gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT already_vaccinated_or_definitely_will_get_vaccinated
  /METHOD=ENTER q60_2 gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q126
  /METHOD=ENTER q60_3 gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT already_vaccinated_or_definitely_will_get_vaccinated
  /METHOD=ENTER q60_3 gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q126
  /METHOD=ENTER q60_4 gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT already_vaccinated_or_definitely_will_get_vaccinated
  /METHOD=ENTER q60_4 gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.


/* S5. Are perceptions of others’ vaccination intentions accurate?

/* S5.1. Operationalizing accuracy
/* Use file pulse2021_puf_27 available from https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html

IF  (RECVDVACC = 1 | GETVACC = 1) received_vaccine_or_definitely_will_get_vaccinated=1.
EXECUTE.

RECODE received_vaccine_or_definitely_will_get_vaccinated (1=1) (SYSMIS=0).
EXECUTE.

* Define Variable Properties.
*EST_ST.
VALUE LABELS EST_ST
  1 'Alabama'
  2 'Alaska'
  4 'Arizona'
  5 'Arkansas'
  6 'California'
  8 'Colorado'
  9 'Connecticut'
  10 'Delaware'
  11 'District of Columbia'
  12 'Florida'
  13 'Georgia'
  15 'Hawaii'
  16 'Idaho'
  17 'Illinois'
  18 'Indiana'
  19 'Iowa'
  20 'Kansas'
  21 'Kentucky'
  22 'Louisiana'
  23 'Maine'
  24 'Maryland'
  25 'Massachusetts'
  26 'Michigan'
  27 'Minnesota'
  28 'Mississippi'
  29 'Missouri'
  30 'Montana'
  31 'Nebraska'
  32 'Nevada'
  33 'New Hampshire'
  34 'New Jersey'
  35 'New Mexico'
  36 'New York'
  37 'North Carolina'
  38 'North Dakota'
  39 'Ohio'
  40 'Oklahoma'
  41 'Oregon'
  42 'Pennsylvania'
  44 'Rhode Island'
  45 'South Carolina'
  46 'South Dakota'
  47 'Tennessee'
  48 'Texas'
  49 'Utah'
  50 'Vermont'
  51 'Virginia'
  53 'Washington'
  54 'West Virginia'
  55 'Wisconsin'
  56 'Wyoming'.
EXECUTE.


WEIGHT BY PWEIGHT.

CROSSTABS
  /TABLES=received_vaccine_or_definitely_will_get_vaccinated BY EST_ST
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.


/* Use file TPL_Testing_Survey_FifthWave_YouGov_MERGEDWITHFOURTHWAVEFORMESSING.sav 
/* Create a new variable indicating accuracy of respondents' state-level estimates

IF  (inputstate = 1 & q60_4 = 4 | inputstate = 1 & q60_4 = 5 | inputstate = 2 & q60_4 = 4 | 
    inputstate = 2 & q60_4 = 5 | inputstate = 4 & q60_4 = 4 | inputstate = 4 & q60_4 = 5 | inputstate = 
    5 & q60_4 = 4 | inputstate = 5 & q60_4 = 5 | inputstate = 6 & q60_4 = 5 | inputstate = 6 & q60_4 = 
    6 | inputstate = 8 & q60_4 = 4 | inputstate = 8 & q60_4 = 5 | inputstate = 9 & q60_4 = 5 | 
    inputstate = 9 & q60_4 = 6 | inputstate = 10 & q60_4 = 4 | inputstate = 10 & q60_4 = 5 | inputstate 
    = 11 & q60_4 = 5 | inputstate = 11 & q60_4 = 6 | inputstate = 12 & q60_4 = 4 | inputstate = 12 & 
    q60_4 = 5 | inputstate = 13 & q60_4 = 4 | inputstate = 13 & q60_4 = 5 | inputstate = 15 & q60_4 = 5 
    | inputstate = 15 & q60_4 = 6 | inputstate = 16 & q60_4 = 4 | inputstate = 16 & q60_4 = 5 | 
    inputstate = 17 & q60_4 = 5 | inputstate = 17 & q60_4 = 6 | inputstate = 18 & q60_4 = 4 | 
    inputstate = 18 & q60_4 = 5 |  inputstate = 19 & q60_4 = 4 | inputstate = 19 & q60_4 = 5 | 
    inputstate = 20 & q60_4 = 4 | inputstate = 20 & q60_4 = 5 | inputstate = 21 & q60_4 = 4 | 
    inputstate = 21 & q60_4 = 5 | inputstate = 22 & q60_4 = 4 | inputstate = 22 & q60_4 = 5 | 
    inputstate = 23 & q60_4 = 5 | inputstate = 23 & q60_4 = 6 | inputstate = 24 & q60_4 = 4 | 
    inputstate = 24 & q60_4 = 5 | inputstate = 25 & q60_4 = 5 | inputstate = 25 & q60_4 = 6 |  
    inputstate = 26 & q60_4 = 4 | inputstate = 26 & q60_4 = 5 | inputstate = 27 & q60_4 = 5 | 
    inputstate = 27 & q60_4 = 6 | inputstate = 28 & q60_4 = 4 | inputstate = 28 & q60_4 = 5 | 
    inputstate = 29 & q60_4 = 4 | inputstate = 29 & q60_4 = 5 | inputstate = 30 & q60_4 = 4 | 
    inputstate = 30 & q60_4 = 5 | inputstate = 31 & q60_4 = 4 | inputstate = 31 & q60_4 = 5 |
    inputstate = 32 & q60_4 = 4 | inputstate = 32 & q60_4 = 5 | inputstate = 33 & q60_4 = 5 | 
    inputstate = 33 & q60_4 = 6 | inputstate = 34 & q60_4 = 4 | inputstate = 34 & q60_4 = 5 |
    inputstate = 35 & q60_4 = 5 | inputstate = 35 & q60_4 = 6 |  inputstate = 36 & q60_4 = 4 | 
    inputstate = 36 & q60_4 = 5 | inputstate = 37 & q60_4 = 4 | inputstate = 37 & q60_4 = 5 |
    inputstate = 38 & q60_4 = 4 | inputstate = 38 & q60_4 = 5 | inputstate = 39 & q60_4 = 4 | 
    inputstate = 39 & q60_4 = 5 | inputstate = 40 & q60_4 = 4 | inputstate = 40 & q60_4 = 5 |
    inputstate = 41 & q60_4 = 4 | inputstate = 41 & q60_4 = 5 | inputstate = 42 & q60_4 = 4 | 
    inputstate = 42 & q60_4 = 5 | inputstate = 44 & q60_4 = 5 | inputstate = 44 & q60_4 = 6 |
    inputstate = 45 & q60_4 = 4 | inputstate = 45 & q60_4 = 5 | inputstate = 46 & q60_4 = 4 | 
    inputstate = 46 & q60_4 = 5 | inputstate = 47 & q60_4 = 4 | inputstate = 47 & q60_4 = 5 |
    inputstate = 48 & q60_4 = 4 | inputstate = 48 & q60_4 = 5 | inputstate = 49 & q60_4 = 4 | 
    inputstate = 49 & q60_4 = 5 | inputstate = 50 & q60_4 = 5 | inputstate = 50 & q60_4 = 6 |
    inputstate = 51 & q60_4 = 5 | inputstate = 51 & q60_4 = 6 | inputstate = 53 & q60_4 = 5 | 
    inputstate = 53 & q60_4 = 6 | inputstate = 54 & q60_4 = 4 | inputstate = 54 & q60_4 = 5 |
    inputstate = 55 & q60_4 = 4 | inputstate = 55 & q60_4 = 5 | inputstate = 56 & q60_4 = 4 |
    inputstate = 56 & q60_4 = 5) state_estimate_accurate=1.
EXECUTE.

RECODE state_estimate_accurate (1=1) (SYSMIS=0).
EXECUTE.


/* Create a new variable indicating accuracy of respondents' partisan group estimates

RECODE q60_5 (5=1) (6=1) (ELSE=0) INTO Democrats_estimate_accuracy.
EXECUTE.

RECODE q60_6 (3=1) (4=1) (ELSE=0) INTO Republicans_estimate_accuracy.
EXECUTE.

RECODE q60_7 (4=1) (5=1) (ELSE=0) INTO Independents_estimate_accuracy.
EXECUTE.


/* S5.2. How accurate are people’s beliefs about others’ vaccination intentions?

USE ALL.
COMPUTE filter_$=(sample < 3).
VARIABLE LABELS filter_$ 'sample < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=state_estimate_accurate Democrats_estimate_accuracy Republicans_estimate_accuracy Independents_estimate_accuracy
  /ORDER=ANALYSIS.


/* S5.3. Accuracy of statewide intentions by demographics


USE ALL.
COMPUTE filter_$=(sample < 3).
VARIABLE LABELS filter_$ 'sample < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

CORRELATIONS
  /VARIABLES=age state_estimate_accurate
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CROSSTABS
  /TABLES=state_estimate_accurate BY gender_client race educ pid3
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.



/* S5.4. Is perceived statewide intention to vaccinate or accuracy of perceived statewide intention to vaccinate a better predictor of own intention to vaccinate?
/* Note this regression is identical to those used above (comparing binary to continuous outcomes). The sole difference is the addition of the accuracy variable.


USE ALL.
COMPUTE filter_$=(sample < 3).
VARIABLE LABELS filter_$ 'sample < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT already_vaccinated_or_definitely_will_get_vaccinated
  /METHOD=ENTER q60_4 state_estimate_accurate gender_dummy_coded_female party_dummy_coded_republican 
    party_dummy_coded_independent race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded 
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.

/* Is perceived partisan ingroup intention to vaccinate or accuracy of perceived partisan ingroup intention to vacciante a better predictor of own intention to vaccinate?
/* Note that we create variables for Independents for future analyses of this interesting group. We do not use the variables here because of the ambiguity in determining who should count as Independents' out-partisans.

IF  (pid3 = 1) ingroup_estimate_accuracy_interimD=Democrats_estimate_accuracy.
EXECUTE.

IF  (pid3 = 2) ingroup_estimate_accuracy_interimR=Republicans_estimate_accuracy.
EXECUTE.

IF  (pid3 = 3) ingroup_estimate_accuracy_interimI=Independents_estimate_accuracy.
EXECUTE.

RECODE ingroup_estimate_accuracy_interimD ingroup_estimate_accuracy_interimR 
    ingroup_estimate_accuracy_interimI (SYSMIS=0).
EXECUTE.

COMPUTE ingroup_estimate_accuracy=ingroup_estimate_accuracy_interimD + 
    ingroup_estimate_accuracy_interimR + ingroup_estimate_accuracy_interimI.
EXECUTE.

USE ALL.
COMPUTE filter_$=(sample < 3 & pid3 < 3).
VARIABLE LABELS filter_$ 'sample < 3 & pid3 < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT already_vaccinated_or_definitely_will_get_vaccinated
  /METHOD=ENTER ingroup_vaccination_intention ingroup_estimate_accuracy gender_dummy_coded_female 
    party_dummy_coded_republican race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded
    HS_dummy_coded Some_college_dummy_coded two_year_degree_dummy_coded age.


/* S6. Does the magnitude of the gap between perceived friends/family and perceived statewide intentions to vaccinate predict one’s own vaccineintention?

COMPUTE network_friends_family_versus_state_gap=q60_4 - q60_1.
EXECUTE.

USE ALL.
COMPUTE filter_$=(sample < 3).
VARIABLE LABELS filter_$ 'sample < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

CORRELATIONS
  /VARIABLES=already_vaccinated_or_definitely_will_get_vaccinated 
    network_friends_family_versus_state_gap
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.


/* Does this magnitude differ by demographics?

USE ALL.
COMPUTE filter_$=(sample < 3).
VARIABLE LABELS filter_$ 'sample < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST GROUPS=gender_client(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=network_friends_family_versus_state_gap
  /CRITERIA=CI(.95).


USE ALL.
COMPUTE filter_$=(sample < 3 & race ~= 5 & race ~= 7 & race ~= 8).
VARIABLE LABELS filter_$ 'sample < 3 & race ~= 5 & race ~= 7 & race ~= 8 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

ONEWAY network_friends_family_versus_state_gap BY race
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

USE ALL.
COMPUTE filter_$=(sample < 3).
VARIABLE LABELS filter_$ 'sample < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

ONEWAY network_friends_family_versus_state_gap BY educ
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

CORRELATIONS
  /VARIABLES=age network_friends_family_versus_state_gap
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.



/* S7. Is there an effect of question order (social norms before versus after intentions)?
/* Use MERGE_NR_2.3.21.sav

USE ALL.
COMPUTE filter_$=(survey = 8).
VARIABLE LABELS filter_$ 'survey = 8 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

RECODE q60_treat (1=1) (ELSE=0) INTO social_norms_before_intentions.
EXECUTE.


COMPUTE question_order_by_social_norms_interaction=social_norms_before_intentions * q60_1.
EXECUTE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q1
  /METHOD=ENTER q60_1 party_dummy_coded_republican party_dummy_coded_independent 
    race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded HS_dummy_coded 
    Some_college_dummy_coded two_year_degree_dummy_coded male_dummy_coded age 
    social_norms_before_intentions question_order_by_social_norms_interaction.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q1
  /METHOD=ENTER q60_2 party_dummy_coded_republican party_dummy_coded_independent 
    race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded HS_dummy_coded 
    Some_college_dummy_coded two_year_degree_dummy_coded male_dummy_coded age 
    social_norms_before_intentions question_order_by_social_norms_interaction.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q1
  /METHOD=ENTER q60_3 party_dummy_coded_republican party_dummy_coded_independent 
    race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded HS_dummy_coded 
    Some_college_dummy_coded two_year_degree_dummy_coded male_dummy_coded age 
    social_norms_before_intentions question_order_by_social_norms_interaction.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q1
  /METHOD=ENTER q60_4 party_dummy_coded_republican party_dummy_coded_independent 
    race_dummy_coded_black race_dummy_coded_latino No_HS_dummy_coded HS_dummy_coded 
    Some_college_dummy_coded two_year_degree_dummy_coded male_dummy_coded age 
    social_norms_before_intentions question_order_by_social_norms_interaction.





