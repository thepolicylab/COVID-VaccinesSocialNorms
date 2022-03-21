* Encoding: UTF-8.


/* To calculate various descriptive statistics for the populations in Study 1,
/* we need to associate the variables from the surveys with the set of respondents that
/* wound up in the matched designs.

/* 1. Open the file outcome_analysis_study1_first_data.sav
/* Note that the variable orig_id has been renamed to caseid to match the original survey data set

/* 2. Open the file MERGE_NR_2.3.21.sav


/* 3. Sort the cases by the index variable

DATASET ACTIVATE DataSet1.
SORT CASES BY caseid(A).
SAVE OUTFILE = "outcome_analysis_study1_first_data_sorted.sav".

DATASET ACTIVATE DataSet2.
SORT CASES BY caseid(A).
SAVE OUTFILE = "MERGE_NR_2.3.21_sorted.sav".


/* 4. Merge the two sets

DATASET ACTIVATE DataSet1.
MATCH FILES /FILE=*
  /FILE='DataSet2'
  /RENAME (q59_1 q1 educ faminc_new ideo5 q22 q100 q7 q60_1 q60_3 q60_2 q60_4 survey q29 = d0 d1 d2 
    d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13) 
  /BY caseid
  /DROP= d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13.
EXECUTE.


/* Bin family income into even intervals

RECODE faminc_new (1=1) (2=1) (3=2) (4=2) (5=3) (6=3) (7=4) (8=4) (9=5) (10=6) (11=7) (12=8) (13=8) 
    (14=8) (15=8) (16=8) (ELSE=SYSMIS) INTO family_incom_binned_evenly.
EXECUTE.

* Define Variable Properties.
*family_incom_binned_evenly.
VALUE LABELS family_incom_binned_evenly
  1.00 '<10,000–19,000'
  2.00 '20,000– 39,000'
  3.00 '40,000– 59,000'
  4.00 '60,000– 79,000'
  5.00 '80,000– 99,000'
  6.00 '100,000–119,000'
  7.00 '120,000–149,000'
  8.00 '>150,000  '.
EXECUTE.




/* 5. Select just the respondents that are in Study 1

USE ALL.
COMPUTE filter_$=(row_id > 0).
VARIABLE LABELS filter_$ 'row_id > 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


/* Review 1.1. 

/* (a) What's the party ID breakdown in the matched group?

FREQUENCIES VARIABLES=pid3 dem_rep_oth
  /ORDER=ANALYSIS.

/* (b) Are these unusual Republicans?

T-TEST GROUPS=dem_rep_oth(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=ideo5 agegood pew_churatd trust_in_science 
  /CRITERIA=CI(.95).

CROSSTABS
  /TABLES=female BY dem_rep_oth
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.

/* Review 2.5. Do intentions vary between the two groups?

FREQUENCIES VARIABLES=q1
  /ORDER=ANALYSIS.


/* General demographics

FREQUENCIES VARIABLES=gender race age_range educ family_incom_binned_evenly ideo5
  /ORDER=ANALYSIS.



/* Testing regression

USE ALL.
COMPUTE filter_$=(row_id > 0).
VARIABLE LABELS filter_$ 'row_id > 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT q30
  /METHOD=ENTER q25 ideo5 agegood faminc_new educ trust_in_science covid_know q51 relig_scale 
    party_dummy_coded_republican party_dummy_coded_independent male_dummy_coded race_dummy_coded_black 
    race_dummy_coded_latino.



