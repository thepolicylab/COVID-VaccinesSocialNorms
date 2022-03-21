* Encoding: UTF-8.


/* To calculate various descriptive statistics for the populations in Study 2,
/* we need to associate the variables from the surveys with the set of respondents that
/* wound up in the matched designs.

/* 1. Open the file outcome_analysis_study2_first_data.sav

/* 2. Open the file TPL_Testing_Survey_FifthWave_YouGov_MERGEDWITHFOURTHWAVEFORMESSING.sav

/* 3. Sort the cases by the index variable (YouGov's unique identifier)

DATASET ACTIVATE DataSet1.
SORT CASES BY caseid_BROW0017(A).
SAVE OUTFILE = "outcome_analysis_study2_data_sorted.sav".

DATASET ACTIVATE DataSet2.
SORT CASES BY caseid_BROW0017(A).
SAVE OUTFILE = "TPL_Testing_Survey_FifthWave_YouGov_MERGEDWITHFOURTHWAVEFORMESSING_sorted.sav".


/* 4. Merge the two sets

DATASET ACTIVATE DataSet1.
MATCH FILES /FILE=*
  /FILE='DataSet2'
  /RENAME (age q59_1 caseid_BROW0013 caseid_BROW0014 caseid_BROW0015 caseid_BROW0016 caseid educ 
    q60_5 q60_7 q60_2 q60_3 q60_1 q60_4 q60_6 faminc_new q25_new ideo5 q22 sample = d0 d1 d2 d3 d4 d5 
    d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19) 
  /BY caseid_BROW0017
  /DROP= d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19.
EXECUTE.


/*  Create age range variable

RECODE age (18 thru 29=1) (30 thru 39=2) (40 thru 49=3) (50 thru 59=4) (60 thru 69=5) (70 thru Highest=6) INTO 
    age_range.
EXECUTE.

* Define Variable Properties.
*age_range.
VALUE LABELS age_range
  1.00 '18-29'
  2.00 '30-39'
  3.00 '40-49'
  4.00 '50-59'
  5.00 '60-69'
  6.00 '70+'.
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
  /VARIABLES=ideo5 trust_in_science trust_in_govt pew_churatd age
  /CRITERIA=CI(.95).

CROSSTABS
  /TABLES=female BY dem_rep_oth
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.

/* Review 2.5. Intentions in the two groups

FREQUENCIES VARIABLES=q126
  /ORDER=ANALYSIS.


/* What's the mean intention in this group?

USE ALL.
COMPUTE filter_$=(row_id > 0 & q126 > 0).
VARIABLE LABELS filter_$ 'row_id > 0 & q126 > 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

MEANS TABLES=q126
  /CELLS=MEAN COUNT STDDEV.


/* General demographics

USE ALL.
COMPUTE filter_$=(row_id > 0).
VARIABLE LABELS filter_$ 'row_id > 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=gender_client race age_range educ family_incom_binned_evenly ideo5
  /ORDER=ANALYSIS.


/* Mask regression

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
  /DEPENDENT q7
  /METHOD=ENTER q100 ideo5 age faminc_new educ trust_in_govt trust_in_science covid_subj_know 
    relig_scale gender_dummy_coded_female party_dummy_coded_republican party_dummy_coded_independent 
    race_dummy_coded_black race_dummy_coded_latino.
