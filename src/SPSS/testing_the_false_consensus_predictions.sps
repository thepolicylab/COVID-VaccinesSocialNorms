* Encoding: UTF-8.




/* 1. Are the people who say 1 (Definitely not) or 5 (Definitely will) on the vaccine intention question more likely to get their state-level estimates wrong?
/* Study 2 only (TPL_Testing_Survey_FifthWave_YouGov_MERGEDWITHFOURTHWAVEFORMESSING.sav)

/* Create a new variable capturing whether people are extremely for (5) or against (1) vaccination

RECODE q126 (1=1) (5=1) (ELSE=0) INTO vaccination_very_pro_and_very_con_or_not.
EXECUTE.

/* Select just the people who had not yet been vaccinated

USE ALL.
COMPUTE filter_$=(sample < 3 & q120 ~= 1).
VARIABLE LABELS filter_$ 'sample < 3 & q120 ~= 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

/* Chi-square to see if the people who are extremely for (5) or against (1) are more likely to be wrong in their estimates of others' vaccination intentions

CROSSTABS
  /TABLES=vaccination_very_pro_and_very_con_or_not BY state_estimate_accurate
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT ROW 
  /COUNT ROUND CELL.


/* 2. Among the most likely to vaccinate against COVID-19, do people who have been vaccinated against seasonal flu give higher ratings than people who have not?

/* Study 1 only (MERGE_NR_2.3.21.sav)
/* Created vacciated against flu dummy code

RECODE q61 (1=1) (SYSMIS=SYSMIS) (ELSE=0) INTO vaccinated_for_flu_dummy_coded.
EXECUTE.

/* Compare means

USE ALL.
COMPUTE filter_$=(survey > 4 & q1 = 5).
VARIABLE LABELS filter_$ 'survey > 4 & q1 = 5 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST GROUPS=vaccinated_for_flu_dummy_coded(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=q60_1 q60_2 q60_3 q60_4
  /CRITERIA=CI(.95).
