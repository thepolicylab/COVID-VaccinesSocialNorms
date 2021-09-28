## ABOUT SURVEY 5

1. The data set appended with `MERGEDWITHFOURTHWAVEFORMESSING` indicates truthfully that this includes data from Survey 4. Filter it out by removing data where sample=3 (for the record, sample=1 is the US population from March, 2021, and sample=2 is the RI population from the same period.

2. Key variables with uninterpretable names are:
  * q126 Do you plan to get vaccinated as soon as it's possible to do so? (only asked of people who said they have not already begun vaccination, q120=2)
  * q60_1 How many people in your network of friends and family do you believe will definitely get vaccinated for coronavirus
  * q60_2 How many people in the neigborhood where you live do you believe will definitely get vaccinated for coronavirus
  * q60_3 How many people in your city or town do you believe will definitely get vaccinated for coronavirus
  * q60_4 How many people in your state do you believe will definitely get vaccinated for coronavirus
  * q60_5 How many Democrats do you believe will definitely get vaccinated for coronavirus
  * q60_6 How many Republicans do you believe will definitely get vaccinated for coronavirus
  * q60_7 How many Independents do you believe will definitely get vaccinated for coronavirus
  * already_vaccinated_or_definitely_will_get_vaccinated A composite variable that gives people a 1 if either q120=1 (they got vaccinated) or q126=5 (they will definitely get vaccinated)

3. I believe the demographics all have interpretable names and should be the same as in Study 1.

4. Analyses we discussed are:

(a) Regressions with matching just like in Study 1 but with the addition of q60_5, q60_6, q60_7
NOTE: I don't totally understand why this doesn't have to be done separately for each partisan group...maybe we can discuss this further

(b) Test for statistically significant differences between coefficients for all pairwise correlations
Democrats only (pid3=1): already_vaccinated_or_definitely_will_get_vaccinated q60_1 q60_2 q60_3 q60_4 q60_5
Republicans only (pid3=2): already_vaccinated_or_definitely_will_get_vaccinated q60_1 q60_2 q60_3 q60_4 q60_6
Independents only (pid3=3): already_vaccinated_or_definitely_will_get_vaccinated q60_1 q60_2 q60_3 q60_4 q60_7
