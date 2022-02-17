## Discover whether the respondents repeated between the October and December
## surveys were used either in the same pair or across more than one pair

library(tidyverse)
library(here)

## Some data from study 1: equivalent to survey==8 (December) in "MERGE_NR_2.3.21.csv"
## This data contains multiple caseids used for the same person in different
## studies.
dec_dat <- read_csv(here("data","BROW0016_OUTPUT_20220207.csv"))
nrow(dec_dat)
dec_dat %>% select(contains("caseid"))
## caseid_15 is caseid in november
## caseid is caseid in december


## Load original data that we used in the matching
dat0 <- read.csv(here("data", "MERGE_NR_2.3.21.csv"))
## We only use surveys 5,6,7,8
dat1 <- dat0 %>%
  filter(survey %in% c(5, 6, 7, 8)) %>%
  filter(!is.na(caseid)) %>% ## get rid of non-YouGov respondents
  droplevels()

## Verify that each row is a person with a unique yougov caseid
with(dat1,stopifnot(all.equal(length(unique(caseid)),nrow(dat1))))
## ## If you have a value for caseid in dat1, it should be unique and it is.
## table(table(dat1$caseid,exclude=c()))
## table(dat1$survey,exclude=c())
## Checking that dec_dat$caseid is the same as dat1$caseid for survey 8
stopifnot(all.equal(sort(dec_dat$caseid),sort(dat1$caseid[dat1$survey==8])))

## Load the data post-matching (dat3) to get the pair indicators:
## This loads other stuff, but we don't need it.
load(file = here("data/calculated", "outcome_analysis_study1.rda"),verbose=TRUE)

# orig_id from the matched design is either caseid_15 or caseid_16 from dec_dat
## To be clear, call the december id caseid_16
## Also make two data frames that are skinny and easy to look at
dec_dat1 <- dec_dat %>% select(contains("caseid")) %>% mutate(caseid_16=caseid)
dat3_thin <- dat3 %>% select(orig_id,bm,survey)

## Verify that all matches were within survey:
same_survey_dat_verify <- dat3_thin %>% group_by(bm) %>% summarize(same_survey=length(unique(survey)))
stopifnot(unique(same_survey_dat_verify$same_survey)==1)

## orig_id on the MERGE... data is caseid in december or november
## caseid_16 is caseid in december
## caseid_15 is caseid in november
## Which caseid(s) correspond to each orig_id?

dat3_thin$orig_id_is_caseid_16 <- dat3_thin$orig_id %in% dec_dat1$caseid_16
dat3_thin$orig_id_is_caseid_15 <- dat3_thin$orig_id %in% dec_dat1$caseid_15
dat3_thin$orig_id_is_caseid_14 <- dat3_thin$orig_id %in% dec_dat1$caseid_14
dat3_thin$orig_id_is_caseid_13 <- dat3_thin$orig_id %in% dec_dat1$caseid_13

dat3_thin

## We have 176 cases where the orig_id is the same as the caseid_16 (december)
## We have 78 cases where the orig_id is the same as the caseid_15 (november)
## no other matches to caseid_13 or caseid_14 so remove those columns
summary(dat3_thin)

dat3_thin$orig_id_is_caseid_14 <- NULL
dat3_thin$orig_id_is_caseid_13 <- NULL

## How often to we have an orig_id which doesn't not match any id in the csv
## file? Answer: 636 people
dat3_thin <- dat3_thin %>%  mutate(some_match=orig_id_is_caseid_16 | orig_id_is_caseid_15)
## Also we see no cases where orig_id is the same as *both* caseid_16 and
## caseid_15
with(dat3_thin,table(interaction(orig_id_is_caseid_16,orig_id_is_caseid_15),some_match,exclude=c()))

## Just verify the finding that we have a lot of orig_ids that are not in the
## csv file
set.seed(12345)
## Do a by hand search for ids that appear not to be in the csv file at all
some_missing_ids <- dat3_thin %>% filter(!some_match) %>% sample_n(10)
## Create code for use with grep:
paste(some_missing_ids$orig_id,collapse="|")
## Doesn't find anything when I go to the unix command line and do:
## grep -E "1269241395|1269724511|1269205219|1269127311|1268933511|1269197021|1268582405|1269298847|1269062805|1269356041" BROW0016_OUTPUT_20220207.csv
## Finds stuff when I do:
## grep -E "1264497677|1264521923|1265277649|1264521185|1264574005|1264543533|1264629649|1265767537|1264504787|1265260767" BROW0016_OUTPUT_20220207.csv
## So I'm satisfied that we have 636 people who were not a part of the csv file
## we received (from december, these people would be a part of other surveys)
table(dat3_thin$some_match,dat3_thin$survey,exclude=c())

## We never have orig_id matching **both** caseid_15 and caseid_16
with(dat3_thin,table(orig_id_is_caseid_15,orig_id_is_caseid_16,exclude=c()))
stopifnot(with(dat3_thin,any(orig_id_is_caseid_15!=orig_id_is_caseid_16)))

## Make it easier to look at by collapsing the orig_id_is_caseid.. vars into one
## column recording where the orig_id matched.
dat3_thin$which_caseid <- with(dat3_thin,
    case_when(orig_id_is_caseid_16~16,
        orig_id_is_caseid_15~15))

## No surprise that the caseids match the surveys
with(dat3_thin,table(survey,which_caseid,exclude=c()))

## Remove those columns just to make it easier to look at
dat3_thin$orig_id_is_caseid_15 <- NULL
dat3_thin$orig_id_is_caseid_16 <- NULL

## add a column to dat3_thin recording the caseid that the orig_id matched
dat3_thin$caseid_nov <- ifelse(dat3_thin$orig_id %in% dec_dat1$caseid_15,dat3_thin$orig_id,NA)
dat3_thin$caseid_dec <- ifelse(dat3_thin$orig_id %in% dec_dat1$caseid_16,dat3_thin$orig_id,NA)

dat3_thin

## Now fill in the missing caseids:
## If caseid_nov not missing but we don't have a caseid_dec for that person,
## fill in caseid_dec with the caseid_16 from the csv file, otherwise don't change
## caseid_dec. (And same for caseid_nov when we don't have one recorded in the
## dat3_thin file which also contains the matched set indicators)

dat4 <- left_join(dat3_thin,
    dec_dat1 %>% select(caseid_15,caseid_16),
    by=c("orig_id"="caseid_16")) %>%
    arrange(bm)  %>%
    mutate(caseid_16=ifelse(is.na(caseid_nov),orig_id,caseid_dec),
        caseid_nov=ifelse(is.na(caseid_nov),caseid_15,caseid_nov))

dat4
dat4 %>% filter(which_caseid==15)
dat4 %>% filter(which_caseid==16)

dec_dat1 %>% filter(caseid_16==1298731269)
dat4 %>% filter(caseid_dec==1298731269)
dec_dat1 %>% filter(caseid_15==1264497677)
dat4 %>% filter(orig_id==1264497677)

dat5 <- left_join(dat4,
    dec_dat1 %>% select(caseid_15,caseid_16),
    by=c("orig_id"="caseid_15"),
    suffix=c("_dat4","_csv")) %>%
    arrange(bm)  %>%
    mutate(caseid_15=ifelse(is.na(caseid_dec)&is.na(caseid_15),orig_id,caseid_nov),
    caseid_nov=ifelse(is.na(caseid_nov),caseid_15,caseid_nov),
    caseid_dec=ifelse(is.na(caseid_dec),caseid_16_csv,caseid_dec))

dat5
dat5 %>% filter(which_caseid==15)
dat5 %>% filter(which_caseid==16)

## Before replacing caseid_dec when missing with caseid_16_csv, check to make
## sure that whenever caseid_16_csv is missing, caseid_16_dat4 is not missing.
stopifnot(dat5 %>% filter(which_caseid==16) %>%  with(.,any(is.na(caseid_16_csv)&!is.na(caseid_16_dat4))))

dat5 <- dat5 %>% mutate() %>% select(-c(caseid_16_csv,caseid_16_dat4,caseid_15))

dat5
dat5 %>% filter(which_caseid==15)
dat5 %>% filter(which_caseid==16)

### Check things: First just look at two cases.
## orig_id 1264497677  is from November (caseid_15), and has the
## id of 1297595061 for december (caseid_16)
## which_caseid==15.
dat5 %>% filter(orig_id==1264497677)
dec_dat1 %>% filter(caseid_15==1264497677)
dat5 %>% filter(bm==1)


## orig_id  1297706273 is from December (caseid_16), and has the
## id of 1264877677 for november (caseid_15)
dat5 %>% filter(orig_id==1297706273)
dec_dat1 %>% filter(caseid_16==1297706273)
dat5 %>% filter(bm==358)
stopifnot(all.equal(dec_dat1 %>% filter(caseid_16==1297706273) %>% select(caseid_15,caseid_16),
        dat5 %>% filter(caseid_dec==1297706273) %>% select(caseid_nov,caseid_dec),
        check.attributes=FALSE))

## Everybody has at least one caseid_nov and one caseid_dec
stopifnot( all( apply(dat5[,c("caseid_nov","caseid_dec"),],1,function(x){ any(!is.na(x)) }) ) )

test_dat5_dat <- dat5 %>% filter(some_match & !is.na(caseid_nov)) %>% sample_n(10) %>% 
    select(caseid_nov,caseid_dec) %>%
    arrange(caseid_nov)

## I checked the above by hand. Now using code. Ignore names
## not using a seed because this should have worked regardless of who is chosen
test_csv_dat <- dec_dat1 %>% filter(caseid_15 %in% test_dat5_dat$caseid_nov[!is.na(test_dat5_dat$caseid_nov)]  |
    caseid_16 %in% test_dat5_dat$caseid_dec) %>% select(caseid_15,caseid_16) %>% arrange(caseid_15)
test_csv_dat

stopifnot(all.equal(test_csv_dat,test_dat5_dat,
        check.attributes=FALSE))

#### Ok. The merging worked. Now, we are going to compare people within pairs.

## First, Testing whether I can really access different entries within a set using the
## [1] and [2] indexing
testdat <- dat5 %>% group_by(bm) %>% summarize(test=caseid_nov[1]-caseid_nov[2]) %>% filter(!is.na(test))
test2 <- dat5 %>% summarize(test2=caseid_nov[orig_id==1264629649] - caseid_nov[orig_id==1265767537])
test1 <- testdat %>% filter(bm==9) %>% select(test)
stopifnot(test2$test2==test1$test)

## Do we ever see a pair where both members are the same person
### Is orig_id the same as caseid_nov when survey is 16? or same as caseid_dec
### when survey is 15? (no) (same result when looking at whether orig_id of
### person 1 same as any id for person 2 and vice-versa)
within_person_match <- dat5 %>% group_by(bm) %>% filter(caseid_nov[1]==caseid_dec[2] | caseid_nov[2]==caseid_dec[1])
stopifnot(nrow(within_person_match)==0)
within_person_match_2 <- dat5 %>% group_by(bm) %>% filter(orig_id[1]==caseid_nov[2] | orig_id[2]==caseid_nov[1] |
    orig_id[1]==caseid_dec[2] | orig_id[2]==caseid_dec[1])
stopifnot(nrow(within_person_match_2)==0)


## Did we ever repeat the same person but **across** different pairs?
## Example: orig_id=1264497677 (which is the Novermber, survey=5, or caseid_nov) is
## associated with caseid_dec=1297595061 (december). If 1297595061 shows up as an
## orig_id for survey!=5, then we know a person has been repeated.


## Notice the in general orig_id==caseid_nov for the first survey and
## orig_id==caseid_dec for the december survey. Survey 7 is irrelevant here.
with(dat5 %>% filter(survey==5 & !is.na(caseid_nov)), table(orig_id==caseid_nov,exclude=c()))
with(dat5 %>% filter(survey==8 & !is.na(caseid_dec)), table(orig_id==caseid_dec,exclude=c()))
with(dat5 %>% filter(survey==7 & !is.na(caseid_nov)), table(orig_id==caseid_dec,exclude=c()))
with(dat5 %>% filter(survey==7 & !is.na(caseid_dec)), table(orig_id==caseid_dec,exclude=c()))

## Notice that we have 78 people in survey 5 who have orig_ids that match the
## csv and 176 people in survey 8 who have orig_ids that match the csv.
with(dat5, table(some_match,which_caseid,exclude=c()))

## And we have 141 people who have both a nov and a dec id.
table( apply(dat5[,c("caseid_nov","caseid_dec"),],1,function(x){ all(!is.na(x)) }))


## So here we have two people:
test_dat3 <- dat5 %>% filter(orig_id %in% c(1264497677, 1297706273))
## if either of them were placed into another pair, we'd see their orig_id match
## either caseid_nov or caseid_dec for the other pair (and other survey). Here
## we have one person from survey 5 and the other person from survey 8.

## Here we see that the orig_ids only match a different case for their own row.
## So, neither of these were repeated across pairs.
test_dat3[test_dat3$orig_id %in% dat5$caseid_nov,]
test_dat3[test_dat3$orig_id %in% dat5$caseid_dec,]


## So, looking **across** surveys for matches between orig_id and caseid_nov or
## caseid_dec.

## Any orig_ids match another caseid_dec in survey 5? no
with(dat5 %>% filter(survey==5 & !is.na(caseid_dec)), table(orig_id %in% caseid_dec,exclude=c()))
## Any orig_ids match another caseid_nov in survey 8? no
with(dat5 %>% filter(survey==8 & !is.na(caseid_nov)), table(orig_id %in% caseid_nov,exclude=c()))

## What we expect is that all of the orig_ids in survey 5 should match
## caseid_nov
with(dat5 %>% filter(survey==5 & !is.na(caseid_nov)), table(orig_id %in% caseid_nov,exclude=c()))
## What we expect is that all of the orig_ids in survey 8 should match
## caseid_dec
with(dat5 %>% filter(survey==8 & !is.na(caseid_dec)), table(orig_id %in% caseid_dec,exclude=c()))


