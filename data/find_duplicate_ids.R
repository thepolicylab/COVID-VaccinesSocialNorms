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

## ## Focus attentin only on those with caseids in the csv file
## dat4 <- dat3_thin %>% filter(some_match)
## dat4
## table(dat4$survey,exclude=c())
## dat4$some_match <- NULL ## by definition all TRUE
## with(dat4,table(orig_id_is_caseid_15,orig_id_is_caseid_16,exclude=c()))
## with(dat3_thin,table(orig_id_is_caseid_15,orig_id_is_caseid_16,exclude=c()))

## Make it easier to look at by collapsing the orig_id_is_caseid.. vars into one
## column.
dat3_thin$which_caseid <- with(dat3_thin,
    case_when(orig_id_is_caseid_16~16,
        orig_id_is_caseid_15~15))

## No surprise that the caseids match the surveys
with(dat3_thin,table(survey,which_caseid,exclude=c()))

## We want to know the different caseids represented within each pair.
### So first add the caseids where orig_id is the same as caseid_16
dat5 <- left_join(dat3_thin %>% select(orig_id,bm,survey,which_caseid,some_match),
    dec_dat1,by=c("orig_id"="caseid_16")) %>%
    arrange(bm) %>%
    mutate(caseid_16_csv=ifelse(which_caseid==16,orig_id,NA), ## to be clear, on dat5, orig_id and caseid_16 are the same
        caseid_16=ifelse(which_caseid==16,orig_id,NA))

dat5 %>% filter(orig_id==1297706273)
dec_dat1 %>% filter(caseid_16==1297706273)

dat5
dat5 %>% filter(which_caseid==15)
dat5 %>% filter(which_caseid==16)

## Then add the caseids for which orig_id is caseid_15
dat6 <- left_join(dat5,dec_dat1,by=c("orig_id"="caseid_15"),suffix = c("_1","_2")) %>%
    arrange(bm) %>%
    mutate(caseid_15_csv=ifelse(which_caseid==15,orig_id,NA), ## to be clear, on dat6, orig_id and caseid_15 are the same
        caseid_15=ifelse(which_caseid==15 & is.na(caseid_15),orig_id,caseid_15),
    caseid_16=ifelse(which_caseid==15,caseid_16_2,caseid_16_1),
    caseid_13=ifelse(which_caseid==15,caseid_13_2,caseid_13_1),
    caseid_14=ifelse(which_caseid==15,caseid_14_2,caseid_14_1),
    )

## make sure we didn't drop anyone used in the matching
stopifnot(all.equal(nrow(dat6),nrow(dat3_thin)))
## Also, make sure we didn't change anything about the matched set orig_ids --- just adding
## columns
all.equal(dat6$orig_id,dat3_thin$orig_id)
## Make sure we didn't break any pairs
stopifnot(unique(table(dat6$bm))==2)

dat6
## orig_id 1264497677  is from November (caseid_15), and has the
## id of 1297595061 for december (caseid_16)
## which_caseid==15.
dat6 %>% filter(orig_id==1264497677) %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)
dec_dat1 %>% filter(caseid_15==1264497677)
dat6 %>% filter(bm==1) %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)

## This person was repeated in the main data:
dat0 %>% filter(caseid %in% c(1264497677,1297595061)) %>%
    select(caseid,caseid_14,survey,birthyr,gender,race,pid3)

## orig_id  1297706273 is from December (caseid_16), and has the
## id of 1264877677 for november (caseid_15)
dat6 %>% filter(orig_id==1297706273) %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)
dec_dat1 %>% filter(caseid_16==1297706273)
dat6 %>% filter(bm==358) %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)

## This person was also repeated in the main data:
dat0 %>% filter(caseid %in% c(1297706273,1264877677)) %>%
    select(caseid,caseid_14,survey,birthyr,gender,race,pid3)


dat6 %>% filter(which_caseid==15)  %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)
dat6 %>% filter(which_caseid==16)  %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)

dat7 <- dat6 %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)

## Person orig_id=1264521923 had that same id in november, but in december had
## id 1298218639
dat6 %>% filter(orig_id==1264521923) %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)
dec_dat1 %>% filter(caseid_16==1298218639)
dat6 %>% filter(bm==2) %>% select(orig_id,bm,survey,which_caseid,caseid_15,caseid_16,caseid_13,caseid_14)

## This person was also repeated in the main data:
dat0 %>% filter(caseid %in% c(1264521923,1298218639)) %>%
    select(caseid,caseid_14,survey,birthyr,gender,race,pid3)

dat7

## Testing whether I can really access different entries within a set using the
## [1] and [2] indexing
testdat <- dat7 %>% group_by(bm) %>% summarize(test=caseid_15[1]-caseid_15[2]) %>% filter(!is.na(test))
test2 <- filter(dat7,bm==9) %>% summarize(test2=caseid_15[orig_id==1264629649] - caseid_15[orig_id==1265767537])
test1 <- testdat %>% filter(bm==9) %>% select(test)
stopifnot(test2$test2==test1$test)

## Do we ever see a pair where both members are the same person
### Is orig_id the same as caseid_15 when survey is 16? or same as caseid_16
### when survey is 15? (no)
within_person_match <- dat7 %>% group_by(bm) %>% filter(caseid_15[1]==caseid_16[2] | caseid_15[2]==caseid_16[1])
stopifnot(nrow(within_person_match)==0)

## Did we ever repeat the same person but across different pairs?
## Example: orig_id=1264497677 (which is the Novermber, survey=5, or caseid_15) is
## associated with caseid_16=1297595061 (december). If 1297595061 shows up as an
## orig_id for survey!=5, then we know a person has been repeated.


## Notice the in general orig_id==caseid_15 for the first survey and
## orig_id==caseid_16 for the december survey
with(dat7 %>% filter(survey==5 & !is.na(caseid_15)), table(orig_id==caseid_15,exclude=c()))
with(dat7 %>% filter(survey==8 & !is.na(caseid_16)), table(orig_id==caseid_16,exclude=c()))

## And neither are used for survey=7
with(dat7,table(survey,is.na(caseid_15)))
with(dat7,table(survey,is.na(caseid_16)))

## The question is whether orig_id is caseid_16 in survey=5 or caseid_15 in
## survey=8
with(dat7 %>% filter(survey==5 & !is.na(caseid_16)), orig_id %in% caseid_16)
with(dat7 %>% filter(survey==5 & !is.na(caseid_16)), intersect(orig_id,caseid_16))

with(dat7 %>% filter(survey==8 & !is.na(caseid_15)), orig_id %in% caseid_15)
with(dat7 %>% filter(survey==8 & !is.na(caseid_15)), intersect(orig_id,caseid_15))
