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

### This next verifies that the dec_dat are what we think they are. So
### commenting out for now.
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
## file?

dat3_thin <- dat3_thin %>%  mutate(some_match=orig_id_is_caseid_16 | orig_id_is_caseid_15)

with(dat3_thin,table(interaction(orig_id_is_caseid_16,orig_id_is_caseid_15),some_match,exclude=c()))

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
    mutate(caseid_16=ifelse(some_match,orig_id,NA)) ## to be clear, on dat5, orig_id and caseid_16 are the same

dat5
dat5 %>% filter(which_caseid==15)
dat5 %>% filter(which_caseid==16)

## Then add the caseids for which orig_id is caseid_15
dat6 <- left_join(dat5,dec_dat1,by=c("orig_id"="caseid_15"),suffix = c("_1","_2")) %>%
    arrange(bm) %>%
    mutate(caseid_15=ifelse(some_match,orig_id,NA)) ## to be clear, on dat5, orig_id and caseid_15 are the same

## make sure we didn't drop anyone used in the matching
stopifnot(all.equal(nrow(dat6),nrow(dat3_thin)))
## Also, didn't change anything about the matched set orig_ids --- just adding
## columns 
all.equal(dat6$orig_id,dat3_thin$orig_id)
## Make sure we didn't break any pairs
stopifnot(unique(table(dat6$bm))==2)

dat6
dat6 %>% filter(which_caseid==15)
dat6 %>% filter(which_caseid==16)

blah <- dat6 %>% group_by(bm) %>%
    summarize(orig_id[1]==caseid_15[2],
        orig_id[1]==caseid_16_2[2],
        orig_id[2]==caseid_15[1],
        orig_id[2]==caseid_16_2[1])


## make sure we didn't drop any one used in the matching
stopifnot(all.equal(nrow(dat4),nrow(dat3_thin)))
## Also, didn't change anything about the matched set orig_ids --- just adding
## columns from dec_dat1.
all.equal(dat4$orig_id,dat3_thin$orig_id)
## People for whom we have no caseid_15 on the csv file.
dat4 %>% filter(is.na(caseid_15))
## People for whom we do have a caseid 15 on the csv file
dat4 %>% filter(!is.na(caseid_15))

dat5 <- left_join(dat3_thin,dec_dat1,by=c("orig_id"="caseid_15")) %>%
    filter(!is.na(bm)) %>%
    arrange(bm) %>%
    mutate(caseid_15=orig_id) ## to be clear, on dat5, orig_id and caseid_16 are the same
## make sure we didn't drop any one used in the matching
stopifnot(all.equal(nrow(dat5),nrow(dat3_thin)))
## Also, didn't change anything about the matched set orig_ids --- just adding
## columns from dec_dat1.
all.equal(dat5$orig_id,dat3_thin$orig_id)
## People for whom we have no caseid_15 on the csv file.
dat5 %>% filter(is.na(caseid_16))
## People for whom we do have a caseid 15 on the csv file
dat5 %>% filter(!is.na(caseid_16))





dat5 <- left_join(dat4,dec_dat1,by=c("orig_id"="caseid_15"),suffix = c("_1","_2")) %>%
    filter(!is.na(bm)) %>%
    arrange(bm) %>%
    mutate(caseid_15=orig_id) ## to be clear, on dat5, orig_id and caseid_16 are the same
## make sure we didn't drop any one used in the matching
stopifnot(all.equal(nrow(dat5),nrow(dat3_thin)))
## Also, didn't change anything about the matched set orig_ids --- just adding
## columns from dec_dat1.
all.equal(dat5$orig_id,dat3_thin$orig_id)
## People for whom we have no caseid_15 on the csv file.
dat5 %>% filter(is.na(caseid_16_2))
## People for whom we do have a caseid 15 on the csv file
dat5 %>% filter(!is.na(caseid_16_2))


## Just checking: we have two and only two members of each pair? (yes)
stopifnot(unique(table(dat4$bm))==2)

## Do we ever see a pair where both members are the same person
### Is caseid the same as caseid_15 within set.

blah <- dec_dat2 %>% group_by(bm) %>%
  summarize(=orig_id[1]==caseid_15[2],
            two_to_one=orig_id[2]==caseid_15[1])


## Did we ever repeat the same person but across different pairs?






