# explore_cleaned_data
# 2022.11.07
# DVK

################################################################################
# file setup - for now, include this at the top of all scripts, eventually can 
# split into separate maintenance file and only run once

# package installer - will install packages you haven't already installed
need <- c('here','tidyverse','ggplot2','broom','lubridate','readxl','glue','dplyr','plyr')
have <- need %in% rownames(installed.packages())
if(any(!have)) install.packages(need[!have])
invisible(lapply(need,library,character.only=T))
# ugly solution for the moment, but here() keeps going to wrong place
detach("package:here", unload=TRUE)

# change path to root directory:
### OVERALL DIRECTORY STRUCTURE: 
# [folder for research project]
## data, raw_data, scripts, tex
### raw_data --> structure in zip file shared by Mengyang
#### if you set things up as i did above, this should run on your machine with no changes
script_folder = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(glue('{script_folder}'))
setwd('..')
rm(list = ls())
options(scipen = 999)
library(here)

# subfolders that we wanna use eventually
rawdata <- here('raw_data')
scripts <- here('scripts')
data <- here('data')

#-------------------------------------------------------------------------------
# service area exploration
load(here(data,"service_areas.Rda"))
load(here(data,"plan_attributes.Rda"))
load(here(data,"enrollment.Rda"))
load(here(data,"networks.Rda"))
# for now, focus on PUF 2, which has actual data instead of projected
load(here(data, "puf_2.Rda"))
load(here(data,"ACS.Rda"))
#load(here(data,"benefits.Rda"))

# this is useful, grab components to merge to other datasets from here:
## grab deduped list of issuers
issuers <- puf_2 %>% select(ISSUER_ID, COMPANY) %>% 
  group_by(ISSUER_ID, COMPANY) %>% distinct() %>%
  # multiple versions of company name per ID, so keep first row
  group_by(ISSUER_ID) %>% slice_head(n=1) %>% arrange(ISSUER_ID)

## deduped list of plan names 
plans <- puf_2 %>% select(PLAN_ID, PROD_NAME) %>% 
  group_by(PLAN_ID, PROD_NAME) %>% distinct() %>%
  # multiple versions of company name per ID, so keep first row
  group_by(PLAN_ID) %>% slice_head(n=1) %>% arrange(PLAN_ID)
# still very large, unclear if useful

## usage data
# drop where state missing
usage <- puf_2[puf_2$EXCHANGE=="Yes"&puf_2$MARKET=="Individual", ] %>% drop_na(STATE)
usage_bronze <- usage[usage$METAL=="Bronze",]

#-------------------------------------------------------------------------------
# start with enrollment dataset, merge on relevant info
# company name
enroll_comp <- enroll_nc %>% mutate(issuer_hios_id = as.double(issuer_hios_id)) %>%
  left_join(issuers, by = c("issuer_hios_id" = "ISSUER_ID"))
# plan name
enroll_pl <- enroll_comp %>% 
  left_join(plans, by = c("selected_insurance_plan" = "PLAN_ID"))

## merging on plan attributes, clean plan_attrib df so merge is clean: 
# merge keys: standard_component_id to selected_insurance_plan
plan_attrib_light <- plan_attributes_nc[plan_attributes_nc$MarketCoverage == "Individual",] %>%
  select(BusinessYear, MarketCoverage, DentalOnlyPlan, StandardComponentId, 
         PlanMarketingName, HIOSProductId, NetworkId, ServiceAreaId, 
         IsNewPlan, PlanType, MetalLevel) %>%
  distinct() %>% arrange(StandardComponentId)

# test if merge key is unique
uniques <- plan_attrib_light %>%
  group_by(StandardComponentId, BusinessYear) %>%
  dplyr::summarize(n=n())
max(uniques$n) # all set

enroll_attrib <- enroll_pl %>%
  left_join(plan_attrib_light, by = 
              c("selected_insurance_plan" = "StandardComponentId", 
                "year" = "BusinessYear"))

# add service areas. merge on Year, IssuerID, ServiceAreaId
# clean service areas. for now, don't keep individual county codes
areas <- service_area_nc %>% 
  select(BusinessYear, IssuerId, ServiceAreaId, ServiceAreaName, CoverEntireState,
         County) %>% distinct() %>% 
  group_by(BusinessYear, IssuerId, ServiceAreaId, ServiceAreaName, CoverEntireState) %>%
  dplyr::summarize(num_counties=n())

# add the number of counties for the moment
# TODO: Calculate number of options by county excluding dental plans
      # Consider using the enrollment data to cut out plans that aren't "real"
      # i.e., dental, whatever's going on with the State of NC plans
enroll_areas <- enroll_attrib %>%
  left_join(areas, by = c("year" = "BusinessYear", "issuer_hios_id" = "IssuerId", 
                      "ServiceAreaId" = "ServiceAreaId"))

# add network data
networks <- network_nc %>% 
  select(BusinessYear, IssuerId, NetworkName, NetworkId) %>% 
  distinct() 

# "final" dataset for the moment
enroll <- enroll_areas %>% 
  left_join(networks, by = c("year" = "BusinessYear", 
                             "issuer_hios_id" = "IssuerId", 
                             "NetworkId" = "NetworkId"))
#-------------------------------------------------------------------------------
# enrollment at county-plan level
load(here(data,"enrollment_issuer.Rda"))

# calculate market size excluding outside option: 
marketsize <- enroll_nc_issuer %>%
  drop_na(plcy_county_fips_code) %>% na_if("*") %>%
  mutate(ever_enrolled_plan_sel = replace_na(ever_enrolled_plan_sel,"0")) %>%
  group_by(plcy_county_fips_code, year) %>%
  dplyr::summarise(tot_enroll = sum(as.double(ever_enrolled_plan_sel))) %>%
  mutate(lag_tot_enroll = lag(tot_enroll))

# process service areas dataset to merge with county coverage dataset
# issuerId version
# subset to plans that exist in the puf
areas_num <- issuers %>%
  left_join(service_area_nc, by = c("ISSUER_ID" = "IssuerId"))

areas_wide <- areas_num %>%
  select(BusinessYear, ISSUER_ID, #ServiceAreaId, CoverEntireState,
         County) %>%
  mutate(flag=1, County = replace_na(as.double(County), 37000)) %>%
  group_by(BusinessYear, County) %>%
  distinct() %>%
  dplyr::mutate(issuer_num = cumsum(flag), num_issuers = n()) %>%
  pivot_wider(names_from = issuer_num, names_prefix = "issuer_", values_from = ISSUER_ID)

# merge on ACS data
issuer_acs <- areas_wide %>%
  mutate(County = as.character(County),
         multiple_issuers = num_issuers > 1) %>%
  left_join(acs, by = c("BusinessYear" = "YEAR", "County" = "fips")) %>%
  drop_na(PUMA5CE)

# merge issuer_acs 
acs_cms <- enroll_nc_issuer %>% drop_na(plcy_county_fips_code) %>%
  left_join(issuer_acs, 
            by = c("year" = "BusinessYear", 
                   "plcy_county_fips_code" = "County")) %>%
  left_join(marketsize, by = c("year","plcy_county_fips_code" )) %>%
  mutate(s0 = (tot_enroll / (tot_enroll+num_outside))) %>%
  group_by(plcy_county_fips_code, year) %>% mutate(lag_s0 = lag(s0)) %>%
  # drop fips code starting with 42 
  subset(plcy_county_fips_code != "42101")

#-------------------------------------------------------------------------------
# enrollment at county-plan level

simple_reg <- lm(num_issuers ~ mean_inc + mean_age + mean_edu + 
                   shr_non_white + shr_female + shr_private_hc + shr_unempl, data=acs_cms)
# to add: population, anything else we can grab from ACS data
# lagged num of issuers in t-1
# can set up transition matrices
summary(simple_reg)

shr_hs_reg <- lm(num_issuers ~ mean_inc + mean_age + shr_finish_hs + 
                   shr_non_white + shr_female + shr_private_hc + shr_unempl, data=acs_cms)
# to add: population, anything else we can grab from ACS data
# lagged num of issuers in t-1
# can set up transition matrices
summary(shr_hs_reg)

# adding nonlinear income effects
nonlinear <- lm(num_issuers ~ poly(mean_inc,2) + mean_age + mean_edu + 
                  shr_non_white + shr_female + shr_private_hc + shr_unempl, data=acs_cms)
summary(nonlinear)

allow_reg <- lm(num_issuers ~ mean_age + shr_female, data=issuer_acs)
summary(allow_reg)

### logit for multiple issuers. BCBS always issuer 1 in analysis df anyway
### need to think about how to regress on proportions
logit_simple <- glm(multiple_issuers ~ mean_inc + mean_age + mean_edu + 
                      shr_non_white + shr_female + shr_private_hc + shr_unempl, 
                    data=acs_cms, family="binomial")
summary(logit_simple)

logit_nonlininc <- glm(multiple_issuers ~ poly(mean_inc,2) + mean_age + 
                         mean_edu + shr_non_white + shr_female + 
                         shr_private_hc + shr_unempl, 
                       data=acs_cms,family="binomial")
summary(logit_nonlininc)

# last year's outside good share as predictor
logit_s0 <- glm(multiple_issuers ~ mean_inc + mean_age + 
                  shr_non_white + shr_female + 
                  shr_private_hc + shr_unempl + lag_s0 + factor(year), 
                data=acs_cms[acs_cms$year>=2015,],family="binomial")
summary(logit_s0)


# more descriptive stuff
table(issuer_acs$BusinessYear, issuer_acs$num_issuers)

#-------------------------------------------------------------------------------
# by issuer: 
issuer_regs <- 




















#-------------------------------------------------------------------------------

####
# maybe need crosswalk first? ended up being not useful
load(here(data, "plan_crosswalk.Rda"))
# subset to bronze plans. seem to be duplicates, so dedupe, unclear why. 
# i think everyone needs to offer a bronze plan? so this is fine? could be wrong
bronze_xw <- xwalk_nc[xwalk_nc$MetalLevel_year=="Bronze",] %>% distinct()
area_xw <- service_area_nc %>%
  left_join(bronze_xw, by = c("BusinessYear" = "year", "IssuerId" = "IssuerID_year", "County" = "FIPSCode"))
bronze_xw %>% select(ReasonForCrosswalk,year) %>% 
  group_by(ReasonForCrosswalk,year) %>% 
  summarize(count = n_distinct(ReasonForCrosswalk))
# doesn't seem like this is useful beyond fairly rare instances
####

areas <- service_area_nc %>% 
  mutate(County = replace_na(as.double(County), 1))

## further cleaning of data
# get plan names
plans <- areas %>%
  select(ServiceAreaName) %>%
  group_by(ServiceAreaName) %>% summarise(n_distinct(ServiceAreaName))


# num counties with 2+ insurers by year
county_coverage <- areas %>% 
  select(BusinessYear, IssuerId, County) %>% 
  group_by(BusinessYear) %>%
  summarise(count=n_distinct(County))
# not what i expected, return to this

insurers_per_county <- areas %>%
  select(BusinessYear, IssuerId, County) %>%
  group_by(BusinessYear, County) %>%
  summarise(count=n_distinct(IssuerId))

