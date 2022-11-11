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

# some hypotheses to test:
# are statewide plans 'worse'?
#-------------------------------------------------------------------------------
# county coverage dataset
load(here(data,"ACS.Rda"))

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
  left_join(acs_shell, by = c("BusinessYear" = "year", "County" = "fullfips"))

lm(num_issuers ~ mean_inc + mean_age, data=issuer_acs)
# to add: population, anything else we can grab from ACS data
# lagged num of issuers in t-1
# can set up transition matrices

sorted_acs_iss <- issuer_acs %>%
  arrange(County, BusinessYear)

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

