# read_ipums_data
# 2022.11.08
# DVK

################################################################################
# file setup - for now, include this at the top of all scripts, eventually can 
# split into separate maintenance file and only run once

library(plyr)
library(dplyr)

# package installer - will install packages you haven't already installed
need <- c('tidyverse','ipumsr','ggplot2','broom','lubridate','readxl','glue', 'tigris')
have <- need %in% rownames(installed.packages())
if(any(!have)) install.packages(need[!have])
invisible(lapply(need,library,character.only=T))
# ugly solution for the moment, but here() keeps going to wrong place
#detach("package:here", unload=TRUE)

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
# create dataset to merge onto
# bc ACS reports fips for state and county separately, create 2 columns:
# full fips (merge to CMS data), county fips (no leading zeros)
# fullfips <- seq(37001,37199,2)
# statefips <- seq(1,199,2)
# fipsdf_noyear <- data.frame(fullfips,statefips) %>% uncount(7)
# 
# years <- 2014:2020
# yeardf <- data.frame(year= rep(years, times = 100))
# 
# fipsdf <- data.frame(fipsdf_noyear, yeardf)

#-------------------------------------------------------------------------------
# read in ACS, merge to fipsdf
ddi <- read_ipums_ddi(here(rawdata,"ACS/usa_00002.xml"))
data_acs <- read_ipums_micro(ddi)
# coerce to puma xwalk format:
data_acs$PUMA <- sprintf("%05d",data$PUMA)

# puma-county crosswalk from 2010
pumas_nc <- pumas(state = 37)
#counties_nc <- counties(state=37, year = 2010)
xw <- read_delim(here(rawdata,"ACS/2010_Census_Tract_to_2010_PUMA.txt"))
xw_nc <- xw[xw$STATEFP==37, c(1,2,4)] %>% distinct() # don't need census tracts


# guide to collapsing PUMS data:
# https://walker-data.com/census-r/analyzing-census-microdata.html

# vars to look at: inc (per), race (per)., sex (per), educ (per), age
# private insurance, emp stat. everything by person, but need to turn into indicators
# for: race, sex, ins, emp.
acs_puma <- data_acs %>%
  group_by(PUMA, YEAR) %>%
  mutate(ind_non_white = RACE != 1,
         ind_female = SEX == 2,
         ind_private_hc = HCOVPRIV == 2,
         ind_unempl = EMPSTAT == 2,
         ind_highschool = EDUC >= 6) %>%
  dplyr::summarise(mean_inc = weighted.mean(INCTOT, w=PERWT),
                   mean_edu = weighted.mean(EDUC, w=PERWT),
                   shr_finish_hs = sum(ind_highschool*PERWT) / sum(PERWT),
                   mean_age = weighted.mean(AGE, w=PERWT),
                   shr_non_white = sum(ind_non_white*PERWT) / sum(PERWT),
                   shr_female = sum(ind_female*PERWT) / sum(PERWT),
                   shr_private_hc = sum(ind_private_hc*PERWT) / sum(PERWT),
                   shr_unempl = sum(ind_unempl*PERWT) / sum(PERWT),
                   num_outside = sum((1-ind_private_hc)*PERWT)) %>%
  # add lagged outside num
  mutate(lagged_outside = lag(num_outside))

acs <- xw_nc %>%
  left_join(acs_puma, by=c("PUMA5CE" = "PUMA"))

# TODO
# coerce to same format, merge
# find correct way to collapse
# fit back to puf data 
# FOR PUFS:
# look at diff between projected claims and claims
# see if entry predicted by size of gap

# sample shell dataset with all counties hopefully
# counties <- data %>% select(YEAR, COUNTYFIP, HHINCOME, AGE) %>%
#   group_by(YEAR, COUNTYFIP) %>% 
#   dplyr::summarize(mean_inc = mean(HHINCOME), mean_age = mean(AGE))
acs$fips = paste0(acs$STATEFP, acs$COUNTYFP)

save(acs, file=here(data,"ACS.Rda"))


