# 850_rp_clean_explore_data
# 2022.10.14
# DVK

################################################################################
# file setup - for now, include this at the top of all scripts, eventually can 
# split into separate maintenance file and only run once

# package installer - will install packages you haven't already installed
need <- c('tidyverse','ggplot2','broom','lubridate','dplyr','plyr','readxl','glue', 'here')
have <- need %in% rownames(installed.packages())
if(any(!have)) install.packages(need[!have])
invisible(lapply(need,library,character.only=T))

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

# subfolders that we wanna use eventually
rawdata <- here('raw_data')
scripts <- here('scripts')
data <- here('data')

################################################################################
# generic utility things
years = list(2014:2020)

# rate PUFs
### just using 2014-2020 data for now/availability. seems like these PUFs are different
# 2021
# puf2021 <- read_csv(here(rawdata,'CMS Health Insurance Exchange/Rate/2021 Rate_PUF.csv'))
# # large
# puf2021nc <- puf2021[puf2021$StateCode=="NC",]
# head(puf2021nc)
# length(unique(puf2021nc$PlanId))
# # 227 unique plans
# length(unique(puf2021nc$RatingAreaId))
# # these are rating areas specifically, so not linked to a specific exchange? 
# unique(puf2021nc$RatingAreaId)
# unique(puf2021nc$IssuerId)
# 
# # 2022
# puf2022 <- read_csv(here(rawdata,'CMS Health Insurance Exchange/Rate/2022 Rate_PUF.csv'))
# puf2022nc <- puf2022[puf2022$StateCode=="NC",]
# length(unique(puf2022nc$PlanId))
# # 379 unique plans
# length(unique(puf2022nc$RatingAreaId))

### subset and stack 

# 2014
puf2014a <- read_csv(here(rawdata,'CMS Rate Review Data/2014 PUF/WKSH1_PUF_2014_20161103.csv'))
puf2014b <- read_csv(here(rawdata,'CMS Rate Review Data/2014 PUF/WKSH2_PUF_2014_20161103.csv'))
puf2014anc <- puf2014a[puf2014a$STATE == "NC", ]
puf2014bnc <- puf2014b[puf2014b$STATE == "NC", ]

# 2015
puf2015a <- read_csv(here(rawdata,'CMS Rate Review Data/2015 PUF/WKSH1_PUF_2015_20161103.csv'))
puf2015b <- read_csv(here(rawdata,'CMS Rate Review Data/2015 PUF/WKSH2_PUF_2015_20161103.csv'))
puf2015anc <- puf2015a[puf2015a$STATE == "NC", ]
puf2015bnc <- puf2015b[puf2015b$STATE == "NC", ]

# 2016
puf2016a <- read_csv(here(rawdata,'CMS Rate Review Data/2016 PUF/WKSH1_PUF_2016_20161103.csv'))
puf2016b <- read_csv(here(rawdata,'CMS Rate Review Data/2016 PUF/WKSH2_PUF_2016_20161103.csv'))
puf2016anc <- puf2016a[puf2016a$STATE == "NC", ]
puf2016bnc <- puf2016b[puf2016b$STATE == "NC", ]

# 2017
puf2017a <- read_csv(here(rawdata,'CMS Rate Review Data/2017 PUF/WKSH1_PUF_2017_20171024.csv'))
puf2017b <- read_csv(here(rawdata,'CMS Rate Review Data/2017 PUF/WKSH2_PUF_2017_20171024.csv'))
puf2017anc <- puf2017a[puf2017a$STATE == "NC", ]
puf2017bnc <- puf2017b[puf2017b$STATE == "NC", ]

# 2018
puf2018a <- read_csv(here(rawdata,'CMS Rate Review Data/2018 PUF/WKSH1_PUF_2018_20180102.csv'))
puf2018b <- read_csv(here(rawdata,'CMS Rate Review Data/2018 PUF/WKSH2_PUF_2018_20180102.csv'))
puf2018anc <- puf2018a[puf2018a$STATE == "NC", ]
puf2018bnc <- puf2018b[puf2018b$STATE == "NC", ]

# 2019
puf2019a <- read_csv(here(rawdata,'CMS Rate Review Data/2019 PUF/WKSH1_PUF_2019_20181025.csv'))
puf2019b <- read_csv(here(rawdata,'CMS Rate Review Data/2019 PUF/WKSH2_PUF_2019_20181025.csv'))
puf2019anc <- puf2019a[puf2019a$STATE == "NC", ]
puf2019bnc <- puf2019b[puf2019b$STATE == "NC", ]

# 2020
puf2020a <- read_csv(here(rawdata,'CMS Rate Review Data/2020 PUF/PY2020_WKSH1_PUF_20201125.csv'))
puf2020b <- read_csv(here(rawdata,'CMS Rate Review Data/2020 PUF/PY2020_WKSH3_PUF_20201125.csv'))
puf2020anc <- puf2020a[puf2020a$STATE == "NC", ]
puf2020bnc <- puf2020b[puf2020b$STATE == "NC", ]

### stack
puf_1 <- rbind.fill(puf2014anc, puf2015anc, puf2016anc, puf2017anc, puf2018anc, 
                    puf2019anc, puf2020anc)
puf_2 <- rbind.fill(puf2014bnc, puf2015bnc, puf2016bnc, puf2017bnc, puf2018bnc, 
                    puf2019bnc, puf2020bnc)
################################################################################
# Issuer level enrollment data
enroll_2014 <- read_excel(here(rawdata,
                             'CMS Issuer Level Enrollment Data/2014-Issuer-Data-Final_10_25.xlsx'))
enroll_2015 <- read_excel(here(rawdata,
                                 'CMS Issuer Level Enrollment Data/2015 Issuer Data Final_.xlsx'))
enroll_2016 <- read_excel(here(rawdata,
                                 'CMS Issuer Level Enrollment Data/2016-Issuer-Enrollment-Disenrollment-Report.xlsx'))
enroll_2017 <- read_excel(here(rawdata,
                                 'CMS Issuer Level Enrollment Data/2017 Enrollment Disenrollment PUF.xlsx'),
                          skip=1)
enroll_2018 <- read_excel(here(rawdata,
                                 'CMS Issuer Level Enrollment Data/2018-Enrollment-Disenrollment-PUF.xlsx'),
                          skip=1)
enroll_2019 <- read_excel(here(rawdata,
                                 'CMS Issuer Level Enrollment Data/2019-Enrollment-Disenrollment-PUF.xlsx'),
                          skip=1)
enroll_2020 <- read_excel(here(rawdata,
                                 'CMS Issuer Level Enrollment Data/2020-Enrollment-Disenrollment-PUF.xlsx'),
                          skip=1)
# will add common year var below
enroll_2014 <- subset(enroll_2014, select=-c(coverage_year_number))
enroll_2015 <- subset(enroll_2015, select=-c(coverage_year_number))
enroll_2016 <- subset(enroll_2016, select=-c(1))

enrolls <- list(enroll_2014, enroll_2015, enroll_2016, enroll_2017, enroll_2018, 
                enroll_2019, enroll_2020)

# use common colnames for each df
enrollnames <- names(enroll_2014)
lapply(enrolls, function(x) {names(x)=enrollnames})
# add year var
enrolls <- Map(transform, enrolls, year = unlist(years))

### stack
enroll <- rbind.fill(enrolls)
enroll_nc <- enroll[enroll$tenant_id=="NC", ]
length(unique(enroll_nc$selected_insurance_plan))
################################################################################
# Benefits and Cost Sharing

bene_2014 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Benefits and Cost Sharing/',
                             '2014 Benefits_Cost_Sharing_PUF.csv'))
bene_2015 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Benefits and Cost Sharing/',
                             '2015 Benefits_Cost_Sharing_PUF.csv'))
bene_2016 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Benefits and Cost Sharing/',
                             '2016 Benefits_Cost_Sharing_PUF.csv'))
bene_2017 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Benefits and Cost Sharing/',
                             '2017 Benefits_Cost_Sharing_PUF.csv'))
bene_2018 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Benefits and Cost Sharing/',
                             '2018 Benefits_Cost_Sharing_PUF.csv'))
bene_2019 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Benefits and Cost Sharing/',
                             '2019 Benefits_Cost_Sharing_PUF.csv'))
bene_2020 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Benefits and Cost Sharing/',
                             '2020 Benefits_Cost_Sharing_PUF.csv'))
benefitlist <- list(bene_2014, bene_2015, bene_2016, bene_2017, bene_2018,
                    bene_2019, bene_2020)
# importdate causing issues in concatenation step, so drop here bc useless
benefitlist <- lapply(benefitlist, function(x) x[!names(x) %in% "ImportDate"])
benefits <- rbind.fill(benefitlist)
benefits_nc <- benefits[benefits$StateCode=="NC", ]
# hopefully we'll never use this
################################################################################
# network details
network_2014 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Network/2014 Network_PUF.csv'))
network_2015 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Network/2015 Network_PUF.csv'))
network_2016 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Network/2016 Network_PUF.csv'))
network_2017 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Network/2017 Network_PUF.csv'))
network_2018 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Network/2018 Network_PUF.csv'))
network_2019 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Network/2019 Network_PUF.csv'))
network_2020 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Network/2020 Network_PUF.csv'))

network <- rbind.fill(network_2014, network_2015, network_2016, network_2017,
                      network_2018, network_2019, network_2020)
network_nc <- network[network$StateCode=="NC",]
################################################################################
# plan attributes 
plan_att_2014 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Plan Attributes/2014 Plan_Attributes_PUF.csv'))
plan_att_2015 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Plan Attributes/2015 Plan_Attributes_PUF.csv'))
plan_att_2016 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Plan Attributes/2016 Plan_Attributes_PUF.csv'))
plan_att_2017 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Plan Attributes/2017 Plan_Attributes_PUF.csv'))
plan_att_2018 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Plan Attributes/2018 Plan_Attributes_PUF.csv'))
plan_att_2019 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Plan Attributes/2019 Plan_Attributes_PUF.csv'))
plan_att_2020 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Plan Attributes/2020 Plan_Attributes_PUF.csv'))

attriblist <- list(plan_att_2014, plan_att_2015, plan_att_2016, plan_att_2017, 
                   plan_att_2018, plan_att_2019, plan_att_2020)
# importdate causing issues in concatenation step, so drop here bc useless
attriblist <- lapply(attriblist, function(x) x[!names(x) %in% "ImportDate"])
plan_attributes <- rbind.fill(attriblist)
plan_attributes_nc <- plan_attributes[plan_attributes$StateCode=="NC",]
################################################################################
# service area 
service_area_2014 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Service Area/2014 Service_Area_PUF.csv'))
service_area_2015 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Service Area/2015 Service_Area_PUF.csv'))
service_area_2016 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Service Area/2016 Service_Area_PUF.csv'))
service_area_2017 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Service Area/2017 Service_Area_PUF.csv'))
service_area_2018 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Service Area/2018 Service_Area_PUF.csv'))
service_area_2019 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Service Area/2019 Service_Area_PUF.csv'))
service_area_2020 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange/Service Area/2020 Service_Area_PUF.csv'))

service_area <- rbind.fill(service_area_2014, service_area_2015, service_area_2016, service_area_2017,
                           service_area_2018, service_area_2019, service_area_2020)
service_area_nc <- service_area[service_area$StateCode=="NC",]
################################################################################
# plan id crosswalk
xwalk_2014 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange',
                            'Plan ID Crosswalk/2014 Plan_Crosswalk_PUF_2014-12-22.csv'))
xwalk_2015 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange',
                            'Plan ID Crosswalk/2015 plan-id-crosswalk-puf.csv'))
xwalk_2016 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange',
                            'Plan ID Crosswalk/2016 MODIFIED_FINAL_TRANSFER_FILE.csv'))
xwalk_2017 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange',
                            'Plan ID Crosswalk/2017 Plan_ID_Crosswalk_PUF.csv'))
xwalk_2018 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange',
                            'Plan ID Crosswalk/2018 Plan_ID_Crosswalk_PUF.csv'))
xwalk_2019 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange',
                            'Plan ID Crosswalk/2019 Plan_ID_Crosswalk_PUF.csv'))
xwalk_2020 <- read_csv(here(rawdata, 'CMS Health Insurance Exchange',
                            'Plan ID Crosswalk/2020 Plan_ID_Crosswalk_PUF.csv'))
xwalks <- list(xwalk_2014, xwalk_2015, xwalk_2016, xwalk_2017, xwalk_2018, 
               xwalk_2019, xwalk_2020)
# common column names for stacking
namelist <- names(xwalk_2014)
namelist <- str_replace(namelist, "_2014","_year")
namelist <- str_replace(namelist, "_2015","_next_year")
namelist <- str_replace(namelist, "2015","_next_year")

# change names, add year variable
lapply(xwalks, function(x) {names(x)=namelist})
xwalks <- Map(transform, xwalks, year = unlist(years))

xwalk <- rbind.fill(xwalks)
xwalk_nc <- xwalk[xwalk$State=="NC",]
################################################################################
# skip business rules 
################################################################################
# save final datasets to ./data
save(puf_1,file=here(data,"puf_1.Rda"))
save(puf_2,file=here(data,"puf_2.Rda"))
save(enroll_nc,file=here(data,"enrollment.Rda"))
save(benefits_nc,file=here(data,"benefits.Rda"))
save(network_nc,file=here(data,"networks.Rda"))
save(plan_attributes_nc,file=here(data,"plan_attributes.Rda"))
save(service_area_nc,file=here(data,"service_areas"))
save(xwalk_nc,file=here(data,"plan_crosswalk.Rda"))
################################################################################
################################################################################
################################################################################
