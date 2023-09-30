###preparing environment: unloaded prior packages, loading new ones----

#unload packages that were loaded before (run function twice to "catch" all pkgs)
#this is a workaround to avoid masking problems when running the scripts successively
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

# Libraries
library(tidyverse)
library(psych)
library(lubridate)

#load imported survey data and imported raw data
load(file="./01_surveydata/cleaned_survey.rda")
load(file="./02_rawdata/rawdata_ID_all.rda")

#merge the data by the common variable "id".
merged_data_all <- left_join(rawdata_ID_all, surveydata, by="id")

#make id a factor variable
merged_data_all$id <- factor(merged_data_all$id)

# Save full merged data set 
save(merged_data_all, file="./03_datamerge/mergeddata_all.rda")

##### create sub data sets ---------------------------------

# sort out excluded data
#create a dataframe containing only the included data.
merged_data_incl <- merged_data_all[merged_data_all$excl == "results",]


# select variables for confirmatory & exploratory analysis
#and save it as a new dataset
merged_data_conf <- select(
  merged_data_incl, id, date, begin, sample_nr, 
                        diameter_3d, confidence, phot_lux, MelIrrad, Mel_EDI, age, 
                        sex, exp_phase, data_loss, age_group, weather, season,
                        MSFsc, time_awake, sleeping_hours, SDweek, iris_colour, #added vars
                        kss_pre, kss_post,#added vars
                        acute_sum, acute_sum_rel, habitual_sum_rel, #added vars
                        log_MelIrrad, log_phot_lux, log_Mel_EDI, MPratio)

#save this as new dataset
save(merged_data_conf, file="./03_datamerge/merged_data_conf.rda")
