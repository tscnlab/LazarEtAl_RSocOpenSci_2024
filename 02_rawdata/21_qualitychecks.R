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

#load libraries
library(tidyverse)

###[1] Load raw data----------------------------------------------------------
load(file="./02_rawdata/rawfiles.rda")

### [2] Data cleaning & quality checks-----------------------------------------

#number of total pupil samples below 0.6 confidence
a <- length(rawdata_ID_all$confidence[rawdata_ID_all$confidence < 0.6])
#number of missing values of confidence (from participants excluded before trial)
b <- sum(is.na(rawdata_ID_all$confidence))
#number of total pupil samples >= 0.6 confidence
c <- length(rawdata_ID_all$confidence[rawdata_ID_all$confidence >= 0.6])
#check whether sum of values is equal to length of data
d=a+c-b
d==length(rawdata_ID_all$confidence)
#total pupil samples data loss due to confidence relative to all samples in perc
data_loss_conf<- round(a/d*100, 2)

#total pupil samples data loss due to confidence relative to all samples in perc

data_loss_conf<- round(a/d*100, 2)

#"Lack of good-quality fit" quality check
#set all  pupil data with confidence (derived from Pupil Labs software) < 0.6 = NA
# this is according to the pupil labs recommendations
rawdata_ID_all$diameter_3d[rawdata_ID_all$confidence < 0.6] <- NA


#"Pupil size screening" quality check

#check the number of "out of bounds" data for pupil size when low confidence data is removed
e <- sum(rawdata_ID_all$diameter_3d > 9, na.rm = T) +
sum(rawdata_ID_all$diameter_3d < 1, na.rm = T)

data_loss_range<- round(e/d*100, 2)

# set all pupil size values over 9 or smaller than 1 to NA
rawdata_ID_all$diameter_3d[rawdata_ID_all$diameter_3d > 9] <- NA
rawdata_ID_all$diameter_3d[rawdata_ID_all$diameter_3d < 1] <- NA


f <- sum(rawdata_ID_all$phot_lux == 0,na.rm=T)
data_loss_satspec<- round(f/d*100, 2)


#"Saturated spectroradiometer samples" quality check
#set all saturated / erroneous light data given by 0 = NA
  rawdata_ID_all$phot_lux[rawdata_ID_all$phot_lux == 0] <- NA
  rawdata_ID_all$RodIrrad[rawdata_ID_all$RodIrrad == 0] <- NA
  rawdata_ID_all$SConeIrrad[rawdata_ID_all$SConeIrrad == 0] <- NA
  rawdata_ID_all$MConeIrrad[rawdata_ID_all$MConeIrrad == 0] <- NA
  rawdata_ID_all$LConeIrrad[rawdata_ID_all$LConeIrrad == 0] <- NA
  rawdata_ID_all$MelIrrad[rawdata_ID_all$MelIrrad == 0] <- NA

  
#"Proportion of excluded data"  quality check
  
# compute data loss per participant (grouped per id)
# this includes all data that either has missing pupil size or missing light data
# the missing values of each participant are divided by the sum of all observations
#(the sum of missing and non-missing data)
#resulting in a data loss ratio

rawdata_ID_all <- rawdata_ID_all %>%
  group_by(`id`) %>%
  mutate(`data_loss` = sum(is.na(diameter_3d) | is.na(MelIrrad))/
           (sum(is.na(diameter_3d)| is.na(MelIrrad)) 
            + sum(!is.na(diameter_3d) & !is.na(MelIrrad)))) %>% ungroup()


#Checking data loss & excluding participants with data loss over threshold------

#create a vector with varying data loss thresholds from 0 to 100% (in 5% steps)
data_loss_thresvec <- seq(0,1, 0.05)

#all dataloss values from the dataset that were not excluded before otherwise
#are included in this process (1 data loss value per subject --> n=87)
uniqloss <- unique(rawdata_ID_all$data_loss[rawdata_ID_all$excl=="results"])

#create empty "dataloss over threshold" vector
lossover_thrsvec <- NA

#create a for loop that goes through all participants' data loss values
#and checks whether each value is exceeding each given data loss threshold (0 to 100%)
#and then sums up the number of participants that are exceeding each threshold

for (i in 1:length(data_loss_thresvec))
{
  lossover_thrsvec[i] <- length(uniqloss[uniqloss > data_loss_thresvec[i]])
}

#match the number of values over threshold with the threshold values
dataloss_rel <- data.frame(data_loss_thresvec,lossover_thrsvec)

# this is used for Figure 4 for  plotting the data loss thresholds vs.
#number of participants over threshold

# The criterion was to find the location beyond the 50% threshold, where 
# increasing the threshold in in 5% steps would lead to the minimally increased
# number of excluded participants. 

# In this data the smallest increase of people over the threshold
#is given in the step from 75% to 80% (+0 subjects over threshold)
#thus  leading to an adjusted threshold of 75, #yielding n=4 excluded 
#participants and retaining n=83.
# CAVE: in RR Stage 1 data_loss_thres was originally specified as 0.5
data_loss_thres <- 0.75

# To compute the results according to the initial data loss threshold of 0.5
#UNCOMMENT THE FOLLOWING CODE LINE

#data_loss_thres <- 0.5


#data loss values that are over the just specified data loss threshold
# CAVE: in RR Stage 1 this was specified as 0.5 while we added a second adjusted
#threshold of 0.75. Results for both datasets are reported

datalossover_thrs <- uniqloss[uniqloss > data_loss_thres]

#find out the IDs of the participant's datasets which exceed the threshold
id_datalossover_thrs <- 
  unique(rawdata_ID_all$id[rawdata_ID_all$data_loss %in% datalossover_thrs])


#"This many pariticpants are excluded additionally due to exceeding the data loss threshold"
id_datalossover_thrs
length(id_datalossover_thrs)

#tag these datasets to be "EXCLUDED" in the "excl" variable
#and tag them as "POST" for the exclusion time variable
rawdata_ID_all$excl[rawdata_ID_all$id %in% id_datalossover_thrs] <- "EXCLUDED"
rawdata_ID_all$excl_time[rawdata_ID_all$id %in% id_datalossover_thrs] <- "POST"

#compute total retained data among the included subjects
total_retained_data <- sum(!is.na(rawdata_ID_all$diameter_3d) &
                             !is.na(rawdata_ID_all$MelIrrad)&
                             rawdata_ID_all$excl=="results"
                             )
#compute total retained data as a ratio in percent
retained_data_perc <- 100*(1-(sum(is.na(rawdata_ID_all$diameter_3d) | is.na(rawdata_ID_all$MelIrrad))/
                                (sum(is.na(rawdata_ID_all$diameter_3d)| is.na(rawdata_ID_all$MelIrrad)) 
                                 + sum(!is.na(rawdata_ID_all$diameter_3d) & !is.na(rawdata_ID_all$MelIrrad)))))


###[3] Light data transformation------------------------------------------------
#compute Melanopic EDI and create log10 transformed light variables

#MelIrrad to Mel_EDI conversion factor
Mel_EDI_convfactor <- 1.32621318911359


#create log10 transformed light data variables & create m-EDI variable

rawdata_ID_all <- rawdata_ID_all %>%
  mutate(`log_MelIrrad` = log10(MelIrrad),
         `log_phot_lux` = log10(phot_lux),
         #Transform MelIrrad to Mel_EDI with conversion factor
         `Mel_EDI` = MelIrrad/1.32621318911359,
         `LCone_EDI` = LConeIrrad/1.62890776589039,
         `MCone_EDI` = MConeIrrad/1.45582633881653,
         `SCone_EDI` = SConeIrrad/0.817289644883213,
         `Rod_EDI` = RodIrrad/1.4497035760559,
         `log_Mel_EDI` = log10(Mel_EDI),
         `log_SCone_EDI` = log10(SCone_EDI),
         `log_MCone_EDI` = log10(MCone_EDI),
         `log_LCone_EDI` = log10(LCone_EDI),
         `log_Rod_EDI` = log10(Rod_EDI)
         )

### [4] Save checked rawfiles---------------------------------------------------
#save the data into a R data file called "checked_rawfiles"
save(rawdata_ID_all, file="./02_rawdata/checked_rawfiles.rda")
save(uniqloss,  file="./02_rawdata/uniqloss.rda")
