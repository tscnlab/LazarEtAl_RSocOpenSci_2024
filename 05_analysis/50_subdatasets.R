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

set.seed(20230703) 


### [1] Load merged data ------------------------------------------------------
#loading merged data with included participants 
load(file="./03_datamerge/merged_data_conf.rda")

### [2] General Subdatasets ------------------------------------------------------------
#create subdatasets for calculations, hypothesis testing and plotting

#remove data with missing pupil and save into cf_data sub dataset
cf_data <- merged_data_conf[!is.na(merged_data_conf$diameter_3d),] 


#create subdatasets of the cfdata set

#field data, which includes only complete observations (light intensity & pupil size)

Fielddata <- cf_data[!is.na(cf_data$exp_phase)  & cf_data$exp_phase == "Field" &
                       !is.na(cf_data$Mel_EDI),]

#number of observations of field data
obs_Fielddata <- nrow(Fielddata)


#Dark data, which includes only complete valid pupil size data (like cfdata)
#light data is not used in the "dark adaptation", 
#since spectroradiometric measures usually contain some noise in very dim conditions
Darkdata <- cf_data[!is.na(cf_data$exp_phase)  & cf_data$exp_phase == "Dark",]

#number of observations of field data
obs_Darkdata <- nrow(Darkdata)


#Labdata,  which includes only complete observations (light intensity & pupil size)
Labdata <- cf_data[!is.na(cf_data$exp_phase) & cf_data$exp_phase == "Lab" &
                     !is.na(cf_data$Mel_EDI),
]

#number of observations of field data
obs_Labdata <- nrow(Labdata)


#data in the transitional phase between lab and field conditions that was not tagged
#and does not belong to either condition
#includes only complete observations (light intensity & pupil size)
Transitiondata <- cf_data[is.na(cf_data$exp_phase)&
                            !is.na(cf_data$Mel_EDI),]

#number of observations of field data
obs_Transitiondata <- nrow(Transitiondata)

# adding up the different subdatasets to all data observations
obs_allincl <- obs_Transitiondata+obs_Labdata+obs_Darkdata+obs_Fielddata


# field  & dark data combined (used for plotting only)
Darkfield <- cf_data [!is.na(cf_data$exp_phase)  & ((cf_data$exp_phase == "Field" |
                       cf_data$exp_phase == "Dark")),]

# field  & dark data combined (used for "postive control" tables only)
Darklab <-  cf_data [!is.na(cf_data$exp_phase)  & ((cf_data$exp_phase == "Lab" |
                                                  cf_data$exp_phase == "Dark")),]

### [3] Subdatasets for visualising the age effect -------------------------------
#Figures 7 and 8 in the manuscript
#field data at specific light intensities (for plots inspired by Winn et al 1994)
#Instead of Melanopic Irradiance we now use Melanopic EDI (linear transformation)
#as mEDI has become the most established light metric
#we also included the dark-adaptation condition (positive control) for comparison

#field data over 1000 lx m-EDI
#grouped per subject and summarized for plot in Figures 7 E & 8 (A, B)
Fieldage_over1klux <- Fielddata[Fielddata$Mel_EDI > 1000,]  %>%
  group_by(id) %>%
  summarise("Mean_pupil" = mean(diameter_3d),
            "Median_pupil" = median(diameter_3d),
            "Min_pupil" = min(diameter_3d),
            "Max_pupil" = max(diameter_3d),
            "Range_pupil" = (Max_pupil - Min_pupil),
            "Q25" = quantile(diameter_3d, probs = .25),
            "Q75" = quantile(diameter_3d, probs = .75),
            "age"= mean(age),
  )



#field data over >100 lx and <= 1000 lx  m-EDI
#grouped per subject and summarized for plot in Figures 7 D & 8 (A, B)
Fieldage_to1klux <- Fielddata[Fielddata$Mel_EDI > 100 & Fielddata$Mel_EDI <= 1000,]
Fieldage_to1klux <- Fieldage_to1klux %>%
  group_by(id) %>%
  summarise("Mean_pupil" = mean(diameter_3d),
            "Median_pupil" = median(diameter_3d),
            "Min_pupil" = min(diameter_3d),
            "Max_pupil" = max(diameter_3d),
            "Range_pupil" = (Max_pupil - Min_pupil),
            "Q25" = quantile(diameter_3d, probs = .25),
            "Q75" = quantile(diameter_3d, probs = .75),
            "age"= mean(age),
  )

#field data over >10 lx and <= 100 lx  m-EDI
#grouped per subject and summarized for plot in Figures 7 C & 8 (A, B)
Fieldage_to100lux <- Fielddata[Fielddata$Mel_EDI > 10 & Fielddata$Mel_EDI <= 100,]
Fieldage_to100lux <- Fieldage_to100lux %>%
  group_by(id) %>%
  summarise("Mean_pupil" = mean(diameter_3d),
            "Median_pupil" = median(diameter_3d),
            "Min_pupil" = min(diameter_3d),
            "Max_pupil" = max(diameter_3d),
            "Range_pupil" = (Max_pupil - Min_pupil),
            "Q25" = quantile(diameter_3d, probs = .25),
            "Q75" = quantile(diameter_3d, probs = .75),
            "age"= mean(age),
  )

#field data over >1 lx and <= 10 lx  m-EDI
#grouped per subject and summarized for plot in Figures 7 B & 8 (A, B)
Fieldage_to10lux<- Fielddata[Fielddata$Mel_EDI > 1 & Fielddata$Mel_EDI <= 10,]
Fieldage_to10lux<- Fieldage_to10lux%>%
  group_by(id) %>%
  summarise("Mean_pupil" = mean(diameter_3d),
            "Median_pupil" = median(diameter_3d),
            "Min_pupil" = min(diameter_3d),
            "Max_pupil" = max(diameter_3d),
            "Range_pupil" = (Max_pupil - Min_pupil),
            "Q25" = quantile(diameter_3d, probs = .25),
            "Q75" = quantile(diameter_3d, probs = .75),
            "age"= mean(age),
  )


#10 min dark adaptation data (positive control condition)
#grouped per subject and summarized for plot in Figures 7 A & 8 (A, B)
Darkage <- Darkdata %>%
  group_by(id) %>%
  summarise("Mean_pupil" = mean(diameter_3d),
            "Median_pupil" = median(diameter_3d),
            "Min_pupil" = min(diameter_3d),
            "Max_pupil" = max(diameter_3d),
            "Range_pupil" = (Max_pupil - Min_pupil),
            "Q25" = quantile(diameter_3d, probs = .25),
            "Q75" = quantile(diameter_3d, probs = .75),
            "age"= mean(age),
  )

#all field data grouped per subject and summarized 
#creating Pupilage" dataset for Figure 8 C to compare pupil range across age

Pupilage <- Fielddata %>%
  group_by(id) %>%
  summarise("Mean_pupil" = mean(diameter_3d),
            "Median_pupil" = median(diameter_3d),
            "Min_pupil" = min(diameter_3d),
            "Max_pupil" = max(diameter_3d),
            "Range_pupil" = (Max_pupil - Min_pupil),
            "Q25" = quantile(diameter_3d, probs = .25),
            "Q75" = quantile(diameter_3d, probs = .75),
            "IQR" =(Q75-Q25),
            "age"= mean(age),
  )



### [4] Subdataset for weather data -------------------------------------------


#creating subdata set with weather data 
# The dataset contains more observations, as missing pupil data are still included)

weatherdata <- merged_data_conf[merged_data_conf$exp_phase == "Field" &
                                  !is.na(merged_data_conf$exp_phase) &
                                  !is.na(merged_data_conf$phot_lux)
                                  ,] %>%
  select(id, sample_nr, date, begin, sample_nr, 
         phot_lux, MelIrrad, Mel_EDI, SConeIrrad, MConeIrrad, LConeIrrad, RodIrrad,
         SCone_EDI, MCone_EDI, LCone_EDI, Rod_EDI,
         weather, exp_phase, season)

### [5] Subdatasets for case data -------------------------------------------

#create "agecomp data" for Figure 6 (plotting only)
# comparing 2 typical subjects at 18 and 87 years old

agecomp <- cf_data[!cf_data$exp_phase == "Lab" & !is.na(cf_data$exp_phase) & 
                     (cf_data$id == "SP076" | cf_data$id == "SP038"),]

#set the dark phase light data to 0 for the data in Fig. 6,
#because they cannot be plotted when m-EDI is set to "NA".

for (i in 1: nrow(agecomp)){
  if ((agecomp$exp_phase[i] == "Dark")
  )
  {agecomp$phot_lux[i] <- 0
  agecomp$SConeIrrad[i] <- 0
  agecomp$MConeIrrad[i] <- 0
  agecomp$LConeIrrad[i] <- 0
  agecomp$RodIrrad[i] <- 0
  agecomp$MelIrrad[i] <- 0
  agecomp$Mel_EDI[i] <- 0
  
  }
}


### [6] Subdatasets for autocorrelation  ---------------------------------------
#autocorrelation approach (create subdata set) for Suppl. Fig. 4

#create a subdataset with all NAs still included (no missing rows) 
# but with all Field data
#then make sure it is ordered by id and sample_nr

autocor_data <- merged_data_conf[merged_data_conf$exp_phase == "Field" &
                                   !is.na (merged_data_conf$exp_phase) ,] %>% 
  arrange(id, sample_nr) #%>% group_by(id)

#create a "filler" variable that fills a number of NA rows between ids
#the number of rows in the filler corresponds to the autocor lag +1
# This way, the acf algorithm does NOT falsely take into account the
#autocorrelation of samples from DIFFERENT ids

#if we want to adjust the max lag in the autocorrelation to more than 18 samples (3 min)
# we also need to adjust the number of rows in the NA filler var

filler = data.frame(matrix( nrow = 19, ncol=length(autocor_data)))

#the id length var is used for the for loop
# because we will fill NA values after every id in the included dataset (n=83)
# we need to subtract idlegnth -1 because after the last id we do NOT need to
# add any NA values 

idlength<- length(unique(autocor_data$id))

#the for loop includes the "autocor_data" + adding the length of the added
#NA values which corresponds to (n-1)*(max.lag+1)
# in this case this is nrow(autocor_data) + (83-1) *(18+1)
# the for loop goes through the full dataframe row by row and checks whether the 
# sample_nr is ascending by 1. If this is NOT the case (and it's not a missing value)
# this is due to the data of the next id is starting
# if this is the case (= if condition) the loop adds 19 NA values (= "filler")
#between where the 2 ids "meet" (i:i+18). This is done via the "insertRows" function
#of the "berryFunctions" package

for (i in 2:(nrow(autocor_data)+((idlength-1)*nrow(filler))))
{if (autocor_data$sample_nr[i] != (autocor_data$sample_nr[i-1]+1) & 
     !is.na(autocor_data$sample_nr[i-1]) & 
     !is.na(autocor_data$sample_nr[i])
)
{autocor_data <- berryFunctions::insertRows(autocor_data, r = (i:(i+18)), new = filler 
)}
}

#after the filler NAs are added to the autocor_data, 

#computing the autocorrelation for the Melanopic EDI variable in the field data
mel_acf <- data.frame(cor = acf(autocor_data$Mel_EDI,
                                lag.max = 18, na.action = na.pass, plot = F)$acf,
                      lag = acf(autocor_data$Mel_EDI,
                                lag.max = 18, na.action = na.pass, plot = F)$lag)

#computing the autocorrelation for the Melanopic EDI variable in the field data
pupil_acf <- data.frame(cor = acf(autocor_data$diameter_3d,
                                  lag.max = 18, na.action = na.pass, plot = F)$acf,
                        lag = acf(autocor_data$diameter_3d,
                                  lag.max = 18, na.action = na.pass, plot = F)$lag)

#the autocorrelation slightly increases compared to 
#NOT using the "filler" work-around autocorellation data approach
#This shows that the work-around was succesful:
# the autocorrelation now does not take into account the samples of a the previous subject


### [7] Saving subdatasets----------------------------------------------------------------------------------------------------------

#save all created subdataset in the environment via save.image
save.image(file="./05_analysis/subdata/conf_subdata.rda")

