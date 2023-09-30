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

###[1] Load checked raw data----------------------------------------------------------
load(file="./02_rawdata/checked_rawfiles.rda")

### [2] Experimental Phase categorisation------------------------------------------------------
#categorising the observations according to the experimental phases 
#they were collected in


#create an empty experimental phase variable "exp_phase"
rawdata_ID_all$exp_phase <- NA

#define the factors of the experimental phase variable (Dark, Lab & Field)
rawdata_ID_all$exp_phase <- factor(rawdata_ID_all$exp_phase, 
                                labels = c("Dark", "Lab", "Field"), 
 
                                                               levels = c(1,2,3))

#As a first approximate categorisation step, mark all data below 1.5 lx as dark data.
for (i in 1: nrow(rawdata_ID_all)){
  if (!is.na(rawdata_ID_all$phot_lux[i]) & rawdata_ID_all$phot_lux[i] < 1.5)
    {rawdata_ID_all$exp_phase[i] <- "Dark"}
}


#Refining the categorisation by looking at the light data points visually 

#2 functions (1 tagger incl. 1 ggplot helper function) are used to visualize
#the data for "tagging" the correct experimental phase.
#The data is categorized  according to sequence of the study protocol:
#1) dark adaptation 2) laboratory condition 3) field condition

#a) gg helper function to plot the light data (photopic illuminance) as a function
# of sample number along with vertical lines for easier reading of the x axis values
#input is the id of the participant whose data is plotted
#the colour of the dots give the experimental phase of that sample after
#they have been tagged (sequentially for each participant) after visual inspection

#In the visual inspection we determined the start and end of the laboratory phase.
#The 10min dark adaptation phase always finishes before the laboratory condition,
#and the field condition always starts after the laboratory condition
#The first few samples of each subject's data (> 1.5 lx) and the first 3 samples
#after the laboratory condition are "transition samples" that are between conditions.
# So are the "20 second darkness" settings between the laboratory light conditions. 
#These samples are not tagged because they can not be clearly categorized to any condition
#all samples following the 3 transition samples after the lab condition has concluded
#are part of the  field condition (until the end of the protocol)

gg  <- function(ids) {
  ggplot(rawdata_ID_all[rawdata_ID_all$id == ids,],
         aes(x = sample_nr, y = phot_lux, fill= exp_phase
  ))+ 
    scale_y_log10(limits = c(0.1,2000), 
                  breaks = c(0.1,1,10,100,1000),
                  labels = c(0.1,1,10,100,"1k"))+
    scale_x_continuous(name = "", limits = c(0,180), 
                       breaks = seq(0,180,5),
                       #labels = seq(140,190,10)
    ) +
    geom_point(shape=21, size=2.25, alpha=0.6, colour="black")+
    labs(x=" ")+
    theme_linedraw()+
    theme(legend.position = "none")
}


#tagging function 
# input: id of the participant + sample numbers that mark the lab condition
#tagging the laboratory phase is done with visual inspection
#using the plot of the photopic lux levels for the first 200 samples
#and knowledge of the length of the lab condition (13 min; 40 sec duration)

#In the tagger function, we provide the id of the subject and
#sample numbers that correspond to start and end of the laboratory condition.
#The first 3 samples (30 seconds) after the laboratory condition are tagged as NA,
#as this is the transition phase from the lab condition to the field condition
#after that all samples are tagged as field conditions.
#the tagger function incorporates the gg helper function defined above.

tagger  <- function(ids, labsamples)
  {
  rawdata_ID_all$exp_phase[rawdata_ID_all$id == ids & rawdata_ID_all$sample_nr 
                           %in% labsamples] <<- "Lab"
  
  rawdata_ID_all$exp_phase[rawdata_ID_all$id == ids & rawdata_ID_all$sample_nr 
                           %in% (max(labsamples)+1):(max(labsamples)+3)] <<- NA
  
  rawdata_ID_all$exp_phase[rawdata_ID_all$id == ids & rawdata_ID_all$sample_nr 
                           %in% (max(labsamples)+4):max(rawdata_ID_all$sample_nr
                                        [rawdata_ID_all$id == ids])] <<-"Field"
  
 
gg(ids)

}


#Summary of categorizing the samples according to the protocol:
#a) first few samples (> 1.5 lx): initiating measurements --> NA
#b) 10min dark adaptation condition
#c) 13:40 min laboratory condition
#d) 3 transition samples --> NA
#e) Field condition


#SP001
tagger("SP001", 73:149)


#example of the results of "tagging" the observations according to their 
#experimental phase for id #SP001
#Initiating measurements: NA (Sample No 0-4)
#10min dark adaptation condition (Sample No 5-72)
#13:40 min laboratory condition (Sample No 73-149)
#3 transition samples: NA (Sample No 150-151)
#Field condition (Sample No 152-end)

#SP002
#NA --> excluded

#SP003
tagger("SP003", 72:175)


#SP004

tagger("SP004", 70:148)


#SP005


tagger("SP005", 71:150)

#SP006 
tagger("SP006", 71:148)

#SP007 excluded due to format error in the pupil data
tagger("SP007", 71:149)

#SP008
tagger("SP008", 70:148)

#SP009
tagger("SP009", 72:150)


#SP010
tagger("SP010", 70:148)



#SP011
tagger("SP011", 71:149)

#SP012
tagger("SP012", 75:152)

#SP013
tagger("SP013", 75:153)


#SP014
tagger("SP014", 72:150)

#SP015
tagger("SP015", 76:154)

#SP016 NA

#SP017
tagger("SP017", 74:152)


#SP018
tagger("SP018", 74:152)

#SP019 NA excluded due to format error in the pupil data

#SP020
tagger("SP020", 77:155)

#SP021
tagger("SP021", 71:148)


#SP022
tagger("SP022", 83:161)


#SP023 excluded due to slippage of the spectrometer
#dark-adaptation phase was restarted after 7 minutes
tagger("SP023", 114:193)


#SP024
tagger("SP024", 76:153)

#SP025
tagger("SP025", 77:154)

#SP026

tagger("SP026", 70:148)

#SP027
tagger("SP027", 72:150)

#SP028
tagger("SP028", 75:152)

#SP029
tagger("SP029", 72:150)

#SP030
tagger("SP030", 78:156)


#SP031
tagger("SP031", 67:145)

#SP032
tagger("SP032", 76:154)

#SP033
tagger("SP033", 74:152)


#SP034
tagger("SP034", 80:157)


#SP035 excluded due to software/format error in the pupil data
tagger("SP035", 81:159)


#SP036 NA

#SP037
tagger("SP037", 80:159)


#SP038
tagger("SP038", 74:152)

#SP039

tagger("SP039", 73:151)

#SP040
tagger("SP040", 68:145)

#SP041
tagger("SP041", 75:152)

#SP042 NA

#SP043
tagger("SP043", 72:149)


#SP044
tagger("SP044", 71:148)

#SP045 NA #SP035 excluded due to software/format error in the data


#SP046
# dark phase started a couple of minutes later
tagger("SP046", 99:176)


#SP047
tagger("SP047", 72:149)

#SP048
tagger("SP048", 89:166)


#SP049
tagger("SP049", 73:150)

#SP050
tagger("SP050", 80:158)


#SP051 NA error during calibration. pupil size model inconsistent
#SP052 NA format/software error in pupil size

#SP053
tagger("SP053", 71:149)

#SP054
tagger("SP054", 73:151)


#SP055
tagger("SP055", 75:151)


#SP056
tagger("SP056", 77:154)


#SP057
#Participant's dark adaptation was repeated 
#(20min in total instead of 10 minutes) due misunderstanding in the protocol
tagger("SP057", 149:226)


#SP058
tagger("SP058", 73:150)


#SP059
tagger("SP059", 78:154)


#SP060 na formatting, software errors

#SP061
tagger("SP061", 74:151)

#SP062 excluded because of eye camera slippage
tagger("SP062", 71:149)

#SP063
tagger("SP063", 83:161)


#SP064
tagger("SP064", 78:155)

#SP065
tagger("SP065", 72:151)


#SP066
tagger("SP066", 73:150)

#SP067
tagger("SP067", 71:150)


#SP068
# extended dark phase by 3min (due some dim light interference after first 3 min)
tagger("SP068", 95:173)

#SP069 NA data format/software error in the pupil data

#SP070
tagger("SP070", 71:149)

#SP071
tagger("SP071", 75:152)

#SP072
tagger("SP072", 73:151)

#SP073
tagger("SP073", 77:154)

#SP074
tagger("SP074", 73:150)

#SP075 na

#SP076
tagger("SP076", 77:154)

#SP077
tagger("SP077", 72:151)

#SP078 NA

#SP079 NA

#SP080
tagger("SP080", 73:152)


#SP081
tagger("SP081", 72:150)


#SP082
tagger("SP082", 79:156)


#SP083
tagger("SP083", 77:155)


#SP084
tagger("SP084", 73:150)

#SP085
#Participant's dark adaptation was repeated 
#(20min in total instead of 10 minutes) due misunderstanding in the protocol
tagger("SP085", 164:241)

#SP086 NA data format/software error in the pupil data

#SP087 
tagger("SP087", 77:155)

#SP088 excluded due to connection loss with spectroradiometer 
tagger("SP088", 76:154)

#SP089
tagger("SP089", 72:150)

#SP090
tagger("SP090", 74:151)

#SP091
tagger("SP091", 74:164)

#SP092
tagger("SP092", 69:147)

#SP093
tagger("SP093", 69:147)

#SP094 excluded camera slippage after calibration. Pupil size model not consistent
tagger("SP094", 82:160)

#SP095
tagger("SP095", 70:149)

#SP096
tagger("SP096", 72:150)

#SP097
tagger("SP097", 75:153)

#SP098
tagger("SP098", 72:149)

#SP099 NA # connection loss with the raspberry pie. no data saved

#SP100
tagger("SP100", 72:150)

#SP101
tagger("SP101", 71:149)

#SP102 NA excluded due to software/format error in the data

#SP103 NA excluded due to software/format error in the data

#SP104 NA excluded due to software/format error in the data

#SP105 NA excluded Recording interrupted due to interruption in recording
#during 2nd minute of the protocol + corrupted and missing files afterwards

#SP106
tagger("SP106", 73:150)


#SP107 NA

#SP108
tagger("SP108", 77:154)

#SP109
tagger("SP109", 78:156)

#SP110

#dark condition had to be tagged by hand because the phot_lux values were slightly 
#higher due to an error in the dark-adaptation set-up
rawdata_ID_all$exp_phase[rawdata_ID_all$id == "SP110"
                         & rawdata_ID_all$phot_lux < 3] <- "Dark"
tagger("SP110", 83:161)


#SP111
tagger("SP111", 74:138) 

#light levels in the last Lab light condition were not as planned,
#these samples were tagged as NA in exp_phase
rawdata_ID_all$exp_phase[rawdata_ID_all$id == "SP111"
                         & rawdata_ID_all$sample_nr %in% 139:152] <- NA
gg("SP111")

#SP112
tagger("SP112", 74:152) 

#SP113
tagger("SP113", 73:151)

#mark all the transition samples in between lab light changes as NA for exp_phase
#threshold for phot_lux was used to identify 
#where samples were below the experimental light intensities --> transition samples
for (i in 1: nrow(rawdata_ID_all)){
  if (!is.na(rawdata_ID_all$phot_lux[i]) & rawdata_ID_all$phot_lux[i] < 7 & 
      rawdata_ID_all$exp_phase[i] == "Lab" & !is.na(rawdata_ID_all$exp_phase[i]))
  {rawdata_ID_all$exp_phase[i] <- NA}
}


#The measured values during the dark phase are not valid light measurements
#the spectrometer does not measure accurately around 1 lux
#and the "real" values in the dark adaptation phase should be really close to 0
# we set all light data during this phase to NA (including the log10_values)

#The dark phase is only used as positive control data
# and to compute and visualize the maximum pupil dilation possible
#and NOT for testing the confirmatory hypotheses


#set the dark phase light data to NA
for (i in 1: nrow(rawdata_ID_all)){
  if ((rawdata_ID_all$exp_phase[i] == "Dark"  & !is.na(rawdata_ID_all$exp_phase[i])) |
       (rawdata_ID_all$Mel_EDI[i] < 1 & !is.na(rawdata_ID_all$Mel_EDI[i])) |
      (rawdata_ID_all$phot_lux[i] < 1 & !is.na(rawdata_ID_all$phot_lux[i])) 
      )
  {rawdata_ID_all$`CIE 1931 x`[i] <- NA
  rawdata_ID_all$`CIE 1931 y`[i] <- NA
  rawdata_ID_all$phot_lux[i] <- NA
  rawdata_ID_all$SConeIrrad[i] <- NA
  rawdata_ID_all$MConeIrrad[i] <- NA
  rawdata_ID_all$LConeIrrad[i] <- NA
  rawdata_ID_all$RodIrrad[i] <- NA
  rawdata_ID_all$MelIrrad[i] <- NA
  rawdata_ID_all$Mel_EDI[i] <- NA
  rawdata_ID_all$log_MelIrrad[i] <- NA
  rawdata_ID_all$log_Mel_EDI[i] <- NA
  rawdata_ID_all$log_phot_lux[i] <- NA
    }
}


#create a new variable called "MPratio" that gives the ratio between 
#Melanopic irradiance and photopic lux for later plotting
rawdata_ID_all$MPratio<- rawdata_ID_all$MelIrrad/rawdata_ID_all$phot_lux

### [5] Data saving------------------------------------------------------
#save cleaned and categorised dataset
save(rawdata_ID_all, file="./02_rawdata/rawdata_ID_all.rda")

