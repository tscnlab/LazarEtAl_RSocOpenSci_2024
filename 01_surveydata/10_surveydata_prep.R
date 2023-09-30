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

# load Libraries needed for survey data
library(tidyverse)
library(psych)
library(lubridate)

### [1] Data import -----------------------------------------------------------

# Read survey data (survey was created on SoSciSurvey.de)
#missing values were coded by "-9"
surveydata <- read_csv2("./01_surveydata/data_chronobiology_2022-04-07_21-20.csv",
                        na="-9")

### [2] Data preparation -----------------------------------------------------------
# Delete superfluous SoSci Survey variables
surveydata <- select(surveydata, -SERIAL, -REF, -QUESTNNR, -MODE)
surveydata <- select(surveydata, -(TIME_SUM:DEG_TIME))
surveydata <- select(surveydata, -(starts_with("TIME"))) # if you're interested in the amount of time spent on a page, keep these variables 

# Rename variables
#first set all to lower case
#replace _ with space
surveydata <- surveydata %>%
  set_names(~str_to_lower(.) %>%
              str_replace_all(" ", "_")) #%>%
             # str_replace_all("it01_", "implicit") %>%
             # str_replace_all("md01_", "mdbf"))

# Rename specific variables
surveydata <- rename(surveydata,
                 begin = started,
                 id = i101_01,
                 ghq01 = g101,
                 ghq02 = g102,
                 ghq03 = g103,
                 ghq04 = g104,
                 ghq05 = g105,
                 ghq06 = g106,
                 ghq07 = g107,
                 ghq08 = g108,
                 ghq09 = g109,
                 ghq10 = g110,
                 ghq11 = g111,
                 ghq12 = g112,
                 ghq_threshold = th02_01,
                 sex = sd01,
                 age = sd02_01,
                 height = sd03_01,
                 weight = sd04_01,
                 handedness = sd05,
                 antihistamin = mp01,
                 antidepressant = mp02,
                 antipsychotic = mp03,
                 motion_sickness = mp04,
                 botox = mp05,
                 atropine = mp06,
                 eye_drops = mp07,
                 muscle_relaxants = mp08,
                 medicine_other = mp09,
                 alcohol_test = dt01,
                 drug_test = dt02,
                 colour_perception = vt01,
                 #contact_lenses = vt02,
                 iris_colour = vt03,
                 visual_acuity_snellen = vt04,
                 text_reading = vt05,
                 kss_pre = ks01,
                 kss_post = ks02,
                 percolator = ch03_01,
                 filter_coffee = ch03_02,
                 instant_coffee = ch03_03,
                 coffee_pads = ch03_04,
                 coffee_capsules = ch03_05,
                 cappuccino = ch03_06,
                 decaf = ch03_07,
                 tea = ch03_08,
                 cacao = ch03_09,
                 espresso = ch04_01,
                 dark_chocolate = ch08_01,
                 milk_chocolate = ch08_02,
                 cola = ch05_01,
                 ice_tea = ch05_02,
                 alcohol_coffeine = ch05_03,
                 energy_drink = ch06_01,
                 energy_shot = ch07_01,
                 coffeine_pills = ch09_01,
                 #other_products = ch10_01,
                 wakeup_time = sw02_01,
                 sleep_time = sw02_02,
                 nap_today = sw03_01,
                 nap_yesterday = sw03_02,
                 squint = ae01,
                 amblyopia = ae02,
                 cataract = ae03,
                 glaukom = ae04,
                 eye_movement_disorder = ae05,
                 retinal_disease = ae06,
                 makula_disease = ae07,
                 eye_inflammation = ae08,
                 visual_aid = ae09,
                 contact_lenses = ae10,
                 #last_appointment_opthalmologist = ae11_01,
                 eye_surgery = ae12,
                 neurological_disorder= ga01,
                 metabolic_disease = ga02,
                 diabetes = ga03,
                 head_injury = ga04,
                 other_injury = ga05,
                 shift_work = mc02,
                 WD = mc03_01,
                 SOw = mc05_01,
                 SEw = mc05_02,
                 SOf = mc06_01,
                 SEf = mc06_02,
                 percolator_acute = ca03_01,
                 filter_coffee_acute = ca03_02,
                 instant_coffee_acute = ca03_03,
                 coffee_pads_acute = ca03_04,
                 coffee_capsules_acute = ca03_05,
                 cappuccino_acute = ca03_06,
                 decaf_acute = ca03_07,
                 tea_acute = ca03_08,
                 cacao_acute = ca03_09,
                 espresso_acute = ca04_01,
                 dark_chocolate_acute = ca08_01,
                 milk_chocolate_acute = ca08_02,
                 cola_acute = ca05_01,
                 ice_tea_acute = ca05_02,
                 alcohol_coffeine_acute = ca05_03,
                 energy_drink_acute = ca06_01,
                 energy_shot_acute = ca07_01,
                 coffeine_pills_acute = ca09_01,
                 #other_products_acute = ca10_01,
                 weather = we01
                 )

# Transform categorical variables into factors
#surveydata$id <- factor(surveydata$id)
                         
surveydata$sex <- factor(surveydata$sex, 
                        labels = c("Female", "Male", "Other"), 
                        levels = c(1,2,3))
surveydata$handedness <- factor(surveydata$handedness, 
                       labels = c("Right", "Left", "Both"), 
                       levels = c(1,2,3))
surveydata$antihistamin <- factor(surveydata$antihistamin, 
                       labels = c("no", "yes"), 
                       levels = c(1,2))
surveydata$antidepressant <- factor(surveydata$antidepressant, 
                       labels = c("no", "yes"), 
                       levels = c(1,2))
surveydata$antipsychotic <- factor(surveydata$antipsychotic, 
                       labels = c("no", "yes"), 
                       levels = c(1,2))
surveydata$motion_sickness <- factor(surveydata$motion_sickness, 
                       labels = c("no", "yes"), 
                       levels = c(1,2))
surveydata$botox <- factor(surveydata$botox, 
                                labels = c("no", "yes"), 
                                levels = c(1,2))
surveydata$atropine <- factor(surveydata$atropine, 
                               labels = c("no", "yes"), 
                               levels = c(1,2))
surveydata$eye_drops <- factor(surveydata$eye_drops, 
                                 labels = c("no", "yes"), 
                                 levels = c(1,2))
surveydata$muscle_relaxants <- factor(surveydata$muscle_relaxants, 
                          labels = c("no", "yes"), 
                          levels = c(1,2))
surveydata$medicine_other <- factor(surveydata$medicine_other, 
                           labels = c("no", "yes"), 
                           levels = c(1,2))
surveydata$alcohol_test <- factor(surveydata$alcohol_test, 
                                labels = c("0", "0.2", ">=0.4"), 
                                levels = c(1,2,3))
surveydata$drug_test <- factor(surveydata$drug_test, 
                                labels = c("negative", "positive"), 
                                levels = c(1,2))
surveydata$colour_perception <- factor(surveydata$colour_perception, 
                                labels = c("yes", "no"), 
                                levels = c(1,2))
surveydata$contact_lenses <- factor(surveydata$contact_lenses, 
                                labels = c("no", "yes"), 
                                levels = c(1,2))
surveydata$iris_colour <- factor(surveydata$iris_colour, 
                                labels = c("Blue", "Hazel/Green", "Brown"), 
                                levels = c(1,2,3))
surveydata$text_reading <- factor(surveydata$text_reading, 
                                labels = c("yes", "no"), 
                                levels = c(1,2))
surveydata$squint <- factor(surveydata$squint, 
                                labels = c("no", "yes"), 
                                levels = c(1,2))
surveydata$amblyopia <- factor(surveydata$amblyopia, 
                                labels = c("no", "yes"), 
                                levels = c(1,2))
surveydata$cataract <- factor(surveydata$cataract, 
                                labels = c("no", "yes"), 
                                levels = c(1,2))
surveydata$glaukom <- factor(surveydata$glaukom, 
                        labels = c("no", "yes"), 
                        levels = c(1,2))
surveydata$eye_movement_disorder <- factor(surveydata$eye_movement_disorder, 
                           labels = c("no", "yes"), 
                           levels = c(1,2))
surveydata$retinal_disease <- factor(surveydata$retinal_disease, 
                          labels = c("no", "yes"), 
                          levels = c(1,2))
surveydata$makula_disease <- factor(surveydata$makula_disease, 
                        labels = c("no", "yes"), 
                        levels = c(1,2))
surveydata$eye_inflammation <- factor(surveydata$eye_inflammation, 
                           labels = c("no", "yes"), 
                           levels = c(1,2))
surveydata$visual_aid <- factor(surveydata$visual_aid, 
                          labels = c("No", "Yes - myopia correction",
                                     "Yes - hyperopia correction"), 
                          levels = c(1,2,3))
surveydata$eye_surgery <- factor(surveydata$eye_surgery, 
                        labels = c("no", "yes"), 
                        levels = c(1,2))
surveydata$neurological_disorder <- factor(surveydata$neurological_disorder, 
                           labels = c("no", "yes"), 
                           levels = c(1,2))
surveydata$metabolic_disease <- factor(surveydata$metabolic_disease, 
                          labels = c("no", "yes"), 
                          levels = c(1,2))
surveydata$diabetes <- factor(surveydata$diabetes, 
                        labels = c("no", "yes"), 
                        levels = c(1,2))
surveydata$head_injury <- factor(surveydata$head_injury, 
                           labels = c("no", "yes"), 
                           levels = c(1,2))
surveydata$other_injury <- factor(surveydata$other_injury, 
                          labels = c("no", "yes"), 
                          levels = c(1,2))
surveydata$shift_work <- factor(surveydata$shift_work, 
                        labels = c("yes", "no"), 
                        levels = c(1,2))
surveydata$weather <- factor(surveydata$weather, 
                           labels = c("Light rain", "Very cloudy", "Cloudy", "Somewhat cloudy", "Sunny"), 
                           levels = c(1,2,3,4,5))

surveydata$visual_acuity_snellen <- factor(surveydata$visual_acuity_snellen, 
                                labels = c("20/200", "20/100","20/70",
                                           "20/50", "20/40","20/30", "20/25",
                                           "20/20", "row 9", "row 10", "row 11"), 
                                levels = 1:11)

### [3] Data transformation #####-------------------------------------------------
# Create BMI variable
surveydata <- surveydata %>%
  mutate(BMI = weight/(height/100)^2)

# Recode caffeine questionnaire variables from -1 to 0
surveydata <- surveydata %>% 
  mutate_at(vars(percolator:coffeine_pills), 
            function(x) case_when(x == -1 ~ 0, x == 1 ~ 1, x == 2 ~ 2,
                                  x == 3 ~ 3, x == 4 ~ 4, x == 5 ~ 5, 
                                  x == 6 ~ 6, x == 7 ~ 7, x == 8 ~ 8))

surveydata <- surveydata %>%
  mutate_at(vars(percolator_acute:coffeine_pills_acute),
            function(x) case_when(x == -1 ~ 0, x == 1 ~ 1, x == 2 ~ 2,
                                  x == 3 ~ 3, x == 4 ~ 4, x == 5 ~ 5,
                                  x == 6 ~ 6, x == 7 ~ 7, x == 8 ~ 8))

# Create sum scores for the habitual and acute caffeine questionnaires
# Habitual caffeine intake questionnaire
surveydata <- surveydata %>%
  mutate(percolator_sum = percolator*80,
         filter_coffee_sum = filter_coffee*44,
         instant_coffee_sum = instant_coffee*42.5,
         coffee_pads_sum = coffee_pads*85,
         coffee_capsules_sum = coffee_capsules*70,
         cappuccino_sum = cappuccino*60,
         decaf_sum = decaf*3,
         tea_sum = tea*32.5,
         cacao_sum = cacao*3,
         espresso_sum = espresso*55,
         dark_chocolate_sum = dark_chocolate*38.5,
         milk_chocolate_sum = milk_chocolate*6.75,
         cola_sum = cola*14,
         ice_tea_sum = ice_tea*15,
         alcohol_coffeine_sum = alcohol_coffeine*170,
         energy_drink_sum = energy_drink*37.5,
         energy_shot_sum = energy_shot*39,
         coffeine_pills_sum = coffeine_pills*50)
#compute sum score
surveydata <- surveydata %>%
  mutate (habitual_sum = percolator_sum + filter_coffee_sum + instant_coffee_sum + coffee_pads_sum +
            coffee_capsules_sum + cappuccino_sum + decaf_sum + tea_sum + cacao_sum + espresso_sum +
            dark_chocolate_sum + milk_chocolate_sum + cola_sum + ice_tea_sum + alcohol_coffeine_sum +
            energy_drink_sum + energy_shot_sum + coffeine_pills_sum)


# Acute caffeine intake questionnaire
surveydata <- surveydata %>%
  mutate(percolator_acute_sum = percolator_acute*80,
         filter_coffee_acute_sum = filter_coffee_acute*44,
         instant_coffee_acute_sum = instant_coffee_acute*42.5,
         coffee_pads_acute_sum = coffee_pads_acute*85,
         coffee_capsules_acute_sum = coffee_capsules_acute*70,
         cappuccino_acute_sum = cappuccino_acute*60,
         decaf_acute_sum = decaf_acute*3,
         tea_acute_sum = tea_acute*32.5,
         cacao_acute_sum = cacao_acute*3,
         espresso_acute_sum = espresso_acute*55,
         dark_chocolate_acute_sum = dark_chocolate_acute*38.5,
         milk_chocolate_acute_sum = milk_chocolate_acute*6.75,
         cola_acute_sum = cola_acute*14,
         ice_tea_acute_sum = ice_tea_acute*15,
         alcohol_coffeine_acute_sum = alcohol_coffeine_acute*170,
         energy_drink_acute_sum = energy_drink_acute*37.5,
         energy_shot_acute_sum = energy_shot_acute*39,
         coffeine_pills_acute_sum = coffeine_pills_acute*50)
#compute sum score 
surveydata <- surveydata %>%
  mutate (acute_sum = percolator_acute_sum + filter_coffee_acute_sum + instant_coffee_acute_sum +
            coffee_pads_acute_sum + coffee_capsules_acute_sum + cappuccino_acute_sum + decaf_acute_sum +
            tea_acute_sum + cacao_acute_sum + espresso_acute_sum + dark_chocolate_acute_sum +
            milk_chocolate_acute_sum + cola_acute_sum + ice_tea_acute_sum + alcohol_coffeine_acute_sum +
            energy_drink_acute_sum + energy_shot_acute_sum + coffeine_pills_acute_sum)

# Create relative sum scores (relative to weight)
#Habitual caffeine consumption
surveydata <- surveydata %>%
  mutate(habitual_sum_rel = habitual_sum/weight)
#Acute caffeine consumption
surveydata <- surveydata %>%
  mutate(acute_sum_rel = acute_sum/weight)


# Micro MCTQ variables computation 
#(see Table S2 from Supplementary Material of Ghotbi et al., 2019)
surveydata <- surveydata %>%
  mutate(FD = 7 - WD,
         SDw = SEw - SOw,
         SDf = SEf - SOf,
         SDw = ifelse(SDw < 0, SDw+86400, SDw),
         SDf = ifelse(SDf < 0, SDf+86400, SDf),
         MSW = SOw + ((SDw)/2),
         MSF = SOf + ((SDf)/2),
         MSW = ifelse(as.numeric(SOw) > 43200, MSW-86400, MSW),
         MSF = ifelse(as.numeric(SOf) > 43200, MSF-86400, MSF),
         SDweek = (SDw * WD + SDf * FD)/7,
         SDweek =SDweek/3600,
         MSFsc = ifelse(SDf > SDw, MSF-(SDf-SDweek)/2, MSF),
         MSFsc = MSFsc/3600)

  
# Sleeping hours
# Recode wakeup and sleep time into numerical variables and transform into hours
surveydata$wakeup_time <- as.POSIXct(surveydata$wakeup_time)
surveydata$wakeup_time <- hour(surveydata$wakeup_time) + minute(surveydata$wakeup_time)/60

surveydata$sleep_time <- as.POSIXct(surveydata$sleep_time)
surveydata$sleep_time <- hour(surveydata$sleep_time) + minute(surveydata$sleep_time)/60

# Create sleeping hours variable
#sleeping hours correction with +24 h if negative values
#includeds night sleep + nap (if applicable)
surveydata <- surveydata %>%
  mutate(sleeping_hours = wakeup_time - sleep_time,
         sleeping_hours = ifelse(sleeping_hours < 0, sleeping_hours+24, sleeping_hours),
         sleeping_hours = sleeping_hours + (nap_today/60)
         )


# Time awake
# Recode "begin"  (start time of the experiment) time into numerical variable
#and transform into hours (/60)
surveydata$date <- date(surveydata$begin)
surveydata$begin <- hour(surveydata$begin) + minute(surveydata$begin)/60

# Create time awake variable
#from experiment start time and wake-up time
surveydata <- surveydata %>%
  mutate(time_awake = begin - wakeup_time)


#create age group variable
surveydata <- surveydata %>%
  mutate(
    # Create categories
    age_group = case_when(
      age > 17 & age <= 24~ "18-24",
      age > 24 & age <= 34 ~ "25-34",
      age > 34 & age <= 44 ~ "35-44",
      age > 44 & age <= 54 ~ "45-54",
      age > 54 & age <= 64 ~ "55-64",
      age > 64             ~ ">64"
    ),
    # Convert age group to a factor
    age_group = factor(
      age_group,
      level = c("18-24", "25-34","35-44","45-54","55-64", ">64")
    )
  )
#create season variable from participation date
#spring was defined as March-May
#summer as June-August
#autumn as September-November
#winter as December-February
surveydata %>%
  mutate(season = case_when(date >= "2021-03-01" & date < "2021-06-01" ~ 'Spring',
                            date >= "2021-06-01" & date < "2021-09-01" ~ 'Summer',
                            date >= "2021-09-01" & date < "2021-12-01" ~ 'Autumn',
                            date >= "2021-12-01" & date < "2022-03-01" ~ 'Winter',
                            date >= "2022-03-01" & date < "2022-06-01" ~ 'Spring',
                            date >= "2022-06-01" & date < "2022-09-01" ~ 'Summer',
                            date >= "2022-09-01" & date < "2022-12-01" ~ 'Autumn',
                            date >= "2022-12-01" & date < "2023-03-01" ~ 'Winter'
  ),
  # Convert season to factor
  season = factor(
    season,
    level = c("Spring", "Summer","Autumn","Winter")
  )
  ) -> surveydata



### [4] Data mistake corrections --------------------------------------------------------  
#correcting manual mistakes made in soscisurvey 
#for more info see "notes.txt"  files of each subject's raw data
#notes were taken by the experimentors (in German)

#correcting spelling of the ID
surveydata$id[surveydata$id =="SPO34"] <- "SP034"
surveydata$id[surveydata$id =="SP080\n"] <- "SP080"
surveydata$id[surveydata$id =="SP090bb"] <- "SP090"
surveydata$id[surveydata$id =="Sp105"] <- "SP105"
surveydata$id[surveydata$id =="SPO72"] <- "SP072"
surveydata$id[surveydata$id =="111"] <- "SP111"

# SP002 still participated in the trial as a pilot, but the data can not be used
# hence we delete the demographic data as well.
surveydata[surveydata$id =="SP002",c("kss_pre", "kss_post", "weather",
                                     "habitual_sum", "acute_sum",
                                     "SDweek", "sleep_time", "sleeping_hours", 
                                     "time_awake", "MSFsc", "MSF", 
                                     "iris_colour") ] <- NA

#Correcting mistakes in the data entry

# Weather, visual screening and drug testing were not entered into 
#the survey in all cases --> correction below

#Check Missing values for HRR test
#id "SP075" "SP079" were excluded before the colour test
surveydata$id[is.na(surveydata$colour_perception)]

#correct missing / wrong values
#SP001 has PASSED the colour vision test --> wrongly entered
surveydata$colour_perception[surveydata$id =="SP001"] <- "yes"
surveydata$colour_perception[surveydata$id =="SP016"] <- "no"
surveydata$colour_perception[surveydata$id =="SP042"] <- "no"
surveydata$colour_perception[surveydata$id =="SP075"] <- "no"
surveydata$colour_perception[surveydata$id =="SP078"] <- "no"
surveydata$colour_perception[surveydata$id =="SP107"] <- "yes"
#remaining missing values HRR
surveydata$id[is.na(surveydata$colour_perception)]

#Check Missing values for visual acuity test
#id "SP016" "SP042" "SP078" "SP079" "SP107" were excluded before the visual acuity test
surveydata$id[is.na(surveydata$visual_acuity_snellen)]

#correct missing / wrong values
surveydata$visual_acuity_snellen[surveydata$id =="SP075"] <- "20/200"
surveydata$visual_acuity_snellen[surveydata$id =="SP107"] <- "20/40"

#Check Missing values for text reading test
#id "SP016" "SP042" "SP078" "SP079" were excluded before the visual acuity test
surveydata$id[is.na(surveydata$text_reading)]

#correct missing / wrong values
surveydata$text_reading[surveydata$id =="SP075"] <- "no"
surveydata$text_reading[surveydata$id =="SP107"] <- "yes"

#Check Missing values for text reading test
surveydata$id[is.na(surveydata$text_reading)]
#id "SP016" "SP042" "SP075" "SP078" "SP079" were excluded before the visual acuity test

#check missing data for the drug test data
surveydata$id[is.na(surveydata$drug_test)]

#add missing value for drug test of SP107
surveydata$drug_test[surveydata$id =="SP107"] <- "positive"

#Check Missing values for iris colour
#id "SP016" "SP042" "SP075" "SP078" "SP079" were excluded before the checking iris colour
surveydata$id[is.na(surveydata$iris_colour)]

#checking visual aid for missing values
#coding of the factors initially contained  a mistake - causing NA values! - now fixed
surveydata$id[is.na(surveydata$visual_aid)]
levels(surveydata$visual_aid)

#Check weather for missing values
#coding of the factors initially contained  a mistake - causing NA values! - now fixed
surveydata$id[is.na(surveydata$weather)]
levels(surveydata$weather)


#adding the Laboratory light condition sequence (counterbalanced) as a variable
surveydata$Lablight_seq <-c("RGBW",NA,"RBWG","RWGB","RWBG","GRBW","GRWB","GBRW","GBWR",
                 "GWRB","GWBR","BRGW","BRWG","BGRW","BGRW","BWRG","BGWR","BWRG",
                 "BWGR","WRGB","WRBG","WGRB","WGBR","WBRG","WBGR","RGBW","RGWB",
                 "RBGW","RBWG","RWGB","RWBG","GRBW","GRWB","GBRW","GBWR","GWRB",
                 "GWBR","BRGW","BRWG","BGRW","BGWR",NA,"BWRG","BWGR","WRGB",
                 "WRBG","WGRB","WGBR","WBRG","WBGR","RGBW","RGWB","RBGW","RBWG",
                 "RWGB","RWBG","GRBW","GRWB","GBRW","GBWR","GWRB","GWBR","GWBR",
                 "BRGW","BRWG","BGRW","BGWR","BWRG","BWGR","WRGB","WRBG","WGRB",
                 "WGBR","WBRG",NA,"WBGR","RGBW",NA,NA,"RGWB","RBGW","RBWG",
                 "RBWG","RWGB","RWBG","GRBW","GRWB","GBRW","GBWR","GWRB","GWBR",
                 "BRGW","BRWG","BGRW","BGWR","BWRG","BWGR","WRGB","WRBG","WGRB",
                 "WGBR","WBRG","WBGR","RGBW","RGWB","RBGW",NA,"RBWG","RWGB",
                 "RWBG","GRBW","GRBW","GRWB")

### [5] Data saving: clean dataset ---------------------------------------------------
save(surveydata, file="./01_surveydata/cleaned_survey.rda")
