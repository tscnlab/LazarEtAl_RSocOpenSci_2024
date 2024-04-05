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


merged_data_dem <- merged_data_all %>% select(
  id, excl, excl_time, overthrs75,overthrs50,  date, begin, age, age_group,
                     sex, handedness, visual_aid, contact_lenses, 
                     visual_acuity_snellen, BMI, MSFsc, time_awake, sleeping_hours,
                     SDweek, acute_sum, acute_sum_rel, habitual_sum_rel, 
                     habitual_sum, iris_colour, weather, kss_pre, kss_post,
                     time_awake, season
  )
  
  
  
# Save full merged data set 
save(merged_data_dem, file="./03_datamerge/mergeddata_dem.rda")

##### select data according to 75% data loss threshold ---------------------------------

# sort out excluded data
#create a dataframe containing only the included data. Here we employ 
#the 75 % data loss threshold

merged_data_incl75 <- merged_data_all[merged_data_all$excl == "results" & 
                                        merged_data_all$overthrs75==FALSE, ]


merged_data_incl <- merged_data_incl75

###  load corrected light data (correct interpolation in spectra) -------------


#load file derived from correctly interpolated spectral data
load(file="./03_datamerge/merged_calc.rda")

str(merged_calc)


sum(is.na(merged_calc$`Illuminance (lx)`))

#Compare the light data (uncorrected vs corrected) and quantify deviation in percent :----

comp <- merged_data_incl75 %>% select (id, sample_nr, exp_phase, phot_lux, Mel_EDI ) 
comp <- cbind(comp, merged_calc$`Illuminance (lx)`, merged_calc$`Melanopic EDI (lx)` )
comp <- comp %>% rename(
  
  illum = `merged_calc$\`Illuminance (lx)\``,
  mEDI = `merged_calc$\`Melanopic EDI (lx)\``
  
  
)

comp <- comp %>% mutate (
  
  dif_illum = illum - phot_lux,
  dif_medi = mEDI - Mel_EDI,
  dif_illumperc = (dif_illum / illum )*100,
  dif_mediperc = (dif_medi/ mEDI)*100
)


# Compute summary statistics for diagnosis
summary_stats <- function(column) {
  c(min = min(column, na.rm = TRUE),
    max = max(column, na.rm = TRUE),
    mean = mean(column, na.rm = TRUE),  # Add na.rm = TRUE to handle NA values
    median = median(column, na.rm = TRUE),  # Add na.rm = TRUE to handle NA values
    sd = sd(column, na.rm = TRUE))  # Add na.rm = TRUE to handle NA values
}

# Compute confidence interval
compute_confidence_interval <- function(column) {
  mean_val <- mean(column, na.rm = TRUE)  # Add na.rm = TRUE to handle NA values
  sd_val <- sd(column, na.rm = TRUE)  # Add na.rm = TRUE to handle NA values
  n <- sum(!is.na(column))  # Calculate the number of non-NA values
  t_val <- qt(0.975, df = n - 1)  # 95% confidence interval
  se <- sd_val / sqrt(n)
  ci <- mean_val + c(-1, 1) * t_val * se
  ci
}

# Apply functions to the desired columns
summary_stats_dif_illumperc <- summary_stats(comp$dif_illumperc)
ci_dif_illumperc <- compute_confidence_interval(comp$dif_illumperc)

summary_stats_dif_mediperc <- summary_stats(comp$dif_mediperc)
ci_dif_mediperc <- compute_confidence_interval(comp$dif_mediperc)

# Combine results into a dataframe
results <- data.frame(
  dif_illumperc = c(summary_stats_dif_illumperc, ci_dif_illumperc),
  dif_mediperc = c(summary_stats_dif_mediperc, ci_dif_mediperc)
)

# Adding row names for better understanding
rownames(results) <- c("min", "max", "mean", "median", "sd", "lower_ci", "upper_ci")

# look at summary results assess maginitues of deviation
print(results)


#The general deviation between corrected and uncorrected is ~ +4%

#now check the (percent deviation) outliers:


# Find row index where the maximum differences occur for dif_illumperc
max_diff_illumperc_index <- which(comp$dif_illumperc == results["max", "dif_illumperc"])

# Find row index where the maximum differences occur for dif_mediperc
max_diff_mediperc_index <- which(comp$dif_mediperc == results["max", "dif_mediperc"])

# Display rows where the maximum differences occur
max_diff_illumperc_rows <- comp[max_diff_illumperc_index, ]
max_diff_mediperc_rows <- comp[max_diff_mediperc_index, ]

print("Rows with maximum difference in dif_illumperc:")
print(max_diff_illumperc_rows)

print("Rows with maximum difference in dif_mediperc:")
print(max_diff_mediperc_rows)

# Conclusion: Maximum deviation for both illuminance (9.8 %) and mEDI (7.5%) 
#were both in very dim values near 1


# Find row index where the minimum differences occur for dif_illumperc
min_diff_illumperc_index <- which(comp$dif_illumperc == results["min", "dif_illumperc"])

# Find row index where the minimum differences occur for dif_mediperc
min_diff_mediperc_index <- which(comp$dif_mediperc == results["min", "dif_mediperc"])

# Display rows where the minimum differences occur
min_diff_illumperc_rows <- comp[min_diff_illumperc_index, ]
min_diff_mediperc_rows <- comp[min_diff_mediperc_index, ]

print("Rows with minimum difference in dif_illumperc:")
print(min_diff_illumperc_rows)

print("Rows with minimum difference in dif_mediperc:")
print(min_diff_mediperc_rows)

# Conclusion: Minimum deviation for both illuminance (9.8 %) and mEDI (7.5%) 
#were both in very dim values near 1 and 2


#clean up environment:

rm(comp, max_diff_illumperc_rows, max_diff_mediperc_rows,
     min_diff_illumperc_rows, min_diff_mediperc_rows, results)
  
  


#replacing light data from before with corrected data  ---------------------------


# Replace photopic illuminance values with corrected spectral values (correct interpolation)
merged_data_incl$phot_lux <- ifelse(!is.na(merged_data_incl$phot_lux), 
                                    merged_calc$`Illuminance (lx)`,
                                    NA)

# Replace Scone irrad values with corrected spectral values (correct interpolation)
merged_data_incl$SConeIrrad <- ifelse(!is.na(merged_data_incl$SConeIrrad), 
                                   merged_calc$`S-cone-opic irradiance (mW ⋅ m⁻²)`,
                                   NA)

# Replace Mcone irrad values with corrected spectral values (correct interpolation)
merged_data_incl$MConeIrrad <- ifelse(!is.na(merged_data_incl$MConeIrrad), 
                                      merged_calc$`M-cone-opic irradiance (mW ⋅ m⁻²)`,
                                      NA)

# Replace Lcone irrad values with corrected spectral values (correct interpolation)
merged_data_incl$LConeIrrad <- ifelse(!is.na(merged_data_incl$LConeIrrad), 
                                      merged_calc$`L-cone-opic irradiance (mW ⋅ m⁻²)`,
                                      NA)

# Replace Rhodopic irrad values with corrected spectral values (correct interpolation)
merged_data_incl$RodIrrad <- ifelse(!is.na(merged_data_incl$RodIrrad), 
                                      merged_calc$`Rhodopic irradiance (mW ⋅ m⁻²)`,
                                    NA)

# Replace melanopic irradiance values with corrected spectral values (correct interpolation)
merged_data_incl$MelIrrad <- ifelse(!is.na(merged_data_incl$MelIrrad), 
                                   merged_calc$`Melanopic irradiance (mW ⋅ m⁻²)`,
                                   NA)

# Replace SCone EDI values with corrected spectral values (correct interpolation)
merged_data_incl$SCone_EDI <- ifelse(!is.na(merged_data_incl$SCone_EDI), 
                                   merged_calc$`S-cone-opic EDI (lx)`,
                                   NA)

# Replace MCone EDI values with corrected spectral values (correct interpolation)
merged_data_incl$MCone_EDI <- ifelse(!is.na(merged_data_incl$MCone_EDI), 
                                     merged_calc$`M-cone-opic EDI (lx)`,
                                     NA)

# Replace mEDI values with corrected spectral values (correct interpolation)
merged_data_incl$LCone_EDI <- ifelse(!is.na(merged_data_incl$LCone_EDI), 
                                     merged_calc$`L-cone-opic EDI (lx)`,
                                     NA)

# Replace mEDI values with corrected spectral values (correct interpolation)
merged_data_incl$Rod_EDI <- ifelse(!is.na(merged_data_incl$Rod_EDI), 
                                     merged_calc$`Rhodopic EDI (lx)`,
                                   NA)

# Replace colour coordinate  values with corrected spectral values (correct interpolation)
merged_data_incl$Mel_EDI <- ifelse(!is.na(merged_data_incl$Mel_EDI), 
                                   merged_calc$`Melanopic EDI (lx)`,
                                   NA)


# Replace colour coordinate values with corrected spectral values (correct interpolation)
merged_data_incl$`CIE 1931 x` <- ifelse(!is.na(merged_data_incl$`CIE 1931 x`), 
                                   merged_calc$`CIE 1931 xy chromaticity [x]`,
                                   NA)

merged_data_incl$`CIE 1931 y` <- ifelse(!is.na(merged_data_incl$`CIE 1931 y`), 
                                        merged_calc$`CIE 1931 xy chromaticity [y]`,
                                        NA)


# log transformed values:

str(merged_data_incl)

merged_data_incl <- merged_data_incl %>%
  mutate(`log_MelIrrad` = log10(MelIrrad),
         `log_phot_lux` = log10(phot_lux),
         `log_Mel_EDI` = log10(Mel_EDI),
         `log_SCone_EDI` = log10(SCone_EDI),
         `log_MCone_EDI` = log10(MCone_EDI),
         `log_LCone_EDI` = log10(LCone_EDI),
         `log_Rod_EDI` = log10(Rod_EDI),
          MPratio = MelIrrad/phot_lux
  )

#after the filling in the correction, there is the same amount 
#of NA values (Data from the "dark" phase + saturated spectra were set to NA
#in the scripts before)

sum(is.na(merged_data_incl$phot_lux))

sum(is.na(merged_data_incl75$phot_lux))


##### select data according to 50% data loss threshold ---------------------------------

#create a dataframe containing only data below the initial 50% data loss threshold

merged_data_incl50 <- merged_data_incl[merged_data_incl$excl == "results" & 
                                         merged_data_incl$overthrs50==FALSE, ]

#If you want to employ the initial data loss threshold (0.5; n=63 included) to the following
#caluclations and scripts:
#UNCOMMENT THE FOLLOWING CODELINE

#merged_data_incl <- merged_data_incl50



### Save dataset -------------


# select variables for confirmatory & exploratory analysis
#and save it as a new dataset
merged_data_conf <- select(
  merged_data_incl, id, excl, overthrs75, overthrs50, date, begin, sample_nr, 
  diameter_3d, confidence, phot_lux, MelIrrad, Mel_EDI, age, 
  sex, exp_phase, data_loss, age_group, weather, season,
  MSFsc, time_awake, sleeping_hours, SDweek, iris_colour, #added vars
  kss_pre, kss_post,#added vars
  acute_sum, acute_sum_rel, habitual_sum_rel, #added vars
  log_MelIrrad, log_phot_lux, log_Mel_EDI, MPratio,
  SConeIrrad, MConeIrrad, LConeIrrad, RodIrrad,#added 2024-01-24
  SCone_EDI, MCone_EDI, LCone_EDI, Rod_EDI,#added 2024-02-02
  log_SCone_EDI, log_MCone_EDI, log_LCone_EDI, log_Rod_EDI#added 2024-02-02
)




#save this as new dataset
save(merged_data_conf, file="./03_datamerge/merged_data_conf.rda")





