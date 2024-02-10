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

#Libraries
library(BayesFactor)
library(lme4)
library(lmerTest)
library(tidyverse)

#set seed function to initialize the pseudorandom number generator
#at the same set point (Mersenne-Twister generator)
#command needs to be run before every simulation
set.seed(20230703) 


#confirmatory hypotheses in text:

# CH1: In real-world conditions, light-adapted pupil size changes
#as a function of melanopsin sensitivity-weighted retinal illumination
#with higher illumination associated with a smaller pupil size.
#
# CH2: In real-world conditions, melanopsin sensitivity-weighted retinal
#illumination better predicts pupil size than the weighted sum of L- and M-cone
#weighted retinal illumination (CIE 1931 Y, photopic illuminance).
#
# CH3: In real-world conditions, light-adapted pupil size changes as a function
#of age, with higher age associated with a smaller pupil size.


###[1] Loading subdatasets------------------------------------------------------

load(file="./05_analysis/subdata/conf_subdata.rda")


###[2] Testing log10 transformation---------------------------------------------------

# comparing model fit log10 vs. linear m-EDI
#computing BF10: likelihood ratio of full model vs. null model
# including Field data only


#full model (numerator) log10-transformed
set.seed(20230703) 
CH1blog_numerator<- lmBF(diameter_3d ~ log_Mel_EDI + age + id  + sex,
                         data = Fielddata, 
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)
#null model (denominator) linear (no transformation)
set.seed(20230703) 
CH1blog_denominator <- lmBF(diameter_3d ~ Mel_EDI + age + id + sex, 
                            data = Fielddata, 
                            whichRandom = c("id","sex"),
                            progress = FALSE, 
                            posterior = TRUE,
                            iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
BF_CH1blog <-compare(CH1blog_numerator, CH1blog_denominator)
#saving the results in a text file
sink(file = "06_output/stat/BF_field_logvsnolog.txt")
summary(BF_CH1blog)
#extract ln of the BF
"ln(BF):"
extractBF(BF_CH1blog, logbf = T, onlybf = T) 

#convert to log10
"log10(BF):"
 extractBF(BF_CH1blog, logbf = T, onlybf = T) / log(10)
 
sink()



###[3] Positive control tests---------------------------------------------------


# Positive control: CH1 in Lab data

#computing BF10: likelihood ratio of full model vs. null model
# including Laboratory data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
CH1a_numerator<- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex,
                      data = Labdata, 
                      whichRandom = c("id","sex"),
                      progress = FALSE, 
                      posterior = TRUE,
                      iterations = 10000
)
#null model (denominator) 
set.seed(20230703) 
CH1a_denominator <- lmBF(diameter_3d ~ age + id + sex, 
                         data = Labdata, 
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
BF_CH1a <- compare(CH1a_numerator, CH1a_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_CH1_lab.txt")
summary(BF_CH1a)
#extract ln of the BF
"ln(BF):"
extractBF(BF_CH1a, logbf = T, onlybf = T)

#convert to log10
"log10(BF):"
extractBF(BF_CH1a, logbf = T, onlybf = T) / log(10)

sink()

# Positive control: CH1 in Lab data

#computing BF10: likelihood ratio of full model vs. null model
# including Laboratory data only
#linear (non-transformed) light data

#full model (numerator)
set.seed(20230703) 
CH1ax_numerator<- lmBF(diameter_3d ~ Mel_EDI + age + id + sex,
                       data = Labdata, 
                       whichRandom = c("id","sex"),
                       progress = FALSE, 
                       posterior = TRUE,
                       iterations = 10000
)
#null model (denominator) 
set.seed(20230703) 
CH1ax_denominator <- lmBF(diameter_3d ~ age + id + sex, 
                          data = Labdata, 
                          whichRandom = c("id","sex"),
                          progress = FALSE, 
                          posterior = TRUE,
                          iterations = 10000
)

#comparison of full div. by null model

set.seed(20230703) 
BF_CH1ax <- compare(CH1ax_numerator, CH1ax_denominator)
#saving the results in a text file
sink(file = "06_output/stat/BF_CH1_lab_nolog.txt")
BF_CH1ax
#extract ln of the BF
"ln(BF):"
extractBF(BF_CH1ax, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH1ax, logbf = T, onlybf = T)/ log(10)
sink()


# Positive control: CH2 in Lab data

#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
CH2a_numerator <- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex, 
                       data = Labdata, 
                       whichRandom = c("id","sex"), 
                       progress = FALSE, 
                       posterior = TRUE,
                       iterations = 10000
)

#null model (denominator) 
set.seed(20230703) 
CH2a_denominator <- lmBF(diameter_3d ~ log_phot_lux + age + id + sex, 
                         data = Labdata,
                         whichRandom = c("id","sex"), 
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
BF_CH2_lab <- compare(CH2a_numerator, CH2a_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_CH2_lab.txt")
summary(BF_CH2_lab)
#extract ln of the BF
"ln(BF):"
extractBF(BF_CH2_lab, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH2_lab, logbf = T, onlybf = T)/ log(10)
sink()


# Positive control: CH2 in Lab data

#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#linear (non-transformed) light data

#full model (numerator)
set.seed(20230703) 
CH2ax_numerator <- lmBF(diameter_3d ~ Mel_EDI + age + id + sex, 
                        data = Labdata, 
                        whichRandom = c("id","sex"), 
                        progress = FALSE, 
                        posterior = TRUE,
                        iterations = 10000
)
#null model (denominator) 
set.seed(20230703) 
CH2ax_denominator <- lmBF(diameter_3d ~ phot_lux + age + id + sex, 
                          data = Labdata,
                          whichRandom = c("id","sex"), 
                          progress = FALSE, 
                          posterior = TRUE,
                          iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
BF_CH2x_lab <- compare(CH2ax_numerator, CH2ax_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_CH2_lab_nolog.txt")
summary(BF_CH2x_lab)
#extract ln of the BF
"ln(BF):"
extractBF(BF_CH2x_lab, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH2x_lab, logbf = T, onlybf = T)/ log(10)
sink()


# Positive control: CH3 in Dark data


#computing BF10: likelihood ratio of full model vs. null model
# including Dark adapation data only
#linear (non-transformed) light data

#full model (numerator)
set.seed(20230703) 
CH3c_numerator<- lmBF(diameter_3d ~  age + id + sex, data = Darkdata, 
                      whichRandom = c("id","sex"),
                      progress = FALSE, 
                      posterior = TRUE,
                      iterations = 10000
)

#null model (denominator) 
set.seed(20230703) 
CH3c_denominator <- lmBF(diameter_3d ~  id + sex, data = Darkdata, 
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
BF_CH3c <-  compare(CH3c_numerator, CH3c_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_CH3_dark.txt")
summary(BF_CH3c)
#extract ln of the BF
"ln(BF):"
extractBF(BF_CH3c, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH3c, logbf = T, onlybf = T)/ log(10)
sink()



### [4] Hypothesis testing - CH1----------------------------------------------------
# CH1: In real-world conditions, light-adapted pupil size changes 
#as a function of melanopsin sensitivity-weighted retinal illumination
#with higher illumination associated with a smaller pupil size. 


#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
CH1b_numerator<- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex,
                      data = Fielddata, 
                      whichRandom = c("id","sex"),
                      progress = FALSE, 
                      posterior = TRUE,
                      iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
CH1b_denominator <- lmBF(diameter_3d ~ age + id + sex, 
                         data = Fielddata, 
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
BF_CH1b <- compare(CH1b_numerator, CH1b_denominator)
#saving the results in a text file
sink(file = "06_output/stat/BF_CH1_field.txt")
summary(BF_CH1b)
#extract ln of the BF
"ln(BF):"
extractBF(BF_CH1b, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH1b, logbf = T, onlybf = T)/ log(10)
sink()

#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#linear  light data (no log10 transformation)

#full model (numerator)
set.seed(20230703) 
CH1bx_numerator<- lmBF(diameter_3d ~ Mel_EDI + age + id + sex,
                      data = Fielddata, 
                      whichRandom = c("id","sex"),
                      progress = FALSE, 
                      posterior = TRUE,
                      iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
CH1bx_denominator <- lmBF(diameter_3d ~ age + id + sex, 
                         data = Fielddata, 
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
BF_CH1bx <-compare(CH1bx_numerator, CH1bx_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_CH1_field_nolog.txt")
summary(BF_CH1bx)
"ln(BF):"
extractBF(BF_CH1bx, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH1bx, logbf = T, onlybf = T)/ log(10)
sink()

### [5] Hypothesis testing - CH2----------------------------------------------------
# CH2: In real-world conditions, melanopsin sensitivity-weighted retinal 
#illumination better predicts pupil size than the weighted sum of L- and M-cone
#weighted retinal illumination (CIE 1931 Y, photopic illuminance). 
#

#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
CH2b_numerator <- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex, 
                       data = Fielddata, 
                       whichRandom = c("id","sex"),
                       progress = FALSE, 
                       posterior = TRUE,
                       iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
CH2b_denominator <- lmBF(diameter_3d ~ log_phot_lux + age + id + sex, 
                         data = Fielddata,
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)
#comparison of full div. by null model
set.seed(20230703) 
BF_CH2b <- compare(CH2b_numerator, CH2b_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_CH2_field.txt")
summary(BF_CH2b)
"ln(BF):"
extractBF(BF_CH2b, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH2b, logbf = T, onlybf = T)/ log(10)
sink()


#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#linear  light data (no log10 transformation)

#full model (numerator)
set.seed(20230703) 
CH2bx_numerator <- lmBF(diameter_3d ~ Mel_EDI + age + id + sex, 
                       data = Fielddata, 
                       whichRandom = c("id","sex"), 
                       progress = FALSE, 
                       posterior = TRUE,
                       iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
CH2bx_denominator <- lmBF(diameter_3d ~ phot_lux + age + id + sex, 
                         data = Fielddata,
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)
#comparison of full div. by null model
set.seed(20230703) 
BF_CH2bx <- compare(CH2bx_numerator, CH2bx_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_CH2_field_nolog.txt")
summary(BF_CH2bx)
"ln(BF):"
extractBF(BF_CH2bx, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH2bx, logbf = T, onlybf = T)/ log(10)
sink()

# Testing the models against each other with BICs from the lm4e package
#these tests are only used as a double check to 
#"validate" our procedure with the BayesFactor package 

#formulate models with lmerTest package
set.seed(20230703) 
(CH2_NHST_H1<-lmer(diameter_3d ~ log_Mel_EDI + age + (1|id) + (1|sex), Fielddata))
set.seed(20230703) 
(CH2_NHST_H0<-lmer(diameter_3d ~ log_phot_lux + age + (1|id) + (1|sex), Fielddata))

#compute a model comparison with Bayesian information criterion
# the lower the BIC, the better the model fit
set.seed(20230703) 
BIC_CH2<- BIC(CH2_NHST_H0,CH2_NHST_H1)
#BIC is lower with log10(m-EDI) than with log10(photopic lux)

#save generated mixed linear model for H1 to text file 
sink(file = "06_output/stat/lmer_age_light_model.txt")
CH2_NHST_H1
sink()


#Approximating Bayes factors from BICs using the equation by Wagenmakers (2007)
#https://doi.org/10.3758/BF03194105, retrieved from Stevens (2019) https://osf.io/eszbd

#function for converting BICs into BFs (retrieved from Stevens (2019))
bic_bf10 <- function(null, alternative) {
  new_bf <- exp((null - alternative) / 2) # convert BICs to Bayes factor
  names(new_bf) <- NULL # remove BIC label
  return(new_bf) # return Bayes factor of alternative over null hypothesis
}

#compute BIC for each model of CH2 defined earlier
set.seed(20230703) 
CH2_H1_bic<- BIC(CH2_NHST_H1)
set.seed(20230703) 
CH2_H0_bic<- BIC(CH2_NHST_H0)

set.seed(20230703) 
#convert the BICs into an approximated BF10 factor
bic_bf10(CH2_H0_bic,CH2_H1_bic)
#ln of approx. BF10 factor
log(bic_bf10(CH2_H0_bic,CH2_H1_bic))
#log10 of approx. BF10 factor
log10(bic_bf10(CH2_H0_bic,CH2_H1_bic))

#result very similar to the BF10 computed with BayesFactor package.

#save BICs & BF10 approximation to text file
sink(file = "06_output/stat/CH2_field_BIC.txt")

paste0("H0:", formula(CH2_NHST_H0), collapse = " ")
paste0("H1:", formula(CH2_NHST_H1), collapse = " ")

BIC_CH2

paste0("from BICs approximated BF10:", bic_bf10(CH2_H0_bic,CH2_H1_bic), collapse = " ")


"ln(approx. BF10)"
log(bic_bf10(CH2_H0_bic,CH2_H1_bic))
"log10(approx. BF10)"
log10(bic_bf10(CH2_H0_bic,CH2_H1_bic))

sink()


# Repeating the BIC test using the non-log10 transformed light intensity vars
set.seed(20230703) 
(CH2_NHST_nolog_H1<-lmer(diameter_3d ~ Mel_EDI + age  + (1|id) + (1|sex), Fielddata))
set.seed(20230703) 
(CH2_NHST_nolog_H0<-lmer(diameter_3d ~ phot_lux + age + (1|id) + (1|sex), Fielddata))
set.seed(20230703) 

BIC(CH2_NHST_nolog_H0, CH2_NHST_nolog_H1)

#the model with mEDI (H1) has a lower BIC and is preferred.


#compute BIC for each model of CH2 without log10 transformation
set.seed(20230703) 
CH2_H1_nolog_bic<- BIC(CH2_NHST_nolog_H1)
set.seed(20230703) 
CH2_H0_nolog_bic<- BIC(CH2_NHST_nolog_H0)

set.seed(20230703) 
#convert the BICs into an approximated BF10 factor
bic_bf10(CH2_H0_nolog_bic,CH2_H1_nolog_bic)
#ln of approx. BF10 factor
log(bic_bf10(CH2_H0_nolog_bic,CH2_H1_nolog_bic))
#log10 of approx. BF10 factor
log10(bic_bf10(CH2_H0_nolog_bic,CH2_H1_nolog_bic))




#results very similar to the BF10 computed with BayesFactor package.



### [6] Hypothesis testing - CH3-------------------------------------------------------------------------
# CH3: In real-world conditions, light-adapted pupil size changes as a function
#of age, with higher age associated with a smaller pupil size. 


#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
CH3b_numerator<- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex, data = Fielddata, 
                      whichRandom = c("id","sex"),
                      progress = FALSE, 
                      posterior = TRUE,
                      iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
CH3b_denominator <- lmBF(diameter_3d ~ log_Mel_EDI + id + sex, data = Fielddata, 
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)
#comparison of full div. by null model
set.seed(20230703) 
BF_CH3b <-  compare(CH3b_numerator, CH3b_denominator)


#saving the results in a text file
sink(file = "06_output/stat/BF_CH3_field.txt")
summary(BF_CH3b)
"ln(BF):"
extractBF(BF_CH3b, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH3b, logbf = T, onlybf = T)/ log(10)
sink()


#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#linear  light data (no log10 transformation)

#full model (numerator)
set.seed(20230703) 
CH3bx_numerator<- lmBF(diameter_3d ~ Mel_EDI + age + id + sex, data = Fielddata, 
                      whichRandom = c("id","sex"),
                      progress = FALSE, 
                      posterior = TRUE,
                      iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
CH3bx_denominator <- lmBF(diameter_3d ~ Mel_EDI + id + sex, data = Fielddata, 
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)
#comparison of full div. by null model
set.seed(20230703) 
BF_CH3b_nolog <-  compare(CH3bx_numerator, CH3bx_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_CH3_field_nolog.txt")
summary(BF_CH3b_nolog)
"ln(BF):"
extractBF(BF_CH3b_nolog, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(BF_CH3b_nolog, logbf = T, onlybf = T)/ log(10)
sink()




### [7] Exploratory analyses - EH-----------------------------------------------

# SEX differences 


#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
sexdif_field_numerator<- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex, 
                              data = Fielddata, 
                              whichRandom = c("id","sex"),
                              progress = FALSE, 
                              posterior = TRUE,
                              iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
sexdif_field_denominator <- lmBF(diameter_3d ~ log_Mel_EDI + age + id,
                                 data = Fielddata, 
                                 whichRandom = c("id","sex"),  
                                 progress = FALSE, 
                                 posterior = TRUE,
                                 iterations = 10000
)
#comparison of full div. by null model
set.seed(20230703) 
sexdif_field <- compare(sexdif_field_numerator, sexdif_field_denominator)
#saving the results in a text file
sink(file = "06_output/stat/BF_sexdif_field.txt")
summary(sexdif_field)
"ln(BF):"
extractBF(sexdif_field, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(sexdif_field, logbf = T, onlybf = T)/ log(10)
sink()
# strong evidence for the null model



#Caffeine consumption-

#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
caf_field_numerator<- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex + acute_sum_rel,
                           data = Fielddata, 
                     whichRandom = c("id","sex"),
                     progress = FALSE, 
                     posterior = TRUE,
                     iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
caf_field_denominator <- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex, 
                              data = Fielddata, 
                              whichRandom = c("id","sex"),
                              progress = FALSE, 
                              posterior = TRUE,
                              iterations = 10000
)

#comparison of full div. by null model

set.seed(20230703) 
caf_field <- compare(caf_field_numerator, caf_field_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_acutecaf_field.txt")
summary(caf_field)
"ln(BF):"
extractBF(caf_field, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(caf_field, logbf = T, onlybf = T)/ log(10)
sink()

# --> moderate evidence for null model 


#habitual caffeine in the field data


#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
hcaf_field_numerator<- lmBF(diameter_3d ~ log_Mel_EDI + age  + id + sex + habitual_sum_rel, 
                            data = Fielddata, 
                          whichRandom = c("id","sex"),
                          progress = FALSE, 
                          posterior = TRUE,
                          iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
hcaf_field_denominator <- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex, 
                               data = Fielddata, 
                               whichRandom = c("id","sex"),
                             progress = FALSE, 
                             posterior = TRUE,
                             iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
hcaf_field<- compare(hcaf_field_numerator, hcaf_field_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_habitualcaf_field.txt")
summary(hcaf_field)
"ln(BF):"
extractBF(hcaf_field, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(hcaf_field, logbf = T, onlybf = T)/ log(10)
sink()

# moderate evidence for null model 


# iris colour 

#computing BF10: likelihood ratio of full model vs. null model
# including Field data only
#log10-transformed light data

#full model (numerator)
set.seed(20230703) 
iris_field_numerator<- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex + iris_colour, 
                            data = Fielddata, 
                          whichRandom = c("id","sex", "iris_colour"),
                          progress = FALSE, 
                          posterior = TRUE,
                          iterations = 10000
)
#null model (denominator)
set.seed(20230703) 
iris_field_denominator <- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex,
                               data = Fielddata, 
                               whichRandom = c("id","sex", "iris_colour"),
                             progress = FALSE, 
                             posterior = TRUE,
                             iterations = 10000
)

#comparison of full div. by null model
set.seed(20230703) 
iris_field <- compare(iris_field_numerator, iris_field_denominator)

#saving the results in a text file
sink(file = "06_output/stat/BF_iris_field.txt")
summary(iris_field)
"ln(BF):"
extractBF(iris_field, logbf = T, onlybf = T)
#convert into log10
"log10(BF):"
extractBF(iris_field, logbf = T, onlybf = T)/ log(10)
sink()
# strong evidence for null model 

### [8] Exploratory analyses without hypotheses------------------------------


#testing other alpha-opic values & illuminance as predictors for pupil size 

#define the different alpha-opic values:

# mEDI
set.seed(20230703) 
CH2b_mEDI <- lmBF(diameter_3d ~ log_Mel_EDI + age + id + sex, 
                       data = Fielddata, 
                       whichRandom = c("id","sex"),
                       progress = FALSE, 
                       posterior = TRUE,
                       iterations = 10000
)

#Scone
set.seed(20230703) 
CH2b_Scone <- lmBF(diameter_3d ~ log_SCone_EDI + age + id + sex, 
                         data = Fielddata,
                         whichRandom = c("id","sex"),
                         progress = FALSE, 
                         posterior = TRUE,
                         iterations = 10000
)


#Mcone
set.seed(20230703) 
CH2b_Mcone <- lmBF(diameter_3d ~ log_MCone_EDI + age + id + sex, 
                   data = Fielddata,
                   whichRandom = c("id","sex"),
                   progress = FALSE, 
                   posterior = TRUE,
                   iterations = 10000
)


#Lcone
set.seed(20230703) 
CH2b_Lcone <- lmBF(diameter_3d ~ log_LCone_EDI + age + id + sex, 
                   data = Fielddata,
                   whichRandom = c("id","sex"),
                   progress = FALSE, 
                   posterior = TRUE,
                   iterations = 10000
)
#rod
set.seed(20230703) 
CH2b_Rod <- lmBF(diameter_3d ~ log_Rod_EDI + age + id + sex, 
                           data = Fielddata, 
                           whichRandom = c("id","sex"),
                           progress = FALSE, 
                           posterior = TRUE,
                           iterations = 10000
)

#illumiance
set.seed(20230703) 
CH2b_illum<- lmBF(diameter_3d ~ log_phot_lux + age + id + sex, 
                           data = Fielddata, 
                           whichRandom = c("id","sex"),
                           progress = FALSE, 
                           posterior = TRUE,
                           iterations = 10000
)

### testing the alpha-opic models and illuminance against each other----------------------------

#redefine names of the models and put them into list
all_models <- list(
  mEDI = CH2b_mEDI,
  Scone = CH2b_Scone,
  Mcone = CH2b_Mcone,
  Lcone = CH2b_Lcone,
  Rod = CH2b_Rod,
  Illuminance = CH2b_illum
)
#dynamic number of models variable (=6)
num_models <- length(all_models)

# Initialize an empty matrix to store log(BF) results
log_bf_matrix <- matrix(NA, nrow = num_models, ncol = num_models, 
                        dimnames = list(NULL, names(all_models)))

# Loop through all pairwise combinations
#use the extractBF function
for (i in 1:(num_models - 1)) {
  for (j in (i + 1):num_models) {
    set.seed(20230703)  # Set the seed for reproducibility in each comparison
    comparison <- compare(all_models[[i]], all_models[[j]])
    log_bf_value <- extractBF(comparison, logbf = T, onlybf = T) # Take the ln of Bayes factor value
    log_bf_matrix[i, j] <- log_bf_value  # Round the ln(BF) to 2 decimals
    set.seed(20230703)
    comparison2 <- compare(all_models[[j]], all_models[[i]])
    log_bf_value2 <- extractBF(comparison2, logbf = T, onlybf = T) # Take the ln of Bayes factor value
    log_bf_matrix[j, i] <- log_bf_value2  # Filling the lower triangle with the negation
  }
}


# Set row and column names
rownames(log_bf_matrix) <- names(all_models)
colnames(log_bf_matrix) <- names(all_models)

#convert matrix from ln to log10
log10_bf_matrix <- log_bf_matrix/log(10)


# Create a data frame from the matrix
log10_bf_matrix_df <- as.data.frame(log10_bf_matrix)



# Save the data frame as a CSV file
write.csv(log10_bf_matrix_df, "./06_output/stat/log10_bf_matrix.csv",
          row.names = TRUE)




# validate testing whether the alpha-opic model comp. loop worked----------------
set.seed(20230703) 
SCone_mEDI<- compare(CH2b_Scone, CH2b_mEDI)
#ln BF
extractBF(SCone_mEDI, logbf = T, onlybf = T)
#log10 BF
extractBF(SCone_mEDI, logbf = T, onlybf = T)/log(10)


set.seed(20230703) 
illum_LCone<- compare(CH2b_illum, CH2b_Lcone)
#ln BF
extractBF(illum_LCone, logbf = T, onlybf = T)
#log10 BF
extractBF(illum_LCone, logbf = T, onlybf = T)/log(10)


# single test results validate the matrix


### testing pairwise combinations of alpha-opic models against each other---------------


#use the different alpha-opic models defined from above 


# Create all combinations of models

#define list of alpha-opic values to make combinations with
vars <- c("log_Mel_EDI", "log_SCone_EDI", "log_MCone_EDI", "log_LCone_EDI",
          "log_Rod_EDI", "log_phot_lux")


# Function to create lmBF model for a pair of variables
create_model <- function(var1, var2) {
  set.seed(20230703)  # Set the seed for replicability
  formula <- as.formula(paste("diameter_3d ~", var1, "+", var2, "+ age + id + sex"))
  
  model <- lmBF(formula,
                data = Fielddata,
                whichRandom = c("id", "sex"),
                progress = FALSE,
                posterior = TRUE,
                iterations = 10000)
  
  return(model)
}


# Generate all combinations of length 2 from the list of variables
#this amounts to 15 models with each a pair of alpha-opic vars included.
combinations <- combn(vars, 2, simplify = TRUE)

# Create models for each combination
all_models <- list()
for (i in 1:ncol(combinations)) {
  model_name <- paste("CH2b_", gsub("log_", "", combinations[1, i]),
                      "_", gsub("log_", "", combinations[2, i]), sep = "")
  all_models[[model_name]] <- create_model(combinations[1, i], combinations[2, i])
}#set seed is part of the function used (see above)


# Redefine names of the models and put them into a list
all_models_list <- as.list(all_models)

# Initialize an empty matrix to store log(BF) results
num_models <- length(all_models_list)
log_bf_matrix_combi <- matrix(NA, nrow = num_models, ncol = num_models, 
                              dimnames = list(NULL, names(all_models_list)))

# Loop through all pairwise combinations
#use the extractBF function
for (i in 1:(num_models - 1)) {
  for (j in (i + 1):num_models) {
    set.seed(20230703)  # Set the seed for reproducibility in each comparison
    comparison <- compare(all_models_list[[i]], all_models_list[[j]])
    log_bf_value <- extractBF(comparison, logbf = T, onlybf = T)  # Take the base 10 logarithm of Bayes factor value
    log_bf_matrix_combi[i, j] <- log_bf_value 
    set.seed(20230703)  # Set the seed for reproducibility in each comparison
    comparison2 <- compare(all_models_list[[j]], all_models_list[[i]])
    log_bf_value2 <- extractBF(comparison2, logbf = T, onlybf = T) 
    log_bf_matrix_combi[j, i] <- log_bf_value2
  }
}

# Set row and column names
rownames(log_bf_matrix_combi) <- names(all_models_list)
colnames(log_bf_matrix_combi) <- names(all_models_list)

#convert log(BF)s into log10(BF)s
log10_bf_matrix_combi <- log_bf_matrix_combi/log(10)


# Save the matrix (transformed into dataframe) as a CSV file
write.csv(as.data.frame(log10_bf_matrix_combi),
          "./06_output/stat/log10_bf_matrix_combi.csv", row.names = TRUE)


# validate testing whether the alpha-opic combi model comp. code worked----------------

#define some pairwise combi models

# mEDI + Scone
set.seed(20230703) 
CH2b_mEDI_Scone <- lmBF(diameter_3d ~  log_SCone_EDI + log_Mel_EDI + age + id + sex, 
                        data = Fielddata, 
                        whichRandom = c("id","sex"),
                        progress = FALSE, 
                        posterior = TRUE,
                        iterations = 10000
)


# mEDI + Scone
set.seed(20230703) 
CH2b_mEDI_Rod <- lmBF(diameter_3d ~  log_Mel_EDI + log_Rod_EDI + age + id + sex, 
                      data = Fielddata, 
                      whichRandom = c("id","sex"),
                      progress = FALSE, 
                      posterior = TRUE,
                      iterations = 10000
)


set.seed(20230703) 
CH2b_Scone_Rod <- lmBF(diameter_3d ~  log_SCone_EDI + log_Rod_EDI + age + id + sex, 
                       data = Fielddata, 
                       whichRandom = c("id","sex"),
                       progress = FALSE, 
                       posterior = TRUE,
                       iterations = 10000
)


#validate test the combi models

# Compare Scone vs. mEDI + Scone

set.seed(20230703) 
Scone_mEDIScone<- compare( CH2b_Scone, CH2b_mEDI_Scone )
Scone_mEDIScone

set.seed(20230703) 
mEDIRod_Scone<- compare(CH2b_mEDI_Rod, CH2b_Scone)
extractBF(mEDIRod_Scone, logbf = T, onlybf = T)
extractBF(mEDIRod_Scone, logbf = T, onlybf = T)/log(10)



set.seed(20230703) 
mEDIRod_MEDI <- compare( CH2b_mEDI_Rod, CH2b_mEDI)

# now test combi models vs. combi models (just like in matrix)

set.seed(20230703) 
mEDIRod_SconeRod<- compare( CH2b_mEDI_Rod, CH2b_Scone_Rod)#
mEDIRod_SconeRod
#ln BF
extractBF(mEDIRod_SconeRod, logbf = T, onlybf = T)
#log10 BF
extractBF(mEDIRod_SconeRod, logbf = T, onlybf = T)/log(10)



set.seed(20230703) 
SconeRod_mEDIScone <- compare(CH2b_Scone_Rod, CH2b_mEDI_Scone )
SconeRod_mEDIScone
extractBF(SconeRod_mEDIScone, logbf = T, onlybf = T)
extractBF(SconeRod_mEDIScone, logbf = T, onlybf = T)/log(10)
