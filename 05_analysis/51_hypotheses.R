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
BF_CH1a
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
#result very similar to the BF10 computed with BayesFactor package.

#save BICs & BF10 approximation to text file
sink(file = "06_output/stat/CH2_field_BIC.txt")

paste0("H0:", formula(CH2_NHST_H0), collapse = " ")
paste0("H1:", formula(CH2_NHST_H1), collapse = " ")

BIC_CH2

paste0("from BICs approximated BF10:", bic_bf10(CH2_H0_bic,CH2_H1_bic), collapse = " ")

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
sink()

# strong evidence for null model 


